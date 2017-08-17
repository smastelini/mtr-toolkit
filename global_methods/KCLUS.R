KCLUS <- new.env()

# Loss function for clustering
KCLUS$loss.func <- function(a, b) {
  sqrt(sum((a-b)^2))
}

KCLUS$train <- function(X, Y, k = 2, max.depth = 6, var.improvp = 0.5, pred.type = "mean", min.cluss = NULL) {
  KCLUS$cidx <- 0
  KCLUS$centroids <- list()
  KCLUS$tree <- data.table(orig = character(0), dest = character(0))
  KCLUS$predictors <- list()

  if(is.null(min.cluss))
    min.cluss <- log2(nrow(X))

  clustree.b <- function(X, Y, level = 0, sup.id = 0, sup.var = Inf) {
    # Accounts the current inter cluster variance sum
    if(nrow(X) >= min.cluss) {
      current.svar <- sum(apply(X, 2, sd))
      # cat("Leaf instances: ", nrow(Y), "\tSV: ", sup.var, "\tAV: ", current.svar, "\tVI: ", (sup.var-current.svar), "\tVT: ", var.improvp*sup.var,"\n")
    }

    # Leaf node
    if(nrow(X) < min.cluss || level > max.depth || (sup.var >= current.svar && sup.var-current.svar < var.improvp*sup.var) || current.svar == 0) {
      KCLUS$tree <- rbindlist(list(KCLUS$tree, list(orig = sup.id, dest = NA)))
      switch(pred.type,
        # Mean prediction
    		mean={
    			KCLUS$predictors[[as.character(sup.id)]] <- as.numeric(Y[, lapply(.SD, mean)])
          NULL
    		},
        lr={
          KCLUS$predictors[[as.character(sup.id)]] <- lapply(seq(ncol(Y)), function(j, w, v) {
            z = v[[j]]
            form <- as.formula(colnames(w) ~ z)
            lm(w ~ z)
          }, w=as.matrix(X), v=Y)
          NULL
        }
      )
      return(NULL)
    }

    # Cluster's inner elements range
    maxmin <- rbindlist(list(X[, lapply(.SD, max)], X[, lapply(.SD, min)]))

    gen.centroids <- maxmin[, lapply(.SD, function(j,k) runif(k, max = j[1], min = j[2]), k = k)]

    actual.centers.idx <- rep(0, k)
    # Random Centers creation
    for(i in seq(k)) {
      # Updates the global centroid hash/node id counter
      KCLUS$cidx <- KCLUS$cidx + 1

      KCLUS$centroids[[as.character(KCLUS$cidx)]] <- unlist(gen.centroids[i])
      actual.centers.idx[i] <- KCLUS$cidx
    }

    # Group points within defined centers
    distances <- matrix(nrow=k, ncol=nrow(X))
    for(i in seq(k)) {
      distances[i,] <- apply(X, 1, KCLUS$loss.func, b = KCLUS$centroids[[as.character(actual.centers.idx[i])]])
    }

    clustered <- apply(distances, 2, which.min)

    for(i in seq(k)) {
      celements <- which(clustered == i)

      if(length(celements) == 0) {
        # Removes empty cluster
        KCLUS$centroids[[as.character(actual.centers.idx[i])]] <- NULL
        next
      }

      X.f <- X[celements]
      Y.f <- Y[celements]

      KCLUS$tree <- rbindlist(list(KCLUS$tree, list(orig = sup.id, dest = actual.centers.idx[i])))

      clustree.b(X.f, Y.f, level = level + 1, sup.id = actual.centers.idx[i], sup.var = current.svar)

    }
    return(NULL)
  }

  clustree.b(X, Y)

  retr <- list(tree = KCLUS$tree, centroids = KCLUS$centroids, predictors = KCLUS$predictors, targets = names(Y), pred.type = pred.type)

  rm(tree, centroids, predictors, cidx, envir = KCLUS)
  return(retr)
}

KCLUS$predict <- function(kclus, new.data) {
  predictions <- list()
  i <- 1
  apply(new.data, 1, function(dat, predictions) {
    actual <- "0"
    while(TRUE) {
      descendants <- as.character(kclus$tree[orig == actual, dest])

      if(length(descendants) == 1 && is.na(descendants)) {
        predictions[[i]] <<- switch(kclus$pred.type,
          # Mean prediction
      		mean={
      			kclus$predictors[[actual]]
      		},
          lr={
            sapply(kclus$predictors[[actual]], function(mod, dt) {
              predict(mod, dt)

            }, dt = dat)
          }
        )
        break
      }

      distances <- sapply(descendants, function(j) KCLUS$loss.func(dat, kclus$centroids[[j]]))
      cluster <- which.min(distances)
      actual <- as.character(descendants[cluster])
    }
    i <<- i + 1
  }, predictions = predictions)

  predictions <- as.data.table(matrix(unlist(predictions), ncol = length(targets), byrow = TRUE))
  names(predictions) <- kclus$targets
  return(predictions)
}
