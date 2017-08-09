KCLUS <- new.env()

KCLUS$train <- function(X, Y, k = 3, max.depth = 6, init.var.prop = 0.05, pred.type = "mean") {
  # Loss function for clustering
  loss.func <- function(a, b) {
    sqrt(sum((a-b)^2))
  }

  KCLUS$cidx <- 0
  KCLUS$centroids <- list()
  KCLUS$tree <- data.table(orig = character(0), dest = character(0))
  KCLUS$predictors <- list()

  # Initial variance of data
  KCLUS$init.var <- sum(as.numeric(X[,lapply(.SD, var)]), na.rm = TRUE)

  clustree.b <- function(X, Y, level = 0, sup.id = 0) {
    # Accounts the current inter cluster variance sum
    current.svar <- Inf
    if(level > 0)
      current.svar <- sum(as.numeric(X[,lapply(.SD, var)]))

    # Leaf node
    if(level > max.depth || current.svar < init.var.prop*KCLUS$init.var) {
      KCLUS$tree <- rbindlist(list(KCLUS$tree, list(orig = sup.id, dest = NA)))
      switch(pred.type,
    		mean={
    			KCLUS$predictors[[as.character(sup.id)]] <- sapply(Y, mean)
          NULL
    		}
      )
      # KCLUS$cidx <- KCLUS$cidx + 1
      return(NULL)
    }

    # Cluster inner elements range
    maxs <- as.numeric(X[, lapply(.SD, max)])
    mins <- as.numeric(X[, lapply(.SD, min)])

    actual.centers.idx <- rep(0, k)
    # Random Centers creation
    for(i in 1:k) {
      # Updates the global centroid hash/node id counter
      KCLUS$cidx <- KCLUS$cidx + 1

      KCLUS$centroids[[as.character(KCLUS$cidx)]] <- sapply(1:length(maxs),
        function(i, maxz, minz) {
          runif(1, min = minz[i], max = maxz[i])
        }, maxz = maxs, minz = mins
      )
      actual.centers.idx[i] <- KCLUS$cidx
    }

    # Group points within defined centers
    distances <- matrix(nrow=k, ncol=nrow(X))
    for(i in 1:k) {
      distances[i,] <- apply(X, 1, loss.func, b = KCLUS$centroids[[as.character(actual.centers.idx[i])]])
    }

    clustered <- apply(distances, 2, which.min)

    for(i in 1:k) {
      celements <- which(clustered == i)

      if(length(celements) == 0) {
        # Removes empty cluster
        KCLUS$centroids[[as.character(actual.centers.idx[i])]] <- NULL
        next
      }

      X.f <- X[celements]
      Y.f <- Y[celements]

      KCLUS$tree <- rbindlist(list(KCLUS$tree, list(orig = sup.id, dest = actual.centers.idx[i])))

      if(length(celements) > 1)
        clustree.b(X.f, Y.f, level + 1, actual.centers.idx[i])
      else
        clustree.b(X.f, Y.f, max.depth + 1, actual.centers.idx[i])
    }
    return(NULL)
  }

  clustree.b(X,Y)

  retr <- list(tree = KCLUS$tree, centroids = KCLUS$centroids, predictors = KCLUS$predictors, targets = names(Y))

  rm(tree, centroids, predictors, cidx, init.var, envir = KCLUS)
  return(retr)
}

KCLUS$predict <- function(kclus, new.data) {
  # Loss function for clustering
  loss.func <- function(a, b) {
    sqrt(sum((a-b)^2))
  }

  predictions <- matrix(nrow=nrow(new.data), ncol = length(kclus$targets))
  colnames(predictions) <- kclus$targets

  i <- 1
  apply(new.data, 1, function(dat) {
    actual <- "0"
    while(TRUE) {
      descendants <- as.character(kclus$tree[orig == actual, dest])
      if(length(descendants) == 1 && is.na(descendants)) {
        predictions[i,] <<- kclus$predictors[[actual]]
        break
      }

      distances <- sapply(descendants, function(j) loss.func(new.data[i,], kclus$centroids[[j]]))
      cluster <- which.min(distances)
      actual <- as.character(descendants[cluster])
    }
    i <<- i + 1
  })

  return(predictions)
}
