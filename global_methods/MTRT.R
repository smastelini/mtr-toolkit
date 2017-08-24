MTRT <- new.env()

# Normalize data in the [0,1] range
MTRT$normalize <- function(data) {
  invisible(data[, names(data) := lapply(.SD, as.numeric)])

  maxs <- as.numeric(data[, lapply(.SD, max)])
  mins <- as.numeric(data[, lapply(.SD, min)])

  data <- as.data.table(scale(data, center = mins, scale = maxs - mins))
  return(data)
}

# Builds a MTRT model
MTRT$train <- function(X, Y, ftest.signf = 0.05, min.size = NULL, max.depth = Inf) {
  # Get node's mean target values
  prototype <- function(Y) {
    unlist(Y[, lapply(.SD, mean)])
  }

  variance <- function(Y) {
    sum(Y[, lapply(.SD, var)])
  }

  homogeneity <- function(Y) {
    c.mean <- prototype(Y)

    MTRT$aux <- rep(0, nrow(Y))
    MTRT$i <- 1
    apply(Y, 1, function(i) {
      MTRT$aux[MTRT$i] <- sum((i - c.mean)^2)
      MTRT$i <- MTRT$i + 1
    })
    return(sum(MTRT$aux))
  }

  # TODO deal with factors && Try to improve performance
  best.split <- function(attr, Y, actual.var, actual.ss) {
    to.eval <- unique(sort(attr))

    # None split could brings some improvement => Homogeneous data
    if(length(to.eval) == 1)
      return(list(split = NA, heur = 0))

    MTRT$best.h <- 0
    MTRT$best.s <- NA

    sapply(to.eval[-length(to.eval)], function(split.p) {
      part <- attr <= split.p

      var.p1 <- max(variance(Y[part]), 0, na.rm = T)
      var.p2 <- max(variance(Y[!part]), 0, na.rm = T)

      # Heuristic calculation
      h <- actual.var - (nrow(Y[part])/nrow(Y)*var.p1 + nrow(Y[!part])/nrow(Y)*var.p2)

      if(h > MTRT$best.h) {
        MTRT$best.h <- h
        MTRT$best.s <- split.p
      }
    })

    # Perform F Test once
    part <- attr <= MTRT$best.s
    p1.ss <- homogeneity(Y[part])
    p2.ss <- homogeneity(Y[!part])

    # F-test to decide whether attr best split is significantly better
    f.test <- (actual.ss/(nrow(Y)-1))/((p1.ss+p2.ss)/(nrow(Y)-2))
    if(f.test > ftest.signf)
      return.l <- list(split = MTRT$best.s, heur = MTRT$best.h)
    else
      return.l <- list(split = NA, heur = 0)

    return(return.l)
  }

  build <- function(X, Y, root = list()) {
    this.var <- variance(Y)
    this.ss <- homogeneity(Y)

    # TODO stopping criterion

    bests <- X[, lapply(.SD, function(attr, Y, acvar, acss) best.split(attr, Y, acvar, acss), Y = Y, acvar = this.var, acss = this.ss)]
    zabest <- which.max(unlist(bests[2]))

    root$split.name <- names(bests)[zabest]
    root$split.val <- bests[1, zabest, with = F]
    root$eval <- function(new, threshold = root$split.val) {
      # Returns the corresponding child's index
      as.numeric(new <= threshold) + 1
    }
    root$descendants <- list()

    # Induced data partition
    part <- X[, zabest, with = F] <= root$split.val

    # Left node
    root$descendants[[1]] <- build(X[part], Y[part])
    # Right node
    root$descendants[[2]] <- build(X[!part], Y[!part])

    return(root)
  }

  tree <- build(X, Y)

}
