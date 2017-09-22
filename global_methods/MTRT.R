# TODO deal with factors && Try to improve performance
best.split <- function(attr, Y, actual.var, actual.ss) {
  to.eval <- unique(sort(attr))

  # None split could brings some improvement => Homogeneous data
  if(length(to.eval) == 1)
    return(list(split = NA, heur = 0))
  mtrt <- new.env()

  mtrt$best.h <- 0
  mtrt$best.s <- NA

  lapply(to.eval[-length(to.eval)], function(split.p) {
    part <- attr <= split.p

    var.p1 <- max(variance(Y[part]), 0, na.rm = TRUE)
    var.p2 <- max(variance(Y[!part]), 0, na.rm = TRUE)

    # Heuristic calculation
    h <- actual.var - (length(which(part))/nrow(Y)*var.p1 + length(which(!part))/nrow(Y)*var.p2)

    if(h > mtrt$best.h) {
      mtrt$best.h <- h
      mtrt$best.s <- split.p
    }
  })

  best.h <- mtrt$best.h
  best.s <- mtrt$best.s

  rm(mtrt)

  # Gains weren't observed
  if(best.h == 0)
    return(list(split = NA, heur = 0))

  # Perform F Test once
  part <- attr <= best.s

  sum.ss <- homogeneity(Y[part]) + homogeneity(Y[!part])
  f.test <- (nrow(Y)-2)*(actual.ss-sum.ss)/sum.ss

  # It have passed the F-test
  if(f.test > qf(1 - ftest.signf, 1, nrow(Y)-2))
    return(list(split = best.s, heur = best.h))
  else # It didn't make through this
    return(list(split = NA, heur = 0))
}

# Builds a MTRT model
MTRT <- function(X, Y, ftest.signf = 0.05, min.size = 5, max.depth = Inf) {
  build.MTRT <- function(X, Y, root = list(), level = 0) {
    # Naive stopping criterion
    if(nrow(Y) < min.size || level > max.depth) {
      root$descendants <- NULL
      l.pred <- prototype(Y)
      l.factory <- function(l.mean) {
        force(l.mean)
        function() {
          return(l.mean)
        }
      }
      root$eval <- l.factory(l.pred)
      return(root)
    }

    this.var <- variance(Y)
    this.ss <- homogeneity(Y)

    bests <- X[, lapply(.SD, function(attr, Y, acvar, acss) best.split(attr, Y, acvar, acss), Y = Y, acvar = this.var, acss = this.ss)]

    # Second stopping criteria
    if(all(is.na(bests[1]))) {
      root$descendants <- NULL
      l.pred <- prototype(Y)
      l.factory <- function(l.mean) {
        force(l.mean)
        function() {
          return(l.mean)
        }
      }
      root$eval <- l.factory(l.pred)
      return(root)
    }

    zabest <- which.max(unlist(bests[2]))
    root$split.name <- names(bests)[zabest]
    root$split.index <- zabest
    root$split.val <- unlist(bests[1, zabest, with = F])

    n.factory <- function(threshold) {
      force(threshold)
      function(new) {
        # Returns the corresponding child's index
        as.numeric(new > threshold) + 1
      }
    }

    root$eval <- n.factory(root$split.val)
    root$descendants <- list()
    # TODO categorical features
    length(root$descendants) <- 2

    # Induced data partition
    part <- X[[zabest]] <= root$split.val

    # Left node
    root$descendants[[1]] <- build(X[part], Y[part], level = level + 1)
    # Right node
    root$descendants[[2]] <- build(X[!part], Y[!part], level = level + 1)

    return(root)
  }

  
  tree <- build.MTRT(X, Y)

  retr <- list(tree = tree, targets = names(Y), type = "MTRT")
  return(retr)
}

predictMTRT <- function(mtrt, new.data) {
  predictions <- list()
  length(predictions) <- nrow(new.data)

  i <- 1
  apply(new.data, 1, function(dat, predictions) {
    root <- mtrt$tree

    while(TRUE) {
      if(length(root$descendants) == 0) {
        predictions[[i]] <<- root$eval()
        break
      } else {
        next.n <- root$eval(dat[root$split.index])
        root <- root$descendants[[next.n]]
      }
    }
    i <<- i + 1
  }, predictions = predictions)

  predictions <- as.data.table(matrix(unlist(predictions), ncol = length(mtrt$targets), byrow = TRUE))
  names(predictions) <- mtrt$targets
  return(predictions)
}
