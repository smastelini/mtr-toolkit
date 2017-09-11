MTRT <- new.env()

# Normalize data in the [0,1] range
MTRT$normalize <- function(data) {
  invisible(data[, names(data) := lapply(.SD, as.numeric)])

  maxs <- as.numeric(data[, lapply(.SD, max)])
  mins <- as.numeric(data[, lapply(.SD, min)])

  data <- as.data.table(scale(data, center = mins, scale = maxs - mins))
  return(data)
}

# Get node's mean target values
MTRT$prototype <- function(Y) {
  unlist(Y[, lapply(.SD, mean)])
}

MTRT$variance <- function(Y) {
  sum(Y[, lapply(.SD, var)])
}

MTRT$homogeneity <- function(Y) {
  c.mean <- MTRT$prototype(Y)
  sum(Y[, sum((.SD - c.mean)^2)])
}

# Builds a MTRT model
MTRT$train <- function(X, Y, ftest.signf = 0.05, min.size = 5, max.depth = Inf) {
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

      var.p1 <- max(MTRT$variance(Y[part]), 0, na.rm = T)
      var.p2 <- max(MTRT$variance(Y[!part]), 0, na.rm = T)

      # Heuristic calculation
      h <- actual.var - (nrow(Y[part])/nrow(Y)*var.p1 + nrow(Y[!part])/nrow(Y)*var.p2)

      if(h > MTRT$best.h) {
        MTRT$best.h <- h
        MTRT$best.s <- split.p
      }
    })

    # Perform F Test once
    part <- attr <= MTRT$best.s
    p1.ss <- MTRT$homogeneity(Y[part])
    p2.ss <- MTRT$homogeneity(Y[!part])

    # F-test to decide whether attr best split is significantly better
    f.test <- (actual.ss/(nrow(Y)-1))/((p1.ss+p2.ss)/(nrow(Y)-2))
    if(f.test > ftest.signf)
      return.l <- list(split = MTRT$best.s, heur = MTRT$best.h)
    else
      return.l <- list(split = NA, heur = 0)

    return(return.l)
  }

  build <- function(X, Y, root = list(), level = 0) {
    # Naive stopping criterion
    if(nrow(Y) < min.size || level > max.depth) {
      root$descendants <- NULL
      l.pred <- unlist(Y[, lapply(.SD, mean)])
      l.factory <- function(l.mean) {
        force(l.mean)
        function() {
          return(l.mean)
        }
      }
      root$eval <- l.factory(l.pred)
      return(root)
    }
    
  	print(Y)
    
    this.var <- MTRT$variance(Y)
    this.ss <- MTRT$homogeneity(Y)
		
    browser()

    bests <- X[, lapply(.SD, function(attr, Y, acvar, acss) best.split(attr, Y, acvar, acss), Y = Y, acvar = this.var, acss = this.ss)]
    zabest <- which.max(unlist(bests[2]))

    # Second stopping criteria
    if(is.na(bests[1, zabest, with = F])) {      
      root$descendants <- NULL
      l.pred <- unlist(Y[, lapply(.SD, mean)])
      l.factory <- function(l.mean) {
        force(l.mean)
        function() {
          return(l.mean)
        }
      }
      root$eval <- l.factory(l.pred)
      return(root)
    }

    root$split.name <- names(bests)[zabest]
    root$split.index <- zabest
    root$split.val <- unlist(bests[1, zabest, with = F])

    n.factory <- function(threshold) {
      force(threshold)
      function(new) {
        # Returns the corresponding child's index
        as.numeric(new <= threshold) + 1
      }
    }

    root$eval <- n.factory(root$split.val)
    root$descendants <- list()
    # TODO categorical features
    length(root$descendants) <- 2

    # Induced data partition
    part <- unlist(X[, zabest, with = F]) <= root$split.val

    # Left node
    root$descendants[[1]] <- build(X[part], Y[part], level = level + 1)
    # Right node
    root$descendants[[2]] <- build(X[!part], Y[!part], level = level + 1)

    return(root)
  }

  tree <- build(X, Y)

  retr <- list(tree = tree, targets = names(Y))
  return(retr)
}

MTRT$predict <- function(mtrt, new.data) {
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

  predictions <- as.data.table(matrix(unlist(predictions), ncol = length(targets), byrow = TRUE))
  names(predictions) <- mtrt$targets
  return(predictions)
}