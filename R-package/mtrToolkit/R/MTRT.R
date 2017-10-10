#' Creates Multi-target Regression Trees (MTRT), as in CLUS
#'
#' @param X, Y The input features and target variables respectively
#' @param ftest.signf The signficance level for F-test's stopping criteria (Default = 0.05)
#' @param min.size Minimum size of generated clusteres (Default = 5, as in CLUS)
#' @param max.depth Maximum depth for generated trees (Default = Inf, split are made while it is possible)
#' @return A MTRT model
#' @export
MTRT <- function(X, Y, ftest.signf = 0.05, min.size = 5, max.depth = Inf) {
  build.MTRT <- function(X, Y, root = new.env(), level = 0) {

    # Naive stopping criterion
    if(nrow(X) <= min.size || level > max.depth) {
      root$descendants <- NULL
      if(is.null(nrow(Y)))
        l.pred <- Y
      else
        l.pred <- prototype(Y)
      l.factory <- function(l.mean) {
        force(l.mean)
        function() {
          return(l.mean)
        }
      }
      root$eval <- l.factory(l.pred)
      rm(X,Y)
      return(root)
    }

    this.var <- variance(Y)
    this.ss <- homogeneity(Y)

    bests <- X[, lapply(.SD, function(attr, Y, acvar, acss) best_split(attr, Y, acvar, acss), Y = Y, acvar = this.var, acss = this.ss)]

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
      rm(X,Y,bests)
      return(root)
    }

    zabest <- which.max(unlist(bests[2], use.names = F))
    root$split.name <- names(bests)[zabest]
    root$split.val <- unlist(bests[1, zabest, with = F], use.names = F)

    browser()

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

    X.part1 <- X[part]
    Y.part1 <- Y[part,]

    X.part2 <- X[!part]
    Y.part2 <- Y[!part,]

    # Avoid memory waste
    rm(X,Y,bests)

    # Left node
    root$descendants[[1]] <- build.MTRT(X.part1, Y.part1, level = level + 1)
    # Right node
    root$descendants[[2]] <- build.MTRT(X.part2, Y.part2, level = level + 1)
		
    rm(X.part1,X.part2,Y.part1,Y.part2)
    return(root)
  }
  
  Y <- as.matrix(Y)
  tree <- build.MTRT(X, Y)

  retr <- list(tree = tree, targets = colnames(Y), type = "MTRT")
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
        next.n <- root$eval(dat[root$split.name])
        root <- root$descendants[[next.n]]
      }
    }
    i <<- i + 1
  }, predictions = predictions)

  backup <- predictions
  predictions <- as.data.table(matrix(unlist(predictions, use.names = F), ncol = length(mtrt$targets), byrow = TRUE))
  names(predictions) <- mtrt$targets
  # Make some memory free
  rm(backup)
  return(predictions)
}
