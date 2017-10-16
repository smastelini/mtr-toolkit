#' Creates Multi-target Regression Trees (MTRT), as in CLUS
#'
#' @param X, Y The input features and target variables respectively
#' @param ftest.signf The signficance level for F-test's stopping criteria (Default = 0.05)
#' @param min.size Minimum size of generated clusteres (Default = 5, as in CLUS)
#' @param max.depth Maximum depth for generated trees (Default = Inf, split are made while it is possible)
#' @return A MTRT model
#' @export
MTRT <- function(X, Y, ftest.signf = 0.05, min.size = 5, max.depth = Inf) {
	nodes <- new.env()
	# Nodes
	nodes$tovisit <- list(NULL)
	# All instances should be evaluated at first
	nodes$tovisit[[1]] <- seq(nrow(X))
	# Nodes ids to linking the tree's nodes
	nodes$ids <- list(NULL)
	# The first id is the root node
	nodes$ids[[1]] <- 1

	# Structure to keep the tree hierarchy
	# Parent, branch = {1:left, 2:right}, level
	nodes$tree <- data.table(N1 = c(NA,NA,1))

	# Structure to save the created nodes and leaves
	nodes$elem <- list()

	# Aux variables to efficiently add elements to the 'tovisit' and 'id' queues
	nodes$counter <- 1
	nodes$size <- 1

	addNode2Visit <- function(item, id, parent, pos, level) {
		if(nodes$counter == nodes$size) {
			length(nodes$tovisit) <- length(nodes$ids) <- nodes$size <- 2 * nodes$size
		}

		if(nodes$size == 0)
			nodes$size <- 1

		nodes$counter <- nodes$counter + 1

		nodes$tovisit[[nodes$counter]] <- item
		nodes$ids[[nodes$counter]] <- id

		# Save tree hierarchy
		nodes$tree[, paste0("N", id) := c(parent, pos, level)]

		NULL
	}

	getNode2Visit <- function() {
		idx <- nodes$tovisit[[1]]
		nodes$tovisit[[1]] <- NULL
		this <- nodes$ids[[1]]
		nodes$ids[[1]] <- NULL

		nodes$size <- nodes$size - 1
		nodes$counter <- nodes$counter - 1
		return(list(idx = idx, this = this))
	}

	getNodeInfo <- function(id) {
		nodes$tree[[paste0("N", id)]]
	}

	thereAreNodes2Visit <- function() {
		nodes$counter > 0
	}

	link2Parent <- function(node, parent.id, branch) {
		nodes$parent <- nodes$elem[[as.character(parent.id)]]
		nodes$parent$descendants[[branch]] <- node
		NULL
	}

	n.factory <- function(split) {
		local({
			default <- split;
			function(new, threshold = default) {
				# Returns the corresponding child's index
				as.numeric(new > threshold) + 1
			}
		})
	}

	l.factory <- function(l.mean) {
		local({
			default <- l.mean;
			function(leaf.mean = default) {
				return(leaf.mean)
			}
		})
	}

	prototypeR <- function(Y) {
		colMeans(Y)
	}

	varianceR <- function(Y) {
	  # sum(Y[, lapply(.SD, var)])
	  sum(apply(Y, 2, var))
	}

	homogeneityR <- function(Y) {
	  c.mean <- prototypeR(Y)
	  sum(colSums(sweep(Y, 2, c.mean)^2))
	}

	best.splitR <- function(attr, Y, actual.var, actual.ss) {
		MTRT <- new.env()

    to.eval <- unique(sort(attr))

    # None split could brings some improvement => Homogeneous data
    if(length(to.eval) == 1)
      return(list(split = NA, heur = 0))

    MTRT$best.h <- 0
    MTRT$best.s <- NA

    sapply(to.eval[-length(to.eval)], function(split.p) {
      part <- attr <= split.p

      var.p1 <- max(MTRT$variance(Y[part]), 0, na.rm = TRUE)
      var.p2 <- max(MTRT$variance(Y[!part]), 0, na.rm = TRUE)

      # Heuristic calculation
      h <- actual.var - (length(which(part))/nrow(Y)*var.p1 + length(which(!part))/nrow(Y)*var.p2)

      if(h > MTRT$best.h) {
        MTRT$best.h <- h
        MTRT$best.s <- split.p
      }
    })

    # Gains weren't observed
    if(MTRT$best.h == 0)
      return(list(split = NA, heur = 0))

    # Perform F Test once
    part <- attr <= MTRT$best.s

    sum.ss <- MTRT$homogeneity(Y[part]) + MTRT$homogeneity(Y[!part])
		f.test <- (nrow(Y)-2)*(actual.ss-sum.ss)/sum.ss

		best.s <- MTRT$best.s
		best.h <- MTRT$best.h

		rm(MTRT)

    # It have passed the F-test
    if(f.test > qf(1 - ftest.signf, 1, nrow(Y)-2))
      return(list(split = best.s, heur = best.h))
    else # It didn't make through this
      return(list(split = NA, heur = 0))
	}

	build.MTRT.inc <- function() {
		node.id <- 2

		while(thereAreNodes2Visit()) {
			n2v <- getNode2Visit()
			idx <- n2v$idx
			this.id <- n2v$this

			# Retrieves node's information
			info <- getNodeInfo(this.id)
			parent.id <- info[[1]]
			branch <- info[[2]]
			this.level <- info[[3]]

			# Naive stopping criterion
			if(length(idx) <= min.size || this.level > max.depth) {
				nodes$n <- new.env()
				nodes$n$descendants <- NULL
				if(length(idx) == 1)
					l.pred <- unname(Y[idx,])
				else
					l.pred <- prototype(Y[idx,])

				nodes$n$eval <- l.factory(l.pred)
				# Saves node for posterior reference
				nodes$elem[[as.character(this.id)]] <- nodes$n

				if(this.id > 1)
					link2Parent(nodes$n, parent.id, branch)

				if(thereAreNodes2Visit())
					next
				else
					break
			}

			this.var <- variance(Y[idx,])
			this.ss <- homogeneity(Y[idx,])

			bests <- X[idx, lapply(.SD, function(attr, T, acvar, acss) best_split(attr, T, acvar, acss), T = Y[idx,], acvar = this.var, acss = this.ss)]

			# Second stopping criteria
			if(all(is.na(bests[1]))) {
				nodes$n <- new.env()
				nodes$n$descendants <- NULL
				l.pred <- prototype(Y[idx,])

				nodes$n$eval <- l.factory(l.pred)
				# Saves node for posterior reference
				nodes$elem[[as.character(this.id)]] <- nodes$n

				if(this.id > 1)
					link2Parent(nodes$n, parent.id, branch)

				rm(bests)
				if(thereAreNodes2Visit())
					next
				else
					break
			}

			best.s <- which.max(unlist(bests[2], use.names = F))

			nodes$n <- new.env()
			nodes$n$split.name <- names(bests)[best.s]
			nodes$n$split.val <- unlist(bests[1, best.s, with = F], use.names = F)

			nodes$n$eval <- n.factory(nodes$n$split.val)
			nodes$n$descendants <- list()
			# TODO categorical features
			length(nodes$n$descendants) <- 2

			if(this.id > 1)
				link2Parent(nodes$n, parent.id, branch)

			# Saves node for posterior reference
			nodes$elem[[as.character(this.id)]] <- nodes$n

			# Induced data partition
			part <- X[idx, best.s, with = FALSE] <= nodes$n$split.val

			addNode2Visit(idx[part], node.id, this.id, 1, this.level + 1)
			addNode2Visit(idx[!part], node.id + 1, this.id, 2, this.level + 1)

			node.id <- node.id + 2

			rm(bests)
		}

		root <- nodes$elem[["1"]]
		return(root)
	}

	Y <- as.matrix(Y)
	tree <- build.MTRT.inc()

	aux <- unlist(nodes, recursive = F)
	rm(nodes)
	rm(aux)

	retr <- list(tree = unlist(tree, recursive = F), targets = colnames(Y), type = "MTRT")
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
