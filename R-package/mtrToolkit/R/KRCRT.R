#' Creates a k-Random Clusters Regression Trees (k-RCRTRF) model.
#'
#' @param X, Y The input features and target variables respectively
#' @param k The number of random clusters to be generated at each split (Default = 3)
#' @param max.depth Maximum depth for generated trees (Default = Inf, split will are made while it is possible)
#' @param var.improvp Minimum variance decrease percentual when comparing a child to its parent needed to continue splitting (Default = 0.01)
#' @param min.size Minimum size of generated clusteres (Default = 5, as in CLUS)
#' @return A k-RCRT model
#' @export
KRCRT <- function(X, Y, k = 2, max.depth = Inf, var.improvp = 0.01, min.size = NULL) {
	krcrtree.b <- function(X, Y, root = list(), level = 0, sup.var = Inf) {
		# Accounts the current inter cluster std sum
		if(nrow(X) >= min.size) {
			current.var <- col_vars(Y)
		}

		# Leaf node
		if(nrow(X) < min.size || level > max.depth || (sup.var >= current.var && sup.var-current.var < var.improvp*sup.var) || current.var == 0) {
			root$descendants <- NULL
			l.pred <- prototype(Y)
			factory.l <- function(l.mean) {
				force(l.mean)
				function() {
					return(l.mean)
				}
			}

			root$eval <- factory.l(l.pred)
			return(root)
		}

		# Cluster's inner elements range
		maxmin <- rbindlist(list(X[, lapply(.SD, max)], X[, lapply(.SD, min)]))

		gen.centroids <- maxmin[, lapply(.SD, function(j,k) runif(k, max = j[1], min = j[2]), k = k)]

		# Group points within defined centers
		distances <- matrix(nrow=k, ncol=nrow(X))
		auxX <- as.matrix(X)
		for(i in seq(k)) {
			distances[i,] <- calcEuclideanDist(auxX, unlist(gen.centroids[i]))
		}

		#sapply?
		clustered <- apply(distances, 2, which.min)

		# Get non-empty clusters
		successful.c <- as.numeric(names(table(clustered)))
		# ... and centroids
		f.centroids <- lapply(successful.c, function(i, gen) unlist(gen[i]), gen = gen.centroids)

		# Function factory
		factory <- function(centers) {
			force(centers)
			function(new) {
				distances <- sapply(centers, function(c, nw) euclideanDist(nw, c), nw = new)
				return(which.min(distances))
			}
		}

		# Evaluation function
		root$eval <- factory(f.centroids)

		root$descendants <- list()
		length(root$descendants) <- length(successful.c)

		for(i in seq(successful.c)) {
			celements <- which(clustered == successful.c[i])

			X.f <- X[celements]
			Y.f <- Y[celements,]

			root$descendants[[i]] <- krcrtree.b(X.f, Y.f, list(), level + 1, current.var)
		}
		return(root)
	}

	if(is.null(min.size))
		min.size <- log2(nrow(X))

	Y <- as.matrix(Y)

	root <- krcrtree.b(X, Y)
	retr <- list(tree = root, targets = colnames(Y), type = "KRCRT")
	return(retr)
}

predictKRCRT <- function(kclus, new.data) {
	predictions <- list()
	length(predictions) <- nrow(new.data)

	i <- 1
	apply(new.data, 1, function(dat, predictions) {
		root <- kclus$tree
		while(TRUE) {
			if(length(root$descendants) == 0) {
				predictions[[i]] <<- root$eval()
				break
			} else {
				next.n <- root$eval(dat)
				root <- root$descendants[[next.n]]
			}
		}
		i <<- i + 1
	}, predictions = predictions)

	predictions <- as.data.table(matrix(unlist(predictions), ncol = length(targets), byrow = TRUE))
	names(predictions) <- kclus$targets
	return(predictions)
}
