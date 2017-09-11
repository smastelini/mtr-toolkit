KCLUS <- new.env()

# Loss function for clustering
KCLUS$loss.func <- function(a, b) {
	sqrt(sum((a-b)^2))
}

KCLUS$train <- function(X, Y, k = 2, max.depth = Inf, std.improvp = 0.01, min.cluss = NULL) {
	if(is.null(min.cluss))
		min.cluss <- log2(nrow(X))

	clustree.b <- function(X, Y, root = list(), level = 0, sup.std = Inf) {
		# Accounts the current inter cluster std sum
		if(nrow(X) >= min.cluss) {
			current.std <- unlist(X[, lapply(.SD, sd)])
		}

		# Leaf node
		if(nrow(X) < min.cluss || level > max.depth || (sup.std >= current.std && sup.std-current.std < std.improvp*sup.std) || current.std == 0) {
			root$descendants <- NULL
			l.pred <- unlist(Y[, lapply(.SD, mean)])
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
		for(i in seq(k)) {
			distances[i,] <- apply(X, 1, KCLUS$loss.func, b = unlist(gen.centroids[i]))
		}
		clustered <- apply(distances, 2, which.min)

		# Get non-empty clusters
		successful.c <- as.numeric(names(table(clustered)))
		# ... and centroids
		f.centroids <- lapply(successful.c, function(i, gen) unlist(gen[i]), gen = gen.centroids)

		# Function factory
		factory <- function(centers) {
			force(centers)
			function(new) {
				distances <- sapply(centers, function(c, nw) KCLUS$loss.func(nw, c), nw = new)
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
			Y.f <- Y[celements]

			root$descendants[[i]] <- clustree.b(X.f, Y.f, list(), level + 1, current.std)
		}
		return(root)
	}

	root <- clustree.b(X, Y)

	retr <- list(tree = root, targets = names(Y))
	return(retr)
}

KCLUS$predict <- function(kclus, new.data) {
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
