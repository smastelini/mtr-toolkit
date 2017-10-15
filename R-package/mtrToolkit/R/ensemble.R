#' Creates a Random Forest ensemble of Multi-target Regression Trees -- Multi-output Random Forest (MORF) (as in CLUS).
#'
#' @param x, y The input features and target variables respectively.
#' @param n.trees The number of tree predictors (Default = 100).
#' @param mtry Feature subsample to build each tree. (Default = , as in CLUS).
#' @param ftest.signf The signficance level for F-test's stopping criteria (Default = 0.05).
#' @param min.size Minimum size of generated clusteres (Default = 5, as in CLUS).
#' @param max.depth Maximum depth for generated trees (Default = Inf, split are made while it is possible).
#' @param parallel Whether to build the forest in parallel, using all disponible cores (Default = FALSE).
#' @return A MORF model
#' @export
MORF <- function(x, y, n.trees = 100, mtry = NULL, ftest.signf = 0.05, min.size = 5, max.depth = Inf, parallel = FALSE) {
	if(is.null(mtry))
		mtry <- max(floor(log2(ncol(x) + 1)), 1)
	# Call garbage collector
	# gc()
	if(!parallel) {
		forest <- list()
		length(forest) <- n.trees
		for(tr in seq(n.trees)) {
			idxs <- sample(nrow(x), replace = TRUE)
			sampled.cols <- sample(ncol(x), mtry)

			x.bootstrap <- x[idxs, sampled.cols, with = FALSE]
			y.bootstrap <- y[idxs]

			forest[[tr]] <- MTRT(x.bootstrap, y.bootstrap, ftest.signf, min.size, max.depth)
			rm(x.bootstrap, y.bootstrap, idxs, sampled.cols)
		}
	} else {
		# Parallel Tree building
		n.cores <- parallel::detectCores()
		cl <- parallel::makeCluster(n.cores, type="FORK")

		forest <- parallel::parLapply(cl, seq(n.trees), function(tr) {
			idxs <- sample(nrow(x), replace = TRUE)
			sampled.cols <- sample(ncol(x), mtry)

			x.bootstrap <- x[idxs, sampled.cols, with = FALSE]
			y.bootstrap <- y[idxs]

			mtrt <- MTRT(x.bootstrap, y.bootstrap, ftest.signf, min.size, max.depth)
			rm(x.bootstrap, y.bootstrap, idxs, sampled.cols)
			mtrt
		})
		parallel::stopCluster(cl)
	}

	list(forest = forest, type = "MORF")
}


#' Creates a Random Forest ensemble of k-Random Clusters Regression Trees (k-RCRTRF).
#'
#' @param x, y The input features and target variables respectively
#' @param n.trees The number of tree predictors (Default = 100)
#' @param mtry Feature subsample to build each tree. (Default = , as in CLUS)
#' @param k The number of random clusters to be generated at each split (Default = 3)
#' @param max.depth Maximum depth for generated trees (Default = Inf, split will are made while it is possible)
#' @param var.improvp Minimum variance decrease percentual when comparing a child to its parent needed to continue splitting (Default = 0.01)
#' @param min.size Minimum size of generated clusteres (Default = 5, as in CLUS)
#' @param parallel Whether to build the forest in parallel, using all disponible cores (Default = FALSE).
#' @return A k-RCRTRF model
#' @export
KRCRTRF <- function(x, y, n.trees = 100, mtry = NULL, k = 3, max.depth = Inf, var.improvp = 0.01, min.size = NULL, parallel = FALSE) {
	if(is.null(mtry))
		mtry <- max(floor(log2(ncol(x) + 1)), 1)

	if(!parallel) {
		forest <- list()
		length(forest) <- n.trees
		for(tr in seq(n.trees)) {
			idxs <- sample(nrow(x), replace = TRUE)
			sampled.cols <- sample(ncol(x), mtry)

			x.bootstrap <- x[idxs, sampled.cols, with = FALSE]
			y.bootstrap <- y[idxs]


			forest[[tr]] <- KRCRT(x.bootstrap, y.bootstrap, k, max.depth, var.improvp, min.size)
		}
	} else {
		# Parallel Tree building
		n.cores <- parallel::detectCores()
		cl <- parallel::makeCluster(n.cores, type="FORK")

		forest <- parallel::parLapply(cl, seq(n.trees), function(tr) {
			idxs <- sample(nrow(x), replace = TRUE)
			sampled.cols <- sample(ncol(x), mtry)

			x.bootstrap <- x[idxs, sampled.cols, with = FALSE]
			y.bootstrap <- y[idxs]


			KRCRT(x.bootstrap, y.bootstrap, k, max.depth, var.improvp, min.size)
		})
		parallel::stopCluster(cl)
	}

	list(forest = forest, type = "KRCRTRF")
}
