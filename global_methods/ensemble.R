MORF <- function(x, y, n.trees = 100, mtry = NULL, ftest.signf = 0.05, min.size = 5, max.depth = Inf) {
	if(is.null(mtry))
		mtry <- max(floor(log2(ncol(x) + 1)), 1)

	forest <- list()
	length(forest) <- n.trees
	for(tr in seq(n.trees)) {
		idxs <- sample(nrow(x.train), replace = TRUE)
		sampled.cols <- sample(ncol(x), mtry)

		x.bootstrap <- x[idxs, sampled.cols, with = FALSE]
		y.bootstrap <- y[idxs]


		forest[[tr]] <- MTRT(x.bootstrap, y.bootstrap, ftest.signf, min.size, max.depth)
	}

	list(forest = forest, type = "MORF")
}

parMORF <- function(x, y, n.trees = 100, mtry = NULL, ftest.signf = 0.05, min.size = 5, max.depth = Inf) {
	if(is.null(mtry))
		mtry <- max(floor(log2(ncol(x) + 1)), 1)

	# Parallel Tree building
	n.cores <- detectCores()
	cl <- makeCluster(n.cores, type="SOCK")

	clusterExport(cl, varlist=c("x", "y", "mtry", "ftest.signf", "min.size", "max.depth"))

	forest <- parLapply(cl, seq(n.trees), function(tr) {
		require(matrixStats)
		require(data.table)

		idxs <- sample(nrow(x), replace = TRUE)
		sampled.cols <- sample(ncol(x), mtry)

		x.bootstrap <- x[idxs, sampled.cols, with = FALSE]
		y.bootstrap <- y[idxs]


		MTRT(x.bootstrap, y.bootstrap, ftest.signf, min.size, max.depth)
	})
	stopCluster(cl)

	list(forest = forest, type = "MORF")
}

KCRTRF <- function(x, y, n.trees = 100, mtry = NULL, k = 2, max.depth = Inf, var.improvp = 0.01, min.kcrts = NULL) {
	forest <- list()
	length(forest) <- n.trees
	for(tr in seq(n.trees)) {
		idxs <- sample(nrow(x.train), replace = TRUE)
		sampled.cols <- sample(ncol(x), mtry)

		x.bootstrap <- x[idxs, sampled.cols, with = FALSE]
		y.bootstrap <- y[idxs]


		forest[[tr]] <- KCRT(x.bootstrap, y.bootstrap, k, max.depth, var.improvp, min.kcrts)
	}

	list(forest = forest, type = "KCRTRF")
}

parKCRTRF <- function(x, y, n.trees = 100, mtry = NULL, k = 2, max.depth = Inf, var.improvp = 0.01, min.kcrts = NULL) {
	if(is.null(mtry))
		mtry <- max(floor(log2(ncol(x) + 1)), 1)

	# Parallel Tree building
	n.cores <- detectCores()
	cl <- makeCluster(n.cores, type="SOCK")

	clusterExport(cl, varlist=c("x", "y", "mtry", "k", "max.depth", "var.improvp", "min.kcrts"))

	forest <- parLapply(cl, seq(n.trees), function(tr) {
		require(data.table)

		idxs <- sample(nrow(x), replace = TRUE)
		sampled.cols <- sample(ncol(x), mtry)

		x.bootstrap <- x[idxs, sampled.cols, with = FALSE]
		y.bootstrap <- y[idxs]


		KCRT(x.bootstrap, y.bootstrap, k, max.depth, var.improvp, min.kcrts)
	})

	stopCluster(cl)
	
	list(forest = forest, type = "KCRTRF")
}