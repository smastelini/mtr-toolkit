# Rcpp::sourceCpp("Rcpp_resources.cpp")

# Normalize data in the [0,1] range
normalize <- function(data) {
	invisible(data[, names(data) := lapply(.SD, as.numeric)])

	maxs <- as.numeric(data[, lapply(.SD, max)])
	mins <- as.numeric(data[, lapply(.SD, min)])

	data <- as.data.table(scale(data, center = mins, scale = maxs - mins))
	return(data)
}

predict <- function(model, new.data, parallel = FALSE) {
	switch(model$type,
		MTRT = {
			predictMTRT(model, new.data)
		},
		KCRT = {
			predictKCRT(model, new.data)
		},
		MORF = {
			predictions <- list()
			length(predictions) <- length(model$forest)

			if(!parallel) {
				predictions <- lapply(model$forest, function(tree) {
					predictMTRT(tree, new.data)
				})
			} else {
				# Parallel predictions
				n.cores <- detectCores()
				cl <- makeCluster(n.cores, type="SOCK")

				clusterExport(cl, varlist=c("new.data"), envir = environment())

				predictions <- parLapply(cl, model$forest, function(tree) {
					require(data.table)
					predictMTRT(model$forest[[tr]], new.data)
				})
				stopCluster(cl)
			}
			as.data.table(apply(simplify2array(lapply(predictions, as.matrix)), 1:2, mean, na.rm = TRUE))
		},
		KCRTRF = {
			predictions <- list()
			length(predictions) <- length(model$forest)

			if(!parallel) {
				predictions <- lapply(model$forest, function(tree) {
					predictKCRT(tree, new.data)
				})
			} else {
				# Parallel predictions
				n.cores <- detectCores()
				cl <- makeCluster(n.cores, type="SOCK")

				clusterExport(cl, varlist=c("new.data"), envir = environment())

				predictions <- parLapply(cl, model$forest, function(tree) {
					require(data.table)
					predictKCRT(model$forest[[tr]], new.data)
				})
				stopCluster(cl)
			}
			as.data.table(apply(simplify2array(lapply(predictions, as.matrix)), 1:2, mean, na.rm = TRUE))
		}
	)
}
