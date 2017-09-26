#' Scale the passed data within the range [0,1]. Scaled data is required for use Cluster-based prediction trees.
#'
#' @param data The data to be scaled
#' @return A data.table containing the scaled data
#' @export
scaleData <- function(data) {
	invisible(data[, names(data) := lapply(.SD, as.numeric)])

	maxs <- as.numeric(data[, lapply(.SD, max)])
	mins <- as.numeric(data[, lapply(.SD, min)])

	data <- as.data.table(scale(data, center = mins, scale = maxs - mins))
	return(data)
}

#' Prediction function for Cluster-based Tree models.
#'
#' @param model A MTRT, k-RCRT, MORF or k-RCRTRF model
#' @param new.data New instances to be predicted
#' @return parallel A boolean indicating whether to predict data in parallel (Only applicable for ensemble models).
#' @export
predict <- function(model, new.data, parallel = FALSE) {
	switch(model$type,
		MTRT = {
			predictMTRT(model, new.data)
		},
		KRCRT = {
			predictKRCRT(model, new.data)
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
				n.cores <- parallel::detectCores()
				cl <- parallel::makeCluster(n.cores, type="FORK")

				predictions <- parallel::parLapply(cl, model$forest, function(tree) {
					predictMTRT(tree, new.data)
				})
				parallel::stopCluster(cl)
			}
			backup <- predictions
			retr <- as.data.table(Reduce("+", lapply(predictions, as.matrix))/length(predictions))
			rm(backup)
			gc()
			retr
		},
		KRCRTRF = {
			predictions <- list()
			length(predictions) <- length(model$forest)

			if(!parallel) {
				predictions <- lapply(model$forest, function(tree) {
					predictKRCRT(tree, new.data)
				})
			} else {
				# Parallel predictions
				n.cores <- parallel::detectCores()
				cl <- parallel::makeCluster(n.cores, type="FORK")

				predictions <- parallel::parLapply(cl, model$forest, function(tree) {
					predictKRCRT(tree, new.data)
				})
				parallel::stopCluster(cl)
			}
			as.data.table(Reduce("+", lapply(predictions, as.matrix))/length(predictions))
		}
	)
}
