dir.create(paste0(output.dir.st, "/prediction_logs/", tech), showWarnings = FALSE, recursive = TRUE)

targets <- list()
centers <- list()
scales <- list()

for(i in 1:length(bases)) {
	set.seed(exp.random.seeds[i])
	dataset <- read.csv(paste0(datasets.folder, "/", bases[i], ".csv"))
	dataset <- remove.unique(dataset)

	targets[[i]] <- colnames(dataset)[(ncol(dataset)-n.targets[i]+1):ncol(dataset)]

	dataset <- dataset[sample(nrow(dataset)),]
	sample.names <- rownames(dataset)

	#Center and Scaling
	dataset <- as.data.table(dataset)
	invisible(dataset[, names(dataset) := lapply(.SD, as.numeric)])

	norm.params <- get.normalization.params(dataset, norm.method)
	centers[[i]] <- norm.params[["center"]]
	scales[[i]] <- norm.params[["scale"]]

	dataset <- as.data.table(scale(dataset, center = centers[[i]], scale = scales[[i]]))

	len.fold <- round(nrow(dataset)/folds.num)

	######Use a testing set
	if(length(bases.test) > 0 && folds.num == 1) {
		dataset.test <- read.csv(paste0(datasets.folder, "/", bases.test[i], ".csv"))
		dataset.test <- as.data.table(dataset.test[, colnames(dataset)])
		invisible(dataset.test[, names(dataset.test) := lapply(.SD, as.numeric)])

		dataset.test <- as.data.table(scale(dataset.test, center = centers[[i]], scale = scales[[i]]))
		init.bound <- nrow(dataset) + 1

		dataset <- rbindlist(list(dataset, dataset.test))
		sample.names <- c(sample.names, rownames(dataset.test))
	}
	#######

	x <- dataset[, !targets[[i]], with = FALSE]
	y <- dataset[, targets[[i]], with = FALSE]

	cat(paste0(bases[i], "\n"))
	col.names.targets <- c()
	for(t in targets[[i]]) {
		col.names.targets <- c(col.names.targets, t)
		col.names.targets <- c(col.names.targets, paste0(t, ".pred"))
	}

	# Cross validation
	for(k in 1:folds.num) {
	  cat(paste0("Fold ", k, "\n"))

		if(folds.num == 1) {
			if(length(bases.test) > 0) {
				train.idx <- 1:(init.bound-1)
				test.idx <- init.bound:nrow(dataset)
			} else {
				test.idx <- as.numeric(rownames(dataset))
				train.idx <- test.idx
			}
		} else {
			test.idx <- ((k-1)*len.fold + 1):(ifelse(k==folds.num, nrow(dataset), k*len.fold))
			train.idx <- setdiff(1:nrow(dataset), test.idx)
		}

		x.train <- remove.unique(x[train.idx])
		y.train <- y[train.idx]

		x.test <- x[test.idx, names(x.train), with = FALSE]
		y.test <- y[test.idx]

		prediction.log <- as.data.table(setNames(replicate(length(col.names.targets),numeric(nrow(x.test)), simplify = F),
													col.names.targets))

		for(t in targets[[i]]) {
			cat(paste0(t, "\n"))
			regressor <- train_(x.train, y.train[[t]], tech, targets[[i]])
			predictions <- predict_(regressor, x.test, tech, targets[[i]])

			prediction.log[[t]] <- y.test[[t]]
			prediction.log[[paste0(t, ".pred")]] <- predictions
		}
		write.csv(data.frame(id=sample.names[test.idx],prediction.log, check.names = F), paste0(output.dir.st, "/prediction_logs/",tech,"/predictions_ST_", bases[i], paste0("_fold", formatC(k, width=2, flag="0")), ".csv"), row.names = FALSE)
	}
}

#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.st, "/prediction_logs"))
i <<- 1

lapply(bases, function(b) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RRMSE.", targets[[i]]), paste0("RMSE.", targets[[i]]))
	performance.log <<- data.frame(algorithm=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
												simplify = F), names.perf.log)), stringsAsFactors = FALSE)

	folds.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
										simplify = F), names.perf.log), stringsAsFactors = FALSE)
	lapply(1:folds.num, function(k) {
		log <- read.csv(paste0(getwd(),"/", tech, "/predictions_ST_", b, paste0("_fold", formatC(k, width=2, flag="0")),".csv"), header=TRUE)
		folds.log[nrow(folds.log)+1, "aCC"] <<- aCC(log, targets[[i]])
		folds.log[nrow(folds.log), "ARE"] <<- ARE(log, targets[[i]])
		folds.log[nrow(folds.log), "MSE"] <<- MSE(log, targets[[i]])
		folds.log[nrow(folds.log), "aRMSE"] <<- aRMSE(log, targets[[i]])
		folds.log[nrow(folds.log), "aRRMSE"] <<- aRRMSE(log, targets[[i]])

		# targets
		for(t in targets[[i]]) {
			folds.log[nrow(folds.log), paste0("R2.", t)] <<- summary(lm(log[,t] ~ log[, paste0(t, ".pred")]))$r.squared

			r <- scales[[i]][t]*log[, t] + centers[[i]][t]
			p <- scales[[i]][t]*log[, paste0(t, ".pred")] + centers[[i]][t]

			folds.log[nrow(folds.log), paste0("RMSE.", t)] <<- RMSE(r, p)
			folds.log[nrow(folds.log), paste0("RRMSE.", t)] <<- RRMSE(r, p)
		}
	})
	performance.log[nrow(performance.log)+1, 1] <<- tech
	performance.log[nrow(performance.log), -1] <<- colMeans(folds.log)

	write.csv(performance.log, paste0("../performance_ST_", tech, "_", b, ".csv"), row.names = FALSE)
	i <<- i + 1
})
setwd(actual.folder)
