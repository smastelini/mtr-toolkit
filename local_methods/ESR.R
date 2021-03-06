dir.create(paste0(output.dir.esr, "/prediction_logs/", tech), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.esr, "/raw_logs/", tech), showWarnings = FALSE, recursive = TRUE)

len.ensemble <- 10

targets <- list()
centers <- list()
scales <- list()

for(i in seq(length(bases))) {
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

		base.predictions.tr <- data.table(matrix(nrow=nrow(x.train), ncol=n.targets[i] * len.ensemble))
		base.predictions.ts <- data.table(matrix(nrow=nrow(x.test), ncol=n.targets[i] * len.ensemble))

		names.basep <- as.vector(outer(paste0("M", seq(len.ensemble)), targets[[i]], paste, sep="."))

		names(base.predictions.tr) <- names(base.predictions.ts) <- names.basep

		# Base predictors
		cat("Base predictions\n")
		for(t in targets[[i]]) {
			cat(paste0(t, "\n"))
			for(m in seq(len.ensemble)) {
				bootstrap.ids <- sample(nrow(x.train), replace=TRUE)
				x.bootstrap <- remove.unique(x.train[bootstrap.ids])
				regressor <- train_(x.bootstrap, y.train[bootstrap.ids, t, with=FALSE][[1]], tech, targets[[i]])
				set(base.predictions.tr, NULL, paste(paste0("M", m), t, sep="."), predict_(regressor, x.train[, names(x.bootstrap), with = FALSE], tech, targets[[i]]))
				set(base.predictions.ts, NULL, paste(paste0("M", m), t, sep="."), predict_(regressor, x.test[, names(x.bootstrap), with = FALSE], tech, targets[[i]]))
			}
		}

		write.csv(data.frame(id=sample.names[train.idx], base.predictions.tr, check.names = F), paste0(output.dir.esr, "/raw_logs/", tech,
			"/raw_ESR_tr_", bases[i], paste0("_fold", formatC(k, width=2, flag="0")), ".csv"), row.names = FALSE)
		write.csv(data.frame(id=sample.names[test.idx], base.predictions.ts, check.names = F), paste0(output.dir.esr, "/raw_logs/", tech,
			"/raw_ESR_ts_", bases[i], paste0("_fold", formatC(k, width=2, flag="0")), ".csv"), row.names = FALSE)

		# To filter non-correlated targets
		target.imp <- getTargetCorrelations(y.train)

		predictions.log <- y.test

		cat("Meta predictions\n")
		for(t in targets[[i]]) {
			cat(paste0(t, "\n"))
			filtered <- targets[[i]][target.imp[t,] > 0]
			filt.cn <- as.vector(outer(paste0("M", seq(len.ensemble)), filtered, paste, sep="."))

			regressor <- train_(base.predictions.tr[, filt.cn, with=FALSE], y.train[[t]], tech, targets[[i]])
			set(predictions.log, NULL, paste0(t, ".pred"), predict_(regressor, base.predictions.ts[, filt.cn, with=FALSE], tech, targets[[i]]))
		}

		write.csv(data.frame(id=sample.names[test.idx], predictions.log, check.names = F), paste0(output.dir.esr, "/prediction_logs/", tech,
			"/predictions_ESR_", bases[i], paste0("_fold", formatC(k, width=2, flag="0")), ".csv"), row.names = FALSE)
	}
}


#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.esr, "/prediction_logs"))
i <<- 1

lapply(bases, function(b) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RRMSE.", targets[[i]]), paste0("RMSE.", targets[[i]]))
	performance.log <<- data.frame(algorithm=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
												simplify = F), names.perf.log)), stringsAsFactors = FALSE)

	folds.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
										simplify = F), names.perf.log), stringsAsFactors = FALSE)
	lapply(1:folds.num, function(k) {
		log <- read.csv(paste0(getwd(),"/", tech, "/predictions_ESR_", b, paste0("_fold", formatC(k, width=2, flag="0")),".csv"), header=TRUE)
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

	write.csv(performance.log, paste0("../performance_ESR_", tech, "_", b, ".csv"), row.names = FALSE)
	i <<- i + 1
})
setwd(actual.folder)
