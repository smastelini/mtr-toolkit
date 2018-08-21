dir.create(paste0(output.dir.mtas, "/prediction_logs/",tech), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.mtas, "/out_imp_assessment/",tech), showWarnings = FALSE, recursive = TRUE)

stacked.regressors <- techs

targets <- list()
maxs <- list()
mins <- list()

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

	maxs[[i]] <- as.numeric(dataset[, lapply(.SD, max)])
	names(maxs[[i]]) <- colnames(dataset)
	mins[[i]] <- as.numeric(dataset[, lapply(.SD, min)])
	names(mins[[i]]) <- colnames(dataset)

	dataset <- as.data.table(scale(dataset, center = mins[[i]], scale = maxs[[i]] - mins[[i]]))

	len.fold <- round(nrow(dataset)/folds.num)

	######Use a testing set
	if(length(bases.teste) > 0 && folds.num == 1) {
		dataset.teste <- read.csv(paste0(datasets.folder, "/", bases.teste[i], ".csv"))
		dataset.teste <- as.data.table(dataset.teste)
		invisible(dataset.teste[, names(dataset.teste) := lapply(.SD, as.numeric)])

		dataset.teste <- as.data.table(scale(dataset.teste, center = mins[[i]], scale = maxs[[i]] - mins[[i]]))
		init.bound <- nrow(dataset) + 1

		dataset <- rbindlist(list(dataset, dataset.teste))
		sample.names <- c(sample.names, rownames(dataset.teste))
	}
	#######

	x <- dataset[, !targets[[i]], with = FALSE]
	y <- dataset[, targets[[i]], with = FALSE]

	print(bases[i])
	#print(bases[i])

	col.names.targets <- c()
	for(t in targets[[i]]) {
		col.names.targets <- c(col.names.targets, t)
		col.names.targets <- c(col.names.targets, paste0(t, ".pred"))
	}

	# Cross validation
	for(k in 1:folds.num) {
	  print(paste0("Fold ", k))

		if(folds.num == 1) {
			if(length(bases.teste) > 0) {
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

		x.train <- x[train.idx]
		y.train <- y[train.idx]

		x.test <- x[test.idx]
		y.test <- y[test.idx]


		############################ RF Importance calc ###################################
		rf.importance <- list()
		timportance <- matrix(nrow = length(targets[[i]]), ncol = length(targets[[i]]))

		for(t in 1:length(targets[[i]])) {
			rf.aux <- randomForest::randomForest(y.train, y.train[[t]], importance = TRUE)
			imp.aux <- randomForest::importance(rf.aux, type = 1)
			imp.aux[imp.aux < 0] <- 0

			rf.importance[[targets[[i]][t]]] <- as.logical(imp.aux > 0)
			timportance[t,] <- imp.aux
		}

		colnames(timportance) <- rownames(timportance) <- targets[[i]]

		write.csv(timportance, paste0(output.dir.mtas, "/out_imp_assessment/", tech, "/", bases[i], "_RF_importance_fold", formatC(k, width=2, flag="0"), ".csv"))
		diag(timportance) <- 0
		uncorrelated <- as.logical(apply(timportance, 1, function(zeta) sum(zeta) == 0))
		names(uncorrelated) <- targets[[i]]
		###################################################################################

		names.logs.sg <- apply(expand.grid(stacked.regressors, targets[[i]]), 1, paste, collapse=".")
		# Predictions trainining set => input to the second layer of regressors
		predictions.l1.train <- as.data.table(setNames(replicate(length(names.logs.sg),numeric(nrow(x.train)), simplify = F),
																									names.logs.sg))
		# Predictions testing set => input to the second layer of regressors
		predictions.l1.test <- as.data.table(setNames(replicate(length(names.logs.sg),numeric(nrow(x.test)), simplify = F),
																									names.logs.sg))

		# Final logs
		prediction.log <- as.data.table(setNames(replicate(length(col.names.targets),numeric(nrow(x.test)), simplify = F),
																									col.names.targets))

		print("Level 1")
		for(t in targets[[i]]) {
			else{print(t)
				for(sgr in stacked.regressors) {
					regressor <- train_(x.train, y.train[[t]], sgr, targets[[i]])

					set(predictions.l1.train, NULL, paste(sgr,t,sep="."), predict_(regressor, x.train, sgr, targets[[i]]))
					set(predictions.l1.test, NULL, paste(sgr,t,sep="."), predict_(regressor, x.test, sgr, targets[[i]]))
				}
		}

		print("Level 2")
		for(t in targets[[i]]) {
			chosen.t <- targets[[i]][rf.importance[[t]]]
			names.t.l2 <- apply(expand.grid(stacked.regressors, chosen.t), 1, paste, collapse=".")

		  print(t)

			x.train[, (names.t.l2) := predictions.l1.train[, names.t.l2, with = FALSE]]
			x.test[, (names.t.l2) := predictions.l1.test[, names.t.l2, with = FALSE]]

			regressor <- train_(x.train, y.train[[t]], tech, targets[[i]])
			set(prediction.log, NULL, t, y.test[[t]])
			set(prediction.log, NULL, paste0(t, ".pred"), predict_(regressor, x.test, tech, targets[[i]]))

			x.train[, (names.t.l2) := NULL]
			x.test[, (names.t.l2) := NULL]
		}

		write.csv(data.frame(id=sample.names[test.idx], prediction.log, check.names = FALSE), paste0(output.dir.mtas, "/prediction_logs/",tech, "/predictions_MTAS_", bases[i], paste0("_fold", formatC(k, width=2, flag="0")), ".csv"), row.names = FALSE)
	}
}

#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.mtas, "/prediction_logs"))
i <<- 1

lapply(bases, function(b) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RMSE.", targets[[i]]))
	performance.log <<- data.frame(algorithm=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
												simplify = F), names.perf.log)), stringsAsFactors = FALSE)

	folds.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
										simplify = F), names.perf.log), stringsAsFactors = FALSE)
	lapply(1:folds.num, function(k) {
		log <- read.csv(paste0(getwd(),"/", tech, "/predictions_MTAS_", b, paste0("_fold", formatC(k, width=2, flag="0")),".csv"), header=TRUE)
		folds.log[nrow(folds.log)+1, "aCC"] <<- aCC(log, targets[[i]])
		folds.log[nrow(folds.log), "ARE"] <<- ARE(log, targets[[i]])
		folds.log[nrow(folds.log), "MSE"] <<- MSE(log, targets[[i]])
		folds.log[nrow(folds.log), "aRMSE"] <<- aRMSE(log, targets[[i]])
		folds.log[nrow(folds.log), "aRRMSE"] <<- aRRMSE(log, targets[[i]])

		# targets
		for(t in targets[[i]]) {
			folds.log[nrow(folds.log), paste0("R2.", t)] <<- summary(lm(log[,t] ~ log[, paste0(t, ".pred")]))$r.squared

			r <- (maxs[[i]][t]-mins[[i]][t])*log[,t] + mins[[i]][t]
			p <- (maxs[[i]][t]-mins[[i]][t])*log[,paste0(t, ".pred")] + mins[[i]][t]

			folds.log[nrow(folds.log), paste0("RMSE.", t)] <<- RMSE(r, p)
		}
	})
	performance.log[nrow(performance.log)+1, 1] <<- tech
	performance.log[nrow(performance.log), -1] <<- colMeans(folds.log)

	write.csv(performance.log, paste0("../performance_MTAS_", tech, "_", b, ".csv"), row.names = FALSE)
	i <<- i + 1
})
setwd(actual.folder)
