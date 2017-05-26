dir.create(paste0(output.dir.sgsst, "/prediction_logs/",tech), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.sgsst, "/out_imp_assessment/",tech), showWarnings = FALSE, recursive = TRUE)

stacked.regressors <- c("parrf", "svm", "gbm", "lr", "mlp", "cart", "ridge")

targets <- list()
maxs <- list()
mins <- list()

for(i in 1:length(bases)) {
	dataset <- read.csv(paste0(datasets.folder, "/", bases[i], ".csv"))
	dataset <- remove.unique(dataset)

	targets[[i]] <- colnames(dataset)[(ncol(dataset)-n.targets[i]+1):ncol(dataset)]

	dataset <- dataset[sample(nrow(dataset)),]
	sample.names <- rownames(dataset)

	dataset <- as.data.frame(sapply(dataset, function(x) as.numeric(x)))

	maxs[[i]] <- apply(dataset, 2, max)
	mins[[i]] <- apply(dataset, 2, min)
	dataset <- as.data.frame(scale(dataset, center = mins[[i]], scale = maxs[[i]] - mins[[i]]))

	len.fold <- round(nrow(dataset)/folds.num)

	######Use a testing set
	if(length(bases.teste) > 0 && folds.num == 1) {
		dataset.teste <- read.csv(paste0(datasets.folder, "/", bases.teste[i], ".csv"))
		dataset.teste <- as.data.frame(sapply(dataset.teste, function(x) as.numeric(x)))
		dataset.teste <- as.data.frame(scale(dataset.teste, center = mins[[i]], scale = maxs[[i]] - mins[[i]]))
		init.bound <- nrow(dataset) + 1
		dataset <- rbind(dataset, dataset.teste)
		sample.names <- c(sample.names, rownames(dataset.teste))
	}
	rownames(dataset) <- 1:nrow(dataset)
	#######

	x <- dataset[, 1:(ncol(dataset)-length(targets[[i]]))]
	y <- dataset[, targets[[i]]]

	if(showProgress){}else{print(bases[i])}
	#print(bases[i])

	col.names.targets <- c()
	for(t in targets[[i]]) {
		col.names.targets <- c(col.names.targets, t)
		col.names.targets <- c(col.names.targets, paste0(t, ".pred"))
	}

	# Cross validation
	for(k in 1:folds.num) {
	  if(showProgress){}else{print(paste0("Fold ", k))}

		if(folds.num == 1) {
			if(length(bases.teste) > 0) {
				train.idx <- 1:(init.bound-1)
				test.idx <- init.bound:nrow(dataset)
			} else {
				test.idx <- as.numeric(rownames(dataset))
				train.idx <- test.idx
			}
		} else {
			test.idx <- as.numeric(rownames(dataset[((k-1)*len.fold + 1):(ifelse(k==folds.num, nrow(dataset), k*len.fold)),]))
			train.idx <- as.numeric(rownames(dataset[-test.idx,]))
		}

		x.train <- x[train.idx,]
		y.train <- y[train.idx,]

		x.test <- x[test.idx,]
		y.test <- y[test.idx,]


		############################ RF Importance calc ###################################
		rf.importance <- list()
		timportance <- matrix(nrow = length(targets[[i]]), ncol = length(targets[[i]]))

		for(t in 1:length(targets[[i]])) {
			rf.aux <- randomForest(y.train, y.train[,t], importance = TRUE)
			imp.aux <- importance(rf.aux, type = 1)
			imp.aux[imp.aux < 0] <- 0

			# imp.aux[imp.aux < imp.aux[t]/n.targets[i]] <- 0

			rf.importance[[targets[[i]][t]]] <- as.logical(imp.aux > 0)
			timportance[t,] <- imp.aux
		}

		write.csv(timportance, paste0(output.dir.sgsst, "/out_imp_assessment/", tech, "/", bases[i], "_RF_importance_fold", formatC(k, width=2, flag="0"), ".csv"))
		diag(timportance) <- 0
		uncorrelated <- as.logical(apply(timportance, 2, function(zeta) sum(zeta) == 0))
		names(uncorrelated) <- targets[[i]]
		###################################################################################

		names.logs.sg <- apply(expand.grid(stacked.regressors, targets[[i]][which(!uncorrelated)]), 1, paste, collapse=".")
		# Predictions trainining set => input to the second layer of regressors
		predictions.l1.train <- as.data.frame(setNames(replicate(length(names.logs.sg),numeric(nrow(x.train)), simplify = F),
																									names.logs.sg))
		# Predictions testing set => input to the second layer of regressors
		predictions.l1.test <- as.data.frame(setNames(replicate(length(names.logs.sg),numeric(nrow(x.test)), simplify = F),
																									names.logs.sg))

		# Final logs
		prediction.log <- as.data.frame(setNames(replicate(length(col.names.targets),numeric(nrow(x.test)), simplify = F),
																									col.names.targets))

		if(showProgress){}else{print("Level 1")}
		for(t in targets[[i]]) {
			if(showProgress){pb$tick()}else{print(t)}
			if(!uncorrelated[t]) {
				for(sgr in stacked.regressors) {
					regressor <- train_(x.train, y.train[,t], sgr, targets[[i]])
					predictions.l1.train[,paste(sgr,t,sep=".")] <- predict_(regressor, x.train, sgr, targets[[i]])
					predictions.l1.test[,paste(sgr,t,sep=".")] <- predict_(regressor, x.test, sgr, targets[[i]])
				}
			}
		}

		if(showProgress){}else{print("Level 2")}
		#print("Level 2")
		for(t in targets[[i]]) {
			chosen.t <- targets[[i]][rf.importance[[t]]]
			names.t.l2 <- apply(expand.grid(stacked.regressors, chosen.t), 1, paste, collapse=".")
			# TODO uncorrelated target
			x.train.l2 <- cbind(x.train, predictions.l1.train[, names.t.l2])

		  if(showProgress){pb$tick()}else{print(t)}

			predictions <- rep(0, nrow(x.test))

			regressor <- train_(x.train.l2, y.train[,t], tech, targets[[i]])
			predictions <- predict_(regressor, cbind(x.test, predictions.l1.test[,names.t.l2]), tech, targets[[i]])
			prediction.log[,t] <- y.test[,t]
			prediction.log[,paste0(t, ".pred")] <- predictions
		}

		prediction.log <- cbind(sample.names[test.idx], prediction.log)
		write.csv(prediction.log, paste0(output.dir.sgsst, "/prediction_logs/",tech, "/predictions_SGSST_", bases[i], paste0("_fold", formatC(k, width=2, flag="0")), ".csv"), row.names = FALSE)
	}
}

#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.sgsst, "/prediction_logs"))
i <<- 1

lapply(bases, function(b) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RMSE.", targets[[i]]))
	performance.log <<- data.frame(algorithm=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
												simplify = F), names.perf.log)), stringsAsFactors = FALSE)

	folds.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
										simplify = F), names.perf.log), stringsAsFactors = FALSE)
	lapply(1:folds.num, function(k) {
		log <- read.csv(paste0(getwd(),"/", tech, "/predictions_SGSST_", b, paste0("_fold", formatC(k, width=2, flag="0")),".csv"), header=TRUE)
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

	write.csv(performance.log, paste0("../performance_SGSST_", tech, "_", b, ".csv"), row.names = FALSE)
	i <<- i + 1
})
setwd(actual.folder)
