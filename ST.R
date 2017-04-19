###############################################################
####################Definiçoes iniciais########################
# output.dir.st <- "~/mastelini/regression_MT/exp_benchmarks2.0/NO_TUNE/ST"
# bases <- c("andro","atp1d","atp7d","edm","enb","jura","oes10","oes97","osales","rf1","rf2","scm1d",
#			"scm20d","scpf","sf1","sf2","slump","wq")
# n.targets <- c(6,6,6,2,2,3,16,16,12,8,8,16,16,3,3,3,3,14)


dir.create(paste0(output.dir.st, "/prediction_logs/", tech), showWarnings = FALSE, recursive = TRUE)

targets <- list()

for(i in 1:length(bases)) {
	dataset <- read.csv(paste0(datasets.folder, "/", bases[i], ".csv"))
	dataset <- remove.unique(dataset)

	targets[[i]] <- colnames(dataset)[(ncol(dataset)-n.targets[i]+1):ncol(dataset)]

	# Remocao de atributos de qualidade de imagem
	# dataset <- subset(dataset, select = -c(SNM, GCF, EME))

	dataset <- dataset[sample(nrow(dataset)),]
	# sample.names <- dataset[,c(1)]
	# dataset <- dataset[,-c(1)]
	sample.names <- rownames(dataset)

	dataset <- as.data.frame(sapply(dataset, function(x) as.numeric(x)))

	maxs <- apply(dataset, 2, max)
	mins <- apply(dataset, 2, min)
	dataset <- as.data.frame(scale(dataset, center = mins, scale = maxs - mins))

	len.fold <- round(nrow(dataset)/folds.num)

	######Usar um testing set
	if(length(bases.teste) > 0 && folds.num == 1) {
		dataset.teste <- read.csv(paste0(datasets.folder, "/", bases.teste[i], ".csv"))
		dataset.teste <- as.data.frame(sapply(dataset.teste, function(x) as.numeric(x)))
		dataset.teste <- as.data.frame(scale(dataset.teste, center = mins, scale = maxs - mins))
		init.bound <- nrow(dataset) + 1
		dataset <- rbind(dataset, dataset.teste)
		sample.names <- c(sample.names, rownames(dataset.teste))
	}
	rownames(dataset) <- 1:nrow(dataset)
	#######

	x <- dataset[, 1:(ncol(dataset)-length(targets[[i]]))]
	y <- dataset[, targets[[i]]]

	print(bases[i])

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
			test.idx <- as.numeric(rownames(dataset[((k-1)*len.fold + 1):(ifelse(k==folds.num, nrow(dataset), k*len.fold)),]))
			train.idx <- as.numeric(rownames(dataset[-test.idx,]))
		}

		x.train <- x[train.idx,]
		y.train <- y[train.idx,]

		x.test <- x[test.idx,]
		y.test <- y[test.idx,]

		prediction.log <- as.data.frame(setNames(replicate(length(col.names.targets),numeric(nrow(x.test)), simplify = F),
													col.names.targets))

		for(t in targets[[i]]) {
			print(t)
			predictions <- rep(0, nrow(x.test))
			regressor <- train_(x.train, y.train[,t], tech, targets[[i]])
			predictions <- predict_(regressor, x.test, tech, targets[[i]])

			prediction.log[,t] <- y.test[,t]
			prediction.log[,paste0(t, ".pred")] <- predictions
		}
		prediction.log <- cbind(sample.names[test.idx], prediction.log)
		write.csv(prediction.log, paste0(output.dir.st, "/prediction_logs/",tech,"/predictions_ST_", bases[i], paste0("_fold", formatC(k, width=2, flag="0")), ".csv"), row.names = FALSE)
	}
}

#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.st, "/prediction_logs"))
i <<- 1

lapply(bases, function(b) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RMSE.", targets[[i]]))
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
			folds.log[nrow(folds.log), paste0("RMSE.", t)] <<- RMSE(log[,t], log[, paste0(t, ".pred")])
		}
	})
	performance.log[nrow(performance.log)+1, 1] <<- tech
	performance.log[nrow(performance.log), -1] <<- colMeans(folds.log)

	write.csv(performance.log, paste0("../performance_ST_", tech, "_", b, ".csv"), row.names = FALSE)
	i <<- i + 1
})
setwd(actual.folder)
