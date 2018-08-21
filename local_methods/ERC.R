dir.create(paste0(output.dir.erc, "/prediction_logs/",tech), showWarnings = FALSE, recursive = TRUE)

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

	col.names.targets <- c()
	for(t in targets[[i]]) {
		col.names.targets <- c(col.names.targets, t)
		col.names.targets <- c(col.names.targets, paste0(t, ".pred"))
	}

	#Length filter
	if(n.targets[i] > 3) {
		cbn <- shuffleSet(n.targets[i], 500, quietly = TRUE)
		cbn <- rbind(cbn, c(1:ncol(y)))
		cbn <- cbn[sample(nrow(cbn), 10),]
	} else {
		cbn <- shuffleSet(n.targets[i], quietly = TRUE)
		cbn <- rbind(cbn, c(1:ncol(y)))
	}

	combinations <- list()
	for(c in 1:nrow(cbn)) {
		combinations[[length(combinations)+1]] <- c(colnames(y)[cbn[c,1]])
		for(d in 2:ncol(cbn)) {
			combinations[[length(combinations)+1]] <- c(combinations[[length(combinations)]], colnames(y)[cbn[c,d]])
		}
	}
	combinations <- unique(combinations)

	# Cross validation
	for(k in 1:folds.num) {
		#print(paste0("Fold ", k))
		print(paste0("Fold ", k))

		if(folds.num == 1) {
			if(length(bases.teste) > 0) {
				train.idx <- 1:(init.bound-1)
				test.idx <- init.bound:nrow(dataset)
			} else {
				test.idx <- 1:nrow(dataset)
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

		models <- list()
		for(j in 1:length(combinations)) {
			# Training and testing sets building
			len.cbn <- length(combinations[[j]])
			if(length(models[[paste(combinations[[j]][1:(len.cbn-1)], collapse="-")]]) != 0) {
				for(l in 1:(len.cbn-1)) {
					actual.model <- paste(combinations[[j]][1:l], collapse="-")
					x.train[, (combinations[[j]][l]) := models[[actual.model]]$pred.trn[[1]]]
					x.test[, (combinations[[j]][l]) := models[[actual.model]]$pred.tst[[1]]]
				}
			}

			t <- combinations[[j]][len.cbn]
			actual.model <- paste(combinations[[j]], collapse="-")
			pred.train <- as.data.table(setNames(replicate(1, numeric(nrow(x.train)), simplify=F), t))
			pred.test <- as.data.table(setNames(replicate(1, numeric(nrow(x.test)), simplify=F), t))

			regressor <- train_(x.train, y.train[[t]], tech, targets[[i]])

			pred.train[,1] <- predict_(regressor, x.train, tech, targets[[i]])
			pred.test[,1] <- predict_(regressor, x.test, tech, targets[[i]])

			if(len.cbn > 1) {
			  x.train[, combinations[[j]][1:(len.cbn-1)] := NULL]
			  x.test[, combinations[[j]][1:(len.cbn-1)] := NULL]
			}

			models[[actual.model]] <- list(pred.trn=pred.train, pred.tst=pred.test)
			if(showProgress){pb$tick()}
		}

		#Prediction logs
		prediction.log <- as.data.table(setNames(replicate(length(col.names.targets),numeric(nrow(x.test)), simplify = F),
															col.names.targets))

		for(t in targets[[i]]) {
		  idx <- sapply(1:length(combinations), function(x) {
				return(combinations[[x]][length(combinations[[x]])] == t)
			})
			idx <- which(idx)

			pred <- data.table(matrix(nrow=nrow(x.test), ncol=length(idx)))

			for(j in 1:length(idx)) {
				set(pred, NULL, as.integer(j), models[[idx[j]]]$pred.tst)
			}

			predictions <- rowMeans(pred)

			prediction.log[[t]] <- y.test[[t]]
			prediction.log[[paste0(t, ".pred")]] <- predictions
		}

		write.csv(data.frame(id=sample.names[test.idx], prediction.log, check.names = F), paste0(output.dir.erc, "/prediction_logs/",tech,"/predictions_ERC_", bases[i], paste0("_fold", formatC(k, width=2, flag="0")), ".csv"), row.names = FALSE)
	}
}

#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.erc, "/prediction_logs"))
i <<- 1

lapply(bases, function(b) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RMSE.", targets[[i]]))
	performance.log <<- data.frame(algorithm=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
												simplify = F), names.perf.log)), stringsAsFactors = FALSE)

	folds.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
										simplify = F), names.perf.log), stringsAsFactors = FALSE)
	lapply(1:folds.num, function(k) {
		log <- read.csv(paste0(getwd(),"/", tech, "/predictions_ERC_", b, paste0("_fold", formatC(k, width=2, flag="0")),".csv"), header=TRUE)
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

	write.csv(performance.log, paste0("../performance_ERC_", tech, "_", b, ".csv"), row.names = FALSE)
	i <<- i + 1
})
setwd(actual.folder)
