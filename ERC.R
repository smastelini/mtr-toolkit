dir.create(paste0(output.dir.erc, "/prediction_logs/",tech), showWarnings = FALSE, recursive = TRUE)

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

	if(showProgress){pb$tick()}else{print(bases[i])}
	#print(bases[i])

	col.names.targets <- c()
	for(t in targets[[i]]) {
		col.names.targets <- c(col.names.targets, t)
		col.names.targets <- c(col.names.targets, paste0(t, ".pred"))
	}

	# Chain combinations
	nperm <- factorial(n.targets[i])
	#Length filter
	if(nperm > 10) {
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
		if(showProgress){pb$tick()}else{print(paste0("Fold ", k))}
		
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


		models <- list()
		for(j in 1:length(combinations)) {
			xtrn <- x.train
			xtst <- x.test
			
			# Training and testing sets building
			len.cbn <- length(combinations[[j]])
			if(length(models[[paste(combinations[[j]][1:(len.cbn-1)], collapse="-")]]) != 0) {
				for(l in 1:(len.cbn-1)) {
					actual.model <- paste(combinations[[j]][1:l], collapse="-")
					xtrn <- cbind(xtrn, models[[actual.model]]$pred.trn)
					xtst <- cbind(xtst, models[[actual.model]]$pred.tst)
				}
			}

			t <- combinations[[j]][len.cbn]
			actual.model <- paste(combinations[[j]], collapse="-")
			pred.train <- as.data.frame(setNames(replicate(1, numeric(nrow(x.train)), simplify=F), t))
			pred.test <- as.data.frame(setNames(replicate(1, numeric(nrow(x.test)), simplify=F), t))

			regressor <- train_(xtrn, y.train[,t], tech, targets[[i]])
			

			pred.train[,1] <- predict_(regressor, xtrn, tech, targets[[i]])
			pred.test[,1] <- predict_(regressor, xtst, tech, targets[[i]])

			models[[actual.model]] <- list(pred.trn=pred.train, pred.tst=pred.test)
		}

		#Prediction logs
		prediction.log <- as.data.frame(setNames(replicate(length(col.names.targets),numeric(nrow(x.test)), simplify = F),
															col.names.targets))

		for(t in targets[[i]]) {
		  if(showProgress){pb$tick()}
		  idx <- sapply(1:length(combinations), function(x) {
				return(combinations[[x]][length(combinations[[x]])] == t)
			})
			idx <- which(idx)

			pred <- models[[idx[1]]]$pred.tst
      
			for(j in 2:length(idx)) {
				pred <- cbind(pred, models[[idx[j]]]$pred.tst)
			}

			predictions <- rowMeans(pred)

			prediction.log[,t] <- y.test[,t]
			prediction.log[,paste0(t, ".pred")] <- predictions
		}
		prediction.log <- cbind(sample.names[test.idx], prediction.log)
		write.csv(prediction.log, paste0(output.dir.erc, "/prediction_logs/",tech,"/predictions_ERC_", bases[i], paste0("_fold", formatC(k, width=2, flag="0")), ".csv"), row.names = FALSE)
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
