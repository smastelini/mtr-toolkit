dir.create(paste0(output.dir.orc, "/prediction_logs/",tech), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.orc, "/out_imp_assessment/",tech), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.orc, "/raw_logs/",tech), showWarnings = FALSE, recursive = TRUE)

hoeffdings.bound <- function(observations, range = 100, confidence = 0.05) {
	return(sqrt((range*log(1/confidence))/(2*observations)))
}

getChainingTree <- function(imp, tar, hb, max.level) {
	chain <- new.env()
	chain$tree <- data.table(orig=numeric(0), dest=numeric(0))
	chain$hash <- list()
	chain$imp <- imp
	chain$hb <- hb
	chain$max.level <- max.level

	letitchaining <- function(target = tar, node.id = 1, level = 1) {
		chain$hash[node.id] <- target

		if(level < chain$max.level) {
			max.i <- which.max(chain$imp[target,])
			# filter relevant targets
			rel.idx <- which(chain$imp[target,] >= chain$imp[target, max.i] - chain$hb)
			rel <- colnames(chain$imp)[rel.idx]

			next.t <- node.id + 1
			for(r in rel) {
				chain$tree <- rbindlist(list(chain$tree, list(orig=node.id, dest=next.t)))
				next.t <- letitchaining(r, next.t, level+1)
			}
			return(next.t)
		} else {
			chain$tree <- rbindlist(list(chain$tree, list(orig=node.id, dest=NA)))
			return(node.id + 1)
		}
	}
	letitchaining()
	return(list(tree = chain$tree, hash = chain$hash, depth = chain$max.level))
}

targets <- list()
maxs <- list()
mins <- list()

for(i in 1:length(bases)) {
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

	###################################Use a testing set#####################################
	if(length(bases.teste) > 0 && folds.num == 1) {
		dataset.teste <- read.csv(paste0(datasets.folder, "/", bases.teste[i], ".csv"))

		invisible(dataset.teste[, names(dataset.teste) := lapply(.SD, as.numeric)])

		dataset.teste <- as.data.table(scale(dataset.teste, center = mins[[i]], scale = maxs[[i]] - mins[[i]]))
		init.bound <- nrow(dataset) + 1

		dataset <- rbindlist(list(dataset, dataset.teste))
		sample.names <- c(sample.names, rownames(dataset.teste))
	}
	#########################################################################################

	x <- dataset[, !targets[[i]], with = FALSE]
	y <- dataset[, targets[[i]], with = FALSE]

	if(showProgress){}else{print(bases[i])}

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
			test.idx <- ((k-1)*len.fold + 1):(ifelse(k==folds.num, nrow(dataset), k*len.fold))
			train.idx <- setdiff(1:nrow(dataset), test.idx)
		}

		x.train <- x[train.idx]
		y.train <- y[train.idx]

		x.test <- x[test.idx]
		y.test <- y[test.idx]

		########################################RF Importance calc#############################################
		timportance <- getTargetImportance(y.train)
		write.csv(timportance, paste0(output.dir.orc, "/out_imp_assessment/", tech, "/", bases[i], "_RF_importance_fold", formatC(k, width=2, flag="0"), ".csv"))
		########################################################################################################

  #   prediction.log <- as.data.frame(setNames(replicate(length(col.names.targets),numeric(nrow(x.test)), simplify = F),
		# 											col.names.targets))

    for(t in targets[[i]]) {
    	orc <- getChainingTree(timportance, t, hoeffdings.bound(nrow(x.train), max(timportance)-min(timportance)), round(n.targets/1.5))
  
    }

    
  #   prediction.log <- cbind(sample.names[test.idx], prediction.log)
		# write.csv(prediction.log, paste0(output.dir.orc, "/prediction_logs/",tech,"/predictions_DRC_", bases[i], paste0("_fold", formatC(k, width=2, flag="0")), ".csv"), row.names = FALSE)
  }
}

# #Performance metrics
# actual.folder <- getwd()
# setwd(paste0(output.dir.orc, "/prediction_logs"))
# i <<- 1

# lapply(bases, function(b) {
# 	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RMSE.", targets[[i]]))
# 	performance.log <<- data.frame(algorithm=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
# 												simplify = F), names.perf.log)), stringsAsFactors = FALSE)

# 	folds.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
# 										simplify = F), names.perf.log), stringsAsFactors = FALSE)
# 	lapply(1:folds.num, function(k) {
# 		log <- read.csv(paste0(getwd(),"/", tech, "/predictions_DRC_", b, paste0("_fold", formatC(k, width=2, flag="0")),".csv"), header=TRUE)
# 		folds.log[nrow(folds.log)+1, "aCC"] <<- aCC(log, targets[[i]])
# 		folds.log[nrow(folds.log), "ARE"] <<- ARE(log, targets[[i]])
# 		folds.log[nrow(folds.log), "MSE"] <<- MSE(log, targets[[i]])
# 		folds.log[nrow(folds.log), "aRMSE"] <<- aRMSE(log, targets[[i]])
# 		folds.log[nrow(folds.log), "aRRMSE"] <<- aRRMSE(log, targets[[i]])

# 		# targets
# 		for(t in targets[[i]]) {
# 			folds.log[nrow(folds.log), paste0("R2.", t)] <<- summary(lm(log[,t] ~ log[, paste0(t, ".pred")]))$r.squared

# 			r <- (maxs[[i]][t]-mins[[i]][t])*log[,t] + mins[[i]][t]
# 			p <- (maxs[[i]][t]-mins[[i]][t])*log[,paste0(t, ".pred")] + mins[[i]][t]

# 			folds.log[nrow(folds.log), paste0("RMSE.", t)] <<- RMSE(r, p)
# 		}
# 	})
# 	performance.log[nrow(performance.log)+1, 1] <<- tech
# 	performance.log[nrow(performance.log), -1] <<- colMeans(folds.log)

# 	write.csv(performance.log, paste0("../performance_DRC_", tech, "_", b, ".csv"), row.names = FALSE)
# 	i <<- i + 1
# })
# setwd(actual.folder)
