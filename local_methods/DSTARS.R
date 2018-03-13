dir.create(paste0(output.dir.dstars, "/output_logs/tuning_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstars, "/output_logs/convergence_layers_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstars, "/output_logs/modelling_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstars, "/output_logs/testing_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstars, "/output_logs/testing_final_logs"), showWarnings = FALSE, recursive = TRUE)

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

	len.fold.eval <- round(nrow(dataset)/folds.num)

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

	x <- dataset[,!targets[[i]], with = FALSE]
	y <- dataset[,targets[[i]], with = FALSE]

	if(showProgress){}else{print(bases[i])}

	for(j in 1:folds.num) {
		if(showProgress){}else{print(paste("Fold Training:", j))}
		if(folds.num == 1) {
			if(length(bases.teste) > 0) {
				modelling.idx <- 1:(init.bound-1)
				testing.idx <- init.bound:nrow(dataset)
			} else {
				testing.idx <- 1:nrow(dataset)
				modelling.idx <- testing.idx
			}
		} else {
			testing.idx <- ((j-1)*len.fold.eval + 1):(ifelse(j==folds.num, nrow(dataset), j*len.fold.eval))
			modelling.idx <- setdiff(1:nrow(dataset), testing.idx)
		}

		modelling.names <- sample.names[modelling.idx]

		modelling.set.x <- x[modelling.idx]
		modelling.set.y <- y[modelling.idx]

		testing.names <- sample.names[testing.idx]

		testing.set.x <- x[testing.idx]
		testing.set.y <- y[testing.idx]

		# Bootstrap training and validation sets definition
		bootstrap.idx <- sample(length(modelling.idx), replace=TRUE)
		x.itb <- modelling.set.x[bootstrap.idx]
		y.itb <- modelling.set.y[bootstrap.idx]

		oob.idx <- unique(bootstrap.idx)

		x.oob <- modelling.set.x[oob.idx]
		y.oob <- modelling.set.y[oob.idx]

		predictions.training <- list()
		predictions.validation <- list()

		predictions.training <- y.itb
		predictions.validation <- y.oob

		for(t in targets[[i]]) {
			regressor <- train_(x.itb, y.itb[[t]], tech, targets[[i]])
			predictions.training[, (paste(0,t,sep=".")) := predict_(regressor, x.itb, tech, targets[[i]])]
			predictions.validation[, (paste(0,t,sep=".")) := predict_(regressor, x.oob, tech, targets[[i]])]
		}

		############################ RF Importance calc ###################################
		rf.importance <- list()
		timportance <- matrix(nrow = n.targets[i], ncol = n.targets[i])

		cont <- 1
		for(t in targets[[i]]) {
			rf.aux <- randomForest::randomForest((predictions.validation[,paste(0,targets[[i]],sep="."), with = FALSE]), 
				predictions.validation[[t]], importance = TRUE)
			imp.aux <- randomForest::importance(rf.aux, type = 1)
			imp.aux[imp.aux < 0] <- 0

			rf.importance[[targets[[i]][cont]]] <- as.logical(imp.aux > 0)
			timportance[cont,] <- imp.aux
			cont <- cont + 1
		}

		rownames(timportance) <- colnames(timportance) <- targets[[i]]
		write.csv(timportance, paste0(output.dir.dstars, "/output_logs/convergence_layers_logs/", 
			bases[i], "_", tech, "_RF_importance_", formatC(j, width=2, flag="0"), ".csv"))
		###################################################################################

		convergence.layers <- as.data.table(matrix(nrow=n.folds.tracking, ncol=length(targets[[i]]) + 1, data = 0))
		set(convergence.layers, NULL, 1L, 1:n.folds.tracking)
		colnames(convergence.layers) <- c("folds/layers", targets[[i]])
		convergence.tracking <- as.data.table(setNames(replicate(length(targets[[i]]), logical(0), simplify = F), targets[[i]]))

		if(showProgress){} else {print(paste("Tracking"))}

		# Training
		converged <- rep(FALSE, n.targets[i])
		names(converged) <- targets[[i]]

		uncorr <- rep(FALSE, n.targets[i])
		names(uncorr) <- targets[[i]]

		# Uncorrelated targets are removed from the deep tracking process
		for(t in seq(n.targets[i])) {
			uncorr[t] <- all(!rf.importance[[t]][-t])
		}

		error.validation <- rep(Inf, n.targets[i])
		names(error.validation) <- targets[[i]]

		for(t in targets[[i]]) {
			rmse.validation <- RMSE(predictions.validation[[t]], predictions.validation[[paste(0,t,sep=".")]])
			error.validation[t] <- rmse.validation
		}

		convergence.layers <- rep(0, n.targets[i])
		names(convergence.layers) <- targets[[i]]
		convergence.tracking <- rbindlist(list(convergence.tracking, as.list(!converged)))
		
		# Separates (uncorrelated) ST tasks
		converged <- uncorr
		rlayer <- 1
		while(!all(converged)) {
			if(showProgress){pb$tick()}else{print(paste("Layer", rlayer))}

			for(t in targets[[i]]) {
				if(!uncorr[t]) {
					tck.tra <- x.itb
					tck.val <- x.oob

					chosen.t <- targets[[i]][rf.importance[[t]]]

					tck.tra[,(chosen.t) := predictions.training[, paste(convergence.layers[chosen.t], 
						chosen.t,sep="."), with = FALSE]]
					tck.val[,(chosen.t) := predictions.validation[, paste(convergence.layers[chosen.t],
						chosen.t,sep="."), with = FALSE]]

					regressor <- train_(tck.tra, y.itb[[t]], tech, targets[[i]])
					predictions.training[, (paste(rlayer, t, sep=".")) := predict_(regressor, tck.tra, tech, targets[[i]])]
					predictions.validation[, (paste(rlayer, t, sep=".")) := predict_(regressor, tck.val, tech, targets[[i]])]

					rmse.validation <- RMSE(predictions.validation[[t]], predictions.validation[[paste(rlayer, t, sep=".")]])
					if(rmse.validation + dstars.delta > error.validation[t]) {
						converged[t] <- TRUE
					} else {
						converged[t] <- FALSE
						error.validation[t] <- rmse.validation
						convergence.layers[t] <- rlayer
					}
				}
			}

			if(!all(converged)) {
				convergence.tracking <- rbindlist(list(convergence.tracking, as.list(!converged)))
			}
			rlayer <- rlayer + 1
		}

		write.csv(data.frame(id=modelling.names[bootstrap.idx], predictions.training, check.names = F), paste0(output.dir.dstars,
			"/output_logs/tuning_raw_logs/", bases[i], "_", tech, "_training_predictions_EV_fold_", formatC(j, width=2, flag="0"), ".csv"),
				row.names = F)
		write.csv(data.frame(id=modelling.names[oob.idx], predictions.validation, check.names = F), paste0(output.dir.dstars,
			"/output_logs/tuning_raw_logs/", bases[i], "_", tech, "_validation_predictions_EV_fold_", formatC(j, width=2, flag="0"), ".csv"), 
				row.names = F)

		rm(predictions.training, predictions.validation)
		write.csv(data.frame(layer=0:(nrow(convergence.tracking)-1), convergence.tracking, check.names = F), 
			paste0(output.dir.dstars, "/output_logs/convergence_layers_logs/", bases[i], "_", tech, "_convergence_accounting_EV_fold_", 
				formatC(j, width=2, flag="0"), ".csv"), row.names = F)

		convergence.layers <- as.numeric(convergence.tracking[, lapply(.SD, function(z) BBmisc::which.last(z) - 1)])
		names(convergence.layers) <- targets[[i]]

		predictions.modelling <- y[modelling.idx]
		predictions.testing <- y[testing.idx]

		max.layers.reached <- rep(FALSE, n.targets[i])
		names(max.layers.reached) <- targets[[i]]

		print("Final Modelling")
		if(showProgress){}else{print("Layer 0")}

		for(t in targets[[i]]) {
			regressor <- train_(modelling.set.x, modelling.set.y[[t]], tech, targets[[i]])
			predictions.modelling[, (paste(0,t,sep=".")) := predict_(regressor, modelling.set.x, tech, targets[[i]])]
			predictions.testing[, (paste(0,t,sep=".")) := predict_(regressor, testing.set.x, tech, targets[[i]])]

			if(convergence.layers[t] == 0) {
				max.layers.reached[t] <- TRUE
			}
		}

		chosen.layers <- rep(0, n.targets[i])
		names(chosen.layers) <- targets[[i]]
		rlayer <- 1

		while(!all(max.layers.reached)) {
			if(showProgress){}else{print(paste("Layer", rlayer))}
			for(t in targets[[i]]) {
				if(convergence.tracking[rlayer+1][[t]]) {
					chosen.t <- targets[[i]][rf.importance[[t]]]

					modelling.set.x[, (chosen.t) := predictions.modelling[, paste(chosen.layers[chosen.t], chosen.t, sep="."), with = F]]
					testing.set.x[, (chosen.t) := predictions.testing[, paste(chosen.layers[chosen.t], chosen.t, sep="."), with = F]]

					regressor <- train_(modelling.set.x, modelling.set.y[[t]], tech, targets[[i]])
					predictions.modelling[, (paste(rlayer, t, sep=".")) := predict_(regressor, modelling.set.x, tech, targets[[i]])]
					predictions.testing[, (paste(rlayer, t, sep=".")) := predict_(regressor, testing.set.x, tech, targets[[i]])]

					modelling.set.x[, (chosen.t) := NULL]
					testing.set.x[, (chosen.t) := NULL]
				}

				if(rlayer == convergence.layers[t]) {
					max.layers.reached[t] <- TRUE
				}
			}

			addressing <- convergence.tracking[rlayer+1]
			addressing <- which(addressing == TRUE)
			chosen.layers[addressing] <- rlayer

			rlayer <- rlayer + 1
		}

		write.csv(data.frame(id=modelling.names, predictions.modelling, check.names = F), paste0(output.dir.dstars,
			"/output_logs/modelling_raw_logs/", bases[i], "_", tech, "_modelling_predictions_fold", formatC(j, width=2, flag="0"), ".csv"),
				row.names = F)
		write.csv(data.frame(id=testing.names, predictions.testing, check.names = F), paste0(output.dir.dstars,
			"/output_logs/testing_raw_logs/", bases[i], "_", tech, "_testing_predictions_fold", formatC(j, width=2, flag="0"), ".csv"),
				row.names = F)

		final.predictions <- testing.set.y
		final.predictions[, (paste0(targets[[i]], ".pred")) := predictions.testing[, paste(convergence.layers,
			targets[[i]], sep="."), with = F]]

		write.csv(data.frame(id=testing.names, final.predictions, check.names = F), paste0(output.dir.dstars, 
			"/output_logs/testing_final_logs/", bases[i], "_", tech, "_testing_final_predictions_fold",
				formatC(j, width=2, flag="0"), ".csv"), row.names = F)
	}
}

#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.dstars, "/output_logs"))
i <<- 1
lapply(bases, function(b) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RMSE.", targets[[i]]))
	performance.log <<- data.frame(dataset=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
							simplify = F), names.perf.log)), stringsAsFactors = FALSE)
	
	repetition.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
							simplify = F), names.perf.log), stringsAsFactors = FALSE)
	lapply(1:folds.num, function(k) {
		log <- read.csv(paste0(getwd(), "/testing_final_logs/", b, "_", tech, "_testing_final_predictions_fold", 
			formatC(k, width=2, flag="0"), ".csv"))
		repetition.log[nrow(repetition.log)+1, "aCC"] <<- aCC(log, targets[[i]])
		repetition.log[nrow(repetition.log), "ARE"] <<- ARE(log, targets[[i]])
		repetition.log[nrow(repetition.log), "MSE"] <<- MSE(log, targets[[i]])
		repetition.log[nrow(repetition.log), "aRMSE"] <<- aRMSE(log, targets[[i]])
		repetition.log[nrow(repetition.log), "aRRMSE"] <<- aRRMSE(log, targets[[i]])

		# targets
		for(t in targets[[i]]) {
			repetition.log[nrow(repetition.log), paste0("R2.", t)] <<- summary(lm(log[,t] ~ log[, paste0(t, ".pred")]))$r.squared

			r <- (maxs[[i]][t]-mins[[i]][t])*log[,t] + mins[[i]][t]
			p <- (maxs[[i]][t]-mins[[i]][t])*log[,paste0(t, ".pred")] + mins[[i]][t]

			repetition.log[nrow(repetition.log), paste0("RMSE.", t)] <<- RMSE(r, p)
		}

	})
	performance.log[nrow(performance.log)+1, 1] <<- tech
	performance.log[nrow(performance.log), -1] <<- colMeans(repetition.log)
	write.csv(performance.log, paste0("../performance_DSTARS_", tech, "_", b, ".csv"), row.names = FALSE)
	
	i <<- i + 1
})
setwd(actual.folder)
