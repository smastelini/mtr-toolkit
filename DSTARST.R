dir.create(paste0(output.dir.dstarst, "/output_logs/tuning_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstarst, "/output_logs/convergence_layers_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstarst, "/output_logs/modelling_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstarst, "/output_logs/testing_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstarst, "/output_logs/testing_final_logs"), showWarnings = FALSE, recursive = TRUE)

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

	len.fold.eval <- round(nrow(dataset)/folds.num)

	######Use a testing set
	if(length(bases.teste) > 0 && folds.num == 1) {
		dataset.teste <- read.csv(paste0(datasets.folder, "/", bases.teste[i], ".csv"))

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

		len.fold.tuning <- round(nrow(modelling.set.x)/n.folds.tracking)

		predictions.training <- list()
		predictions.validation <- list()

		# Builds all tracking step ST models to verificate correlation between outcome variables
		for(k in 1:n.folds.tracking) {
			validation.idx <- ((k-1)*len.fold.tuning + 1):(ifelse(k==n.folds.tracking, nrow(modelling.set.x), k*len.fold.tuning))
			training.idx <- if(n.folds.tracking == 1) validation.idx else setdiff(1:nrow(modelling.set.x), validation.idx)
      
			x.training.tuning <- modelling.set.x[training.idx]
			y.training.tuning <- modelling.set.y[training.idx]

			x.validation.tuning <- modelling.set.x[validation.idx]
			y.validation.tuning <- modelling.set.y[validation.idx]

			predictions.training[[k]] <- y.training.tuning
			predictions.validation[[k]] <- y.validation.tuning

			for(t in targets[[i]]) {
				regressor <- train_(x.training.tuning, y.training.tuning[[t]], tech, targets[[i]])
				set(predictions.training[[k]], NULL, paste(0,t,sep="."), predict_(regressor, x.training.tuning, tech, targets[[i]]))
				set(predictions.validation[[k]], NULL, paste(0,t,sep="."), predict_(regressor, x.validation.tuning, tech, targets[[i]]))
			}

			if(k == 1)
				pimp <- predictions.validation[[k]]
			else
				pimp <- rbindlist(list(pimp, predictions.validation[[k]]))
		}

		############################ RF Importance calc ###################################
		rf.importance <- list()
		timportance <- matrix(nrow = length(targets[[i]]), ncol = length(targets[[i]]))

		cont <- 1
		for(k in targets[[i]]) {
			rf.aux <- randomForest((pimp[, paste(0,targets[[i]],sep="."), with = FALSE]), pimp[[k]], importance = TRUE)
			imp.aux <- importance(rf.aux, type = 1)
			imp.aux[imp.aux < 0] <- 0

			# imp.aux[imp.aux < imp.aux[k]/n.targets[i]] <- 0

			rf.importance[[targets[[i]][cont]]] <- as.logical(imp.aux > 0)
			timportance[cont,] <- imp.aux
			cont <- cont + 1
		}

		rownames(timportance) <- colnames(timportance) <- targets[[i]]
		write.csv(timportance, paste0(output.dir.dstarst, "/output_logs/convergence_layers_logs/", bases[i], "_", tech, "_RF_importance_", formatC(j, width=2, flag="0"), ".csv"))
		rm(pimp)
		###################################################################################

		convergence.layers <- as.data.table(matrix(nrow=n.folds.tracking, ncol=length(targets[[i]]) + 1, data = 0))
		set(convergence.layers, NULL, 1L, 1:n.folds.tracking)
		colnames(convergence.layers) <- c("folds/layers", targets[[i]])
		convergence.tracking <- as.data.table(setNames(replicate(length(targets[[i]]), numeric(0), simplify = F), targets[[i]]))

		if(showProgress){}else{print(paste("Tuning"))}

		# Cross validation
		for(k in 1:n.folds.tracking) {
			if(showProgress){pb$tick()}else{print(paste("Fold tuning", k))}
			validation.idx <- ((k-1)*len.fold.tuning + 1):(ifelse(k==n.folds.tracking, nrow(modelling.set.x), k*len.fold.tuning))
			training.idx <- if(n.folds.tracking == 1) validation.idx else setdiff(1:nrow(modelling.set.x), validation.idx)
      
			x.training.tuning <- modelling.set.x[training.idx]
			y.training.tuning <- modelling.set.y[training.idx]

			x.validation.tuning <- modelling.set.x[validation.idx]
			y.validation.tuning <- modelling.set.y[validation.idx]

			# Training
			converged <- rep(FALSE, n.targets[i])
			names(converged) <- targets[[i]]

			uncorr <- rep(FALSE, n.targets[i])
			names(uncorr) <- targets[[i]]

			# Uncorrelated targets are removed from the deep tracking process
			for(t in 1:n.targets[i]) {
				uncorr[t] <- all(!rf.importance[[t]][-t])
			}

			error.validation <- rep(Inf, n.targets[i])
			names(error.validation) <- targets[[i]]

			for(t in targets[[i]]) {
				rmse.validation <- RMSE(y.validation.tuning[[t]], predictions.validation[[k]][[paste(0,t,sep=".")]])
				error.validation[t] <- rmse.validation
				set(convergence.layers, k, t, 0)
			}
      
			if(nrow(convergence.tracking) == 0) {
				convergence.tracking <- rbindlist(list(convergence.tracking, as.list(as.numeric(!converged))))
			} else {
				set(convergence.tracking,1L, targets[[i]], convergence.tracking[1] + as.numeric(!converged))
			}

			converged <- uncorr
			rlayer <- 1
			while(!all(converged)) {
				if(showProgress){pb$tick()}else{print(paste("Layer", rlayer))}

				for(t in targets[[i]]) {
					if(!uncorr[t]) {
						tck.tra <- x.training.tuning
						tck.val <- x.validation.tuning

						chosen.t <- targets[[i]][rf.importance[[t]]]

						set(tck.tra,NULL,chosen.t, predictions.training[[k]][,paste(convergence.layers[k,chosen.t, with = FALSE], chosen.t,sep="."), with = FALSE])
						set(tck.val,NULL,chosen.t, predictions.validation[[k]][,paste(convergence.layers[k,chosen.t, , with = FALSE], chosen.t,sep="."), with = FALSE])

						regressor <- train_(tck.tra, y.training.tuning[[t]], tech, targets[[i]])
						set(predictions.training[[k]], NULL, paste(rlayer,t,sep="."), predict_(regressor, tck.tra, tech, targets[[i]]))
						set(predictions.validation[[k]], NULL, paste(rlayer,t,sep="."), predict_(regressor, tck.val, tech, targets[[i]]))

						rmse.validation <- RMSE(y.validation.tuning[[t]], predictions.validation[[k]][[paste(rlayer,t,sep=".")]])
						if(rmse.validation + dstars.delta > error.validation[t]) {
							converged[t] <- TRUE
						} else {
							converged[t] <- FALSE
							error.validation[t] <- rmse.validation
							set(convergence.layers, k, t, rlayer)
						}
					}
				}
        if(!all(converged)) {
					if(rlayer + 1 > nrow(convergence.tracking)) {
						convergence.tracking <- rbindlist(list(convergence.tracking, as.list(as.numeric(!converged))))
					} else {
					  set(convergence.tracking, as.integer(rlayer+1), targets[[i]], convergence.tracking[rlayer+1] + as.numeric(!converged))
					}
        }
				rlayer <- rlayer + 1
			}

			write.csv(data.frame(id=modelling.names[training.idx], predictions.training[[k]], check.names = F), paste0(output.dir.dstarst, "/output_logs/tuning_raw_logs/", bases[i], "_", tech, "_training_predictions_EV_fold_", formatC(j, width=2, flag="0"), "_TN_fold", formatC(k, width=2, flag="0"), ".csv"), row.names = F)
			write.csv(data.frame(id=modelling.names[validation.idx], predictions.validation[[k]], check.names = F), paste0(output.dir.dstarst, "/output_logs/tuning_raw_logs/", bases[i], "_", tech, "_validation_predictions_EV_fold_", formatC(j, width=2, flag="0"), "_TN_fold", formatC(k, width=2, flag="0"), ".csv"), row.names = F)
		}

		rm(predictions.training, predictions.validation)
		write.csv(data.frame(layer=0:(nrow(convergence.tracking)-1),convergence.tracking, check.names = F), paste0(output.dir.dstarst, "/output_logs/convergence_layers_logs/", bases[i], "_", tech, "_convergence_accounting_EV_fold_", formatC(j, width=2, flag="0"), ".csv"), row.names = F)

		invisible(convergence.tracking[, names(convergence.tracking) := lapply(.SD, function(nmrd, dnmd) nmrd/dnmd, dnmd = n.folds.tracking)])

		# Test different phi values
		for(dstars.phi in seq(0,1, 0.1)) {
			dir.create(paste0(output.dir.dstarst, "/output_logs/convergence_layers_logs/phi=",dstars.phi), showWarnings = FALSE, recursive = TRUE)
			dir.create(paste0(output.dir.dstarst, "/output_logs/modelling_raw_logs/phi=",dstars.phi), showWarnings = FALSE, recursive = TRUE)
			dir.create(paste0(output.dir.dstarst, "/output_logs/testing_raw_logs/phi=",dstars.phi), showWarnings = FALSE, recursive = TRUE)
			dir.create(paste0(output.dir.dstarst, "/output_logs/testing_final_logs/phi=",dstars.phi), showWarnings = FALSE, recursive = TRUE)

			convergence.tracking_ <- convergence.tracking[, lapply(.SD, function(z, threshold) z >= threshold, threshold = dstars.phi)]
			write.csv(data.frame(layer=0:(nrow(convergence.tracking_)-1), convergence.tracking_, check.names = F), paste0(output.dir.dstarst, "/output_logs/convergence_layers_logs/phi=",dstars.phi, "/", bases[i], "_", tech, "_convergence_tracking_EV_fold_", formatC(j, width=2, flag="0"), ".csv"), row.names = F)
			
			convergence.layers_ <- rbindlist(list(convergence.layers, as.list(c("modelling", as.numeric(convergence.tracking_[,lapply(.SD, function(z) BBmisc::which.last(z) - 1)])))))
			write.csv(convergence.layers_, paste0(output.dir.dstarst, "/output_logs/convergence_layers_logs/phi=", dstars.phi, "/", bases[i], "_", tech, "_convergence_layers_EV_fold_", formatC(j, width=2, flag="0"), ".csv"), row.names = F)

			if(showProgress){}else{print(paste("Fold", j, ", phi = ", dstars.phi, ", final modelling"))}

			predictions.modelling <- modelling.set.y
			predictions.testing <- testing.set.y

			max.layers.reached <- rep(FALSE, n.targets[i])
			names(max.layers.reached) <- targets[[i]]

			if(showProgress){}else{print("Layer 0")}

			for(t in targets[[i]]) {
				regressor <- train_(modelling.set.x, modelling.set.y[[t]], tech, targets[[i]])
				set(predictions.modelling, NULL, paste(0,t,sep="."), predict_(regressor, modelling.set.x, tech, targets[[i]]))
				set(predictions.testing, NULL, paste(0,t,sep="."), predict_(regressor, testing.set.x, tech, targets[[i]]))

				if(as.numeric(convergence.layers_[nrow(convergence.layers_),t, with = FALSE]) == 0) {
					max.layers.reached[t] <- TRUE
				}
			}

			chosen.layers <- rep(0, n.targets[i])
			names(chosen.layers) <- targets[[i]]
			rlayer <- 1

			while(!all(max.layers.reached)) {
				if(showProgress){}else{print(paste("Layer", rlayer))}
				for(t in targets[[i]]) {
					if(convergence.tracking_[rlayer+1][[t]]) {
						modelling.set.x_ <- modelling.set.x
						testing.set.x_ <- testing.set.x
						chosen.t <- targets[[i]][rf.importance[[t]]]

						set(modelling.set.x_, NULL, chosen.t, predictions.modelling[, paste(chosen.layers[chosen.t], chosen.t, sep="."), with = F])
						set(testing.set.x_, NULL, chosen.t, predictions.testing[, paste(chosen.layers[chosen.t], chosen.t, sep="."), with = F])

						regressor <- train_(modelling.set.x_, modelling.set.y[[t]], tech, targets[[i]])
						set(predictions.modelling, NULL, paste(rlayer,t,sep="."), predict_(regressor, modelling.set.x_, tech, targets[[i]]))
						set(predictions.testing, NULL, paste(rlayer,t,sep="."), predict_(regressor, testing.set.x_, tech, targets[[i]]))
					}

					if(rlayer == as.numeric(convergence.layers_[nrow(convergence.layers_),t, with = F])) {
						max.layers.reached[t] <- TRUE
					}
				}

				addressing <- convergence.tracking_[rlayer+1]
				addressing <- which(addressing == TRUE)
				chosen.layers[addressing] <- rlayer

				rlayer <- rlayer + 1
			}

			write.csv(data.frame(id=modelling.names, predictions.modelling, check.names = F), paste0(output.dir.dstarst, "/output_logs/modelling_raw_logs/phi=", dstars.phi, "/", bases[i], "_", tech, "_modelling_predictions_fold", formatC(j, width=2, flag="0"), ".csv"), row.names = F)
			write.csv(data.frame(id=testing.names, predictions.testing, check.names = F), paste0(output.dir.dstarst, "/output_logs/testing_raw_logs/phi=", dstars.phi, "/", bases[i], "_", tech, "_testing_predictions_fold", formatC(j, width=2, flag="0"), ".csv"), row.names = F)

			final.predictions <- testing.set.y
			set(final.predictions, NULL, paste0(targets[[i]], ".pred"), predictions.testing[, paste(convergence.layers_[nrow(convergence.layers_),-1], targets[[i]],sep="."), with = F])

			write.csv(data.frame(id=testing.names, final.predictions, check.names = F), paste0(output.dir.dstarst, "/output_logs/testing_final_logs/phi=", dstars.phi, "/", bases[i], "_", tech, "_testing_final_predictions_fold", formatC(j, width=2, flag="0"), ".csv"), row.names = F)
		}
	}
}

#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.dstarst, "/output_logs"))
i <<- 1
lapply(bases, function(b) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RMSE.", targets[[i]]))
	performance.log <<- data.frame(dataset=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
							simplify = F), names.perf.log)), stringsAsFactors = FALSE)
	lapply(seq(0,1, 0.1), function(phi) {


		repetition.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
								simplify = F), names.perf.log), stringsAsFactors = FALSE)
		lapply(1:folds.num, function(k) {
			log <- read.csv(paste0(getwd(),"/testing_final_logs/phi=", phi, "/", b, "_", tech, "_testing_final_predictions_fold", formatC(k, width=2, flag="0"), ".csv"))
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
		performance.log[nrow(performance.log)+1, 1] <<- paste(b, tech, paste0("phi=",phi), sep="+")
		performance.log[nrow(performance.log), -1] <<- colMeans(repetition.log)
		write.csv(performance.log, paste0("../performance_DSTARST_", tech, "_", b, ".csv"), row.names = FALSE)
	})
	i <<- i + 1
})
setwd(actual.folder)
