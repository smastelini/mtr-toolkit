epsilon <- 0.0

dir.create(paste0(output.dir.dstarsit, "/output_logs/tuning_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstarsit, "/output_logs/convergence_layers_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstarsit, "/output_logs/modelling_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstarsit, "/output_logs/testing_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstarsit, "/output_logs/testing_final_logs"), showWarnings = FALSE, recursive = TRUE)

targets <- list()
maxs <- list()
mins <- list()

for(i in 1:length(bases)) {
	dataset <- read.csv(paste0(datasets.folder, "/", bases[i], ".csv"))
	dataset <- remove.unique(dataset)
	dataset <- remove.unique(dataset)

	targets[[i]] <- colnames(dataset)[(ncol(dataset)-n.targets[i]+1):ncol(dataset)]

	#Center and Scaling
	dataset <- as.data.frame(sapply(dataset, function(x) as.numeric(x)))
	maxs[[i]] <- apply(dataset, 2, max)
	mins[[i]] <- apply(dataset, 2, min)
	dataset <- as.data.frame(scale(dataset, center = mins[[i]], scale = maxs[[i]] - mins[[i]]))

	dataset <- dataset[sample(nrow(dataset)),]
	sample.names <- rownames(dataset)

	len.fold.eval <- round(nrow(dataset))/folds.num

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

	x <- subset(dataset, select = !(colnames(dataset) %in% targets[[i]]))
	y <- subset(dataset, select = colnames(dataset) %in% targets[[i]])

	print(bases[i])

	for(j in 1:folds.num) {
		print(paste("Fold Training:", j))

		if(folds.num == 1) {
			if(length(bases.teste) > 0) {
				modelling.idx <- 1:(init.bound-1)
				testing.idx <- init.bound:nrow(dataset)
			} else {
				testing.idx <- as.numeric(rownames(dataset))
				modelling.idx <- testing.idx
			}
		} else {
			testing.idx <- as.numeric(rownames(dataset[((j-1)*len.fold.eval + 1):(ifelse(j==folds.num, nrow(dataset), j*len.fold.eval)),]))
			modelling.idx <- as.numeric(rownames(dataset[-testing.idx,]))
		}

		modelling.names <- sample.names[modelling.idx]

		modelling.set.x <- x[modelling.idx,]
		modelling.set.y <- y[modelling.idx,]

		############################ RF Importance calc ###################################
		rf.importance <- list()
		timportance <- matrix(nrow = length(targets[[i]]), ncol = length(targets[[i]]))

		for(k in 1:length(targets[[i]])) {
			rf.aux <- randomForest(modelling.set.y, modelling.set.y[,k], importance = TRUE)
			imp.aux <- importance(rf.aux, type = 1)
			imp.aux[imp.aux < 0] <- 0

			imp.aux[imp.aux < mean(imp.aux)] <- 0
			rf.importance[[targets[[i]][k]]] <- as.logical(imp.aux > 0)
			timportance[k,] <- imp.aux
		}

		write.csv(timportance, paste0(output.dir.dstarsit, "/output_logs/convergence_layers_logs/", bases[i], "_", tech, "_RF_importance_", formatC(j, width=2, flag="0"), ".csv"))

		###################################################################################

		rownames(modelling.set.x) <- 1:nrow(modelling.set.x)
		rownames(modelling.set.y) <- 1:nrow(modelling.set.y)

		testing.names <- sample.names[testing.idx]

		testing.set.x <- x[testing.idx,]
		testing.set.y <- y[testing.idx,]

		len.fold.tuning <- round(nrow(modelling.set.x)/n.folds.tracking)

		convergence.layers <- as.data.frame(setNames(replicate(length(targets[[i]]), numeric(length(n.folds.tracking)), simplify = F), targets[[i]]))
		convergence.layers <- cbind(1:n.folds.tracking, convergence.layers)
		colnames(convergence.layers)[1] <- "folds/layers"

		convergence.tracking <- as.data.frame(setNames(replicate(length(targets[[i]]), numeric(0), simplify = F), targets[[i]]))

		print(paste("Tuning"))

		# Cross validation
		for(k in 1:n.folds.tracking) {
			print(paste("Fold tuning", k))
			validation.idx <- as.numeric(rownames(modelling.set.x[((k-1)*len.fold.tuning + 1):(ifelse(k==n.folds.tracking, nrow(modelling.set.x), k*len.fold.tuning)),]))
			training.idx <- if(n.folds.tracking == 1) validation.idx else as.numeric(rownames(modelling.set.x[-validation.idx,]))

			x.training.tuning <- modelling.set.x[training.idx,]
			y.training.tuning <- modelling.set.y[training.idx,]

			x.validation.tuning <- modelling.set.x[validation.idx,]
			y.validation.tuning <- modelling.set.y[validation.idx,]

			predictions.training <- y.training.tuning
			predictions.validation <- y.validation.tuning

			# Training
			converged <- rep(FALSE, n.targets[i])
			names(converged) <- targets[[i]]

			# Uncorrelated targets are removed from the deep tracking process
			for(t in 1:length(targets[[i]])) {
				converged[t] <- all(!rf.importance[[t]][-t])
			}

			# TODO verify error
			error.validation <- rep(Inf, n.targets[i])
			names(error.validation) <- targets[[i]]


			# ST layer
			for(t in targets[[i]]) {
				regressor <- train_(x.training.tuning, y.training.tuning[,t], tech, targets[[i]])
				predictions.training[, paste(0,t,sep=".")] <- predict_(regressor, x.training.tuning, tech, targets[[i]])
				predictions.validation[, paste(0,t,sep=".")] <- predict_(regressor, x.validation.tuning, tech, targets[[i]])

				rmse.validation <- RMSE(y.validation.tuning[,t], predictions.validation[, paste(0,t,sep=".")])
				error.validation[t] <- rmse.validation
				convergence.layers[k,t] <- 0
			}
			convergence.tracking[nrow(convergence.tracking)+1,] <- as.numeric(!converged)
			#

			layer <- 1

			while(!all(converged)) {
				print(paste("Layer", layer))

				for(t in targets[[i]]) {
					if(!converged[t]) {
						tck.tra <- x.training.tuning
						tck.val <- x.validation.tuning

						chosen.t <- targets[[i]][rf.importance[[t]]]

						tck.tra[, chosen.t] <- predictions.training[, paste(convergence.layers[k,chosen.t], chosen.t,sep=".")]
						tck.val[, chosen.t] <- predictions.validation[, paste(convergence.layers[k,chosen.t], chosen.t,sep=".")]

						regressor <- train_(tck.tra, y.training.tuning[,t], tech, targets[[i]])
						predictions.training[, paste(layer,t,sep=".")] <- predict_(regressor, tck.tra, tech, targets[[i]])
						predictions.validation[, paste(layer,t,sep=".")] <- predict_(regressor, tck.val, tech, targets[[i]])

						rmse.validation <- RMSE(y.validation.tuning[,t], predictions.validation[, paste(layer,t,sep=".")])
						if(rmse.validation > error.validation[t] + epsilon | abs(rmse.validation - error.validation[t]) < dstars.delta) {
							converged[t] <- TRUE
						} else {
							converged[t] <- FALSE
							error.validation[t] <- rmse.validation
							convergence.layers[k,t] <- layer
						}
					}
				}

				if(layer + 1 > nrow(convergence.tracking)) {
					convergence.tracking[nrow(convergence.tracking)+1,] <- as.numeric(!converged)
				} else {
					convergence.tracking[layer+1,] <- convergence.tracking[layer+1,] + as.numeric(!converged)
				}

				# x.training.tuning[, targets[[i]]] <- predictions.training[, paste(convergence.layers[k,-1], targets[[i]],sep=".")]
				# x.validation.tuning[, targets[[i]]] <- predictions.validation[, paste(convergence.layers[k,-1], targets[[i]],sep=".")]
				layer <- layer + 1
			}

			browser()

			rownames(predictions.training) <- modelling.names[training.idx]
			rownames(predictions.validation) <- modelling.names[validation.idx]

			write.csv(predictions.training, paste0(output.dir.dstarsit, "/output_logs/tuning_raw_logs/", bases[i], "_", tech, "_training_predictions_EV_fold_", formatC(j, width=2, flag="0"), "_TN_fold", formatC(k, width=2, flag="0"), ".csv"))
			write.csv(predictions.validation, paste0(output.dir.dstarsit, "/output_logs/tuning_raw_logs/", bases[i], "_", tech, "_validation_predictions_EV_fold_", formatC(j, width=2, flag="0"), "_TN_fold", formatC(k, width=2, flag="0"), ".csv"))
		}

		rownames(convergence.tracking) <- 0:(nrow(convergence.tracking)-1)
		write.csv(convergence.tracking, paste0(output.dir.dstarsit, "/output_logs/convergence_layers_logs/", bases[i], "_", tech, "_convergence_accounting_EV_fold_", formatC(j, width=2, flag="0"), ".csv"))

		convergence.tracking <- convergence.tracking/n.folds.tracking
		convergence.layers_ <- convergence.layers

		# Test different phi values
		for(dstars.phi in seq(0,1, 0.1)) {
			dir.create(paste0(output.dir.dstarsit, "/output_logs/convergence_layers_logs/phi=",dstars.phi), showWarnings = FALSE, recursive = TRUE)
			dir.create(paste0(output.dir.dstarsit, "/output_logs/modelling_raw_logs/phi=",dstars.phi), showWarnings = FALSE, recursive = TRUE)
			dir.create(paste0(output.dir.dstarsit, "/output_logs/testing_raw_logs/phi=",dstars.phi), showWarnings = FALSE, recursive = TRUE)
			dir.create(paste0(output.dir.dstarsit, "/output_logs/testing_final_logs/phi=",dstars.phi), showWarnings = FALSE, recursive = TRUE)

			convergence.tracking_ <- as.data.frame(sapply(convergence.tracking, function(z, threshold) z >= threshold, threshold = dstars.phi))

			rownames(convergence.tracking_) <- 0:(nrow(convergence.tracking_)-1)
			write.csv(convergence.tracking_, paste0(output.dir.dstarsit, "/output_logs/convergence_layers_logs/phi=",dstars.phi, "/", bases[i], "_", tech, "_convergence_tracking_EV_fold_", formatC(j, width=2, flag="0"), ".csv"))

			convergence.layers_[nrow(convergence.layers_)+1,] <- c("modelling", as.numeric(sapply(convergence.tracking_, function(z) BBmisc::which.last(z) - 1)))
			write.csv(convergence.layers_, paste0(output.dir.dstarsit, "/output_logs/convergence_layers_logs/phi=", dstars.phi, "/", bases[i], "_", tech, "_convergence_layers_EV_fold_", formatC(j, width=2, flag="0"), ".csv"), row.names = F)

			print(paste("Fold", j, ", phi = ", dstars.phi, ", final modelling"))

			predictions.modelling <- modelling.set.y
			predictions.testing <- testing.set.y

			modelling.set.x_ <- modelling.set.x
			testing.set.x_ <- testing.set.x

			layer <- 0

			max.layers.reached <- rep(FALSE, n.targets[i])
			names(max.layers.reached) <- targets[[i]]

			chosen.layers <- rep(0, n.targets[i])

			while(!all(max.layers.reached)) {
				print(paste("Layer", layer))
				for(t in targets[[i]]) {
					if(convergence.tracking_[as.character(layer),t]) {
						regressor <- train_(modelling.set.x_, modelling.set.y[,t], tech, targets[[i]])
						predictions.modelling[, paste(layer,t,sep=".")] <- predict_(regressor, modelling.set.x_, tech, targets[[i]])
						predictions.testing[, paste(layer,t,sep=".")] <- predict_(regressor, testing.set.x_, tech, targets[[i]])
					}

					if(layer == as.numeric(convergence.layers_[nrow(convergence.layers_),t])) {
						max.layers.reached[t] <- TRUE
					}
				}

				addressing <- convergence.tracking_[as.character(layer),]
				addressing <- which(addressing == TRUE)
				chosen.layers[addressing] <- layer

				modelling.set.x_[, targets[[i]]] <- predictions.modelling[, paste(chosen.layers, targets[[i]],sep=".")]
				testing.set.x_[, targets[[i]]] <- predictions.testing[, paste(chosen.layers, targets[[i]],sep=".")]

				layer <- layer + 1
			}

			rownames(predictions.modelling) <- modelling.names
			rownames(predictions.testing) <- testing.names

			write.csv(predictions.modelling, paste0(output.dir.dstarsit, "/output_logs/modelling_raw_logs/phi=", dstars.phi, "/", bases[i], "_", tech, "_modelling_predictions_fold", formatC(j, width=2, flag="0"), ".csv"))
			write.csv(predictions.testing, paste0(output.dir.dstarsit, "/output_logs/testing_raw_logs/phi=", dstars.phi, "/", bases[i], "_", tech, "_testing_predictions_fold", formatC(j, width=2, flag="0"), ".csv"))

			final.predictions <- testing.set.y
			final.predictions[, paste0(targets[[i]], ".pred")] <- predictions.testing[, paste(convergence.layers_[nrow(convergence.layers_),-1], targets[[i]],sep=".")]

			rownames(final.predictions) <- testing.names
			write.csv(final.predictions, paste0(output.dir.dstarsit, "/output_logs/testing_final_logs/phi=", dstars.phi, "/", bases[i], "_", tech, "_testing_final_predictions_fold", formatC(j, width=2, flag="0"), ".csv"))
		}
	}
}

#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.dstarsit, "/output_logs"))
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
