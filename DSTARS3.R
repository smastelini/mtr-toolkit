dir.create(paste0(output.dir.dstars3, "/output_logs/tracking_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstars3, "/output_logs/convergence_layers_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstars3, "/output_logs/modelling_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstars3, "/output_logs/testing_raw_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.dstars3, "/output_logs/testing_final_logs"), showWarnings = FALSE, recursive = TRUE)
epsilon <- 0.0

targets <- list()

for(i in 1:length(bases)) {
	dataset <- read.csv(paste0(datasets.folder, "/", bases[i], ".csv"))
	dataset <- remove.unique(dataset)

	targets[[i]] <- colnames(dataset)[(ncol(dataset)-n.targets[i]+1):ncol(dataset)]

	#Center e Scaling
	dataset <- as.data.frame(sapply(dataset, function(x) as.numeric(x)))
	maxs <- apply(dataset, 2, max)
	mins <- apply(dataset, 2, min)
	dataset <- as.data.frame(scale(dataset, center = mins, scale = maxs - mins))

	dataset <- dataset[sample(nrow(dataset)),]
	sample.names <- rownames(dataset)

	len.fold.eval <- round(nrow(dataset))/folds.num

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

	x <- subset(dataset, select= !(colnames(dataset) %in% targets[[i]]))
	y <- subset(dataset, select= colnames(dataset) %in% targets[[i]])

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

		rownames(modelling.set.x) <- 1:nrow(modelling.set.x)
		rownames(modelling.set.y) <- 1:nrow(modelling.set.y)

		testing.names <- sample.names[testing.idx]

		testing.set.x <- x[testing.idx,]
		testing.set.y <- y[testing.idx,]

		len.fold.tracking <- round(nrow(modelling.set.x)/dstars.n.folds.tracking)

		convergence.layers <- as.data.frame(setNames(replicate(length(targets[[i]]), numeric(length(dstars.n.folds.tracking)), simplify = F), targets[[i]]))
		convergence.layers <- cbind(1:dstars.n.folds.tracking, convergence.layers)
		colnames(convergence.layers)[1] <- "folds/layers"

		convergence.tracking <- as.data.frame(setNames(replicate(length(targets[[i]]), numeric(0), simplify = F), targets[[i]]))

		print(paste("Tracking"))

		# Cross validation
		for(k in 1:dstars.n.folds.tracking) {
			print(paste("Fold tracking", k))
			validation.idx <- as.numeric(rownames(modelling.set.x[((k-1)*len.fold.tracking + 1):(ifelse(k==dstars.n.folds.tracking, nrow(modelling.set.x), k*len.fold.tracking)),]))
			training.idx <- if(dstars.n.folds.tracking == 1) validation.idx else as.numeric(rownames(modelling.set.x[-validation.idx,]))

			x.training.tracking <- modelling.set.x[training.idx,]
			y.training.tracking <- modelling.set.y[training.idx,]

			x.validation.tracking <- modelling.set.x[validation.idx,]
			y.validation.tracking <- modelling.set.y[validation.idx,]

			predictions.training <- y.training.tracking
			predictions.validation <- y.validation.tracking

			# Training
			converged <- rep(FALSE, n.targets[i])
			names(converged) <- targets[[i]]

			error.validation <- rep(1, n.targets[i])
			names(error.validation) <- targets[[i]]

			layer <- 0

			while(!all(converged)) {
				print(paste("Layer", layer))
				position <- 1
				for(t in targets[[i]]) {
					regressor <- train_(x.training.tracking, y.training.tracking[,t], tech, targets[[i]])
					predictions.training[, paste(layer,t,sep=".")] <- predict_(regressor, x.training.tracking, tech, targets[[i]])
					predictions.validation[, paste(layer,t,sep=".")] <- predict_(regressor, x.validation.tracking, tech, targets[[i]])

					rrmse.validation <- RRMSE(y.validation.tracking[,t], predictions.validation[, paste(layer,t,sep=".")])
					if(rrmse.validation > error.validation[t] + epsilon | abs(rrmse.validation - error.validation[t]) < dstars.delta) {
						converged[t] <- TRUE
					} else {
						err.diff <- abs(rrmse.validation - error.validation[t])
					  if(layer + 1 > nrow(convergence.tracking)) {
					    convergence.tracking[nrow(convergence.tracking)+1,] <- rep(0, length(targets[[i]]))
					    convergence.tracking[layer+1,position] <- err.diff
					  } else {
					    convergence.tracking[layer+1,position] <- convergence.tracking[layer+1,position] + err.diff
					  }

						converged[t] <- FALSE
						error.validation[t] <- rrmse.validation
						convergence.layers[k,t] <- layer
					}
					position <- position + 1
				}

				x.training.tracking[, targets[[i]]] <- predictions.training[, paste(convergence.layers[k,-1], targets[[i]],sep=".")]
				x.validation.tracking[, targets[[i]]] <- predictions.validation[, paste(convergence.layers[k,-1], targets[[i]],sep=".")]

				layer <- layer + 1
			}

			rownames(predictions.training) <- modelling.names[training.idx]
			rownames(predictions.validation) <- modelling.names[validation.idx]

			#print log de layers
			write.csv(predictions.training, paste0(output.dir.dstars3, "/output_logs/tracking_raw_logs/", bases[i], "_", tech, "_training_predictions_EV_fold_", formatC(j, width=2, flag="0"), "_TN_fold", formatC(k, width=2, flag="0"), ".csv"))
			write.csv(predictions.validation, paste0(output.dir.dstars3, "/output_logs/tracking_raw_logs/", bases[i], "_", tech, "_validation_predictions_EV_fold_", formatC(j, width=2, flag="0"), "_TN_fold", formatC(k, width=2, flag="0"), ".csv"))
		}

		# E se não houve melhorias em nenhum ST?
		if(nrow(convergence.tracking) <= 1) {
		  aux.strut <- data.frame(matrix(0,nrow=2,ncol=n.targets[i]))
		  colnames(aux.strut) <- targets[[i]]
			convergence.tracking <- rbind(convergence.tracking, aux.strut)
		}

		rownames(convergence.tracking) <- 0:(nrow(convergence.tracking)-1)
		write.csv(convergence.tracking, paste0(output.dir.dstars3, "/output_logs/convergence_layers_logs/", bases[i], "_", tech, "_convergence_accounting_EV_fold_", formatC(j, width=2, flag="0"), ".csv"))

		convergence.tracking <- apply(convergence.tracking, 2, function(zeta) zeta*(zeta>0))

		#Normaliza erro
 		convergence.tracking <- apply(convergence.tracking, 2, function(zeta) {
			s.zeta <- sum(zeta)
 		  zeta/ifelse(s.zeta == 0, 1, s.zeta)
 		})

		s.zero <- which(apply(convergence.tracking, 2, function(zeta) sum(zeta) == 0) == TRUE)
		if(length(s.zero) > 0)
			convergence.tracking[1,s.zero] <- 1

		convergence.tracking <- as.data.frame(apply(convergence.tracking, 2, function(z, threshold) z >= threshold, threshold = dstars.phi))

		rownames(convergence.tracking) <- 0:(nrow(convergence.tracking)-1)
		write.csv(convergence.tracking, paste0(output.dir.dstars3, "/output_logs/convergence_layers_logs/", bases[i], "_", tech, "_convergence_tracking_EV_fold_", formatC(j, width=2, flag="0"), ".csv"))
    last.used.model <- as.numeric(sapply(convergence.tracking, function(z) BBmisc::which.last(z) - 1))
    #Força a utilização de modelo ST mesmo em caso de não haver ganho em relação ao output da media aritmetica do target
    last.used.model[is.na(last.used.model)] <- 0

		convergence.layers[nrow(convergence.layers)+1,] <- c("modelling", last.used.model)
		write.csv(convergence.layers, paste0(output.dir.dstars3, "/output_logs/convergence_layers_logs/", bases[i], "_", tech, "_convergence_layers_EV_fold_", formatC(j, width=2, flag="0"), ".csv"), row.names = F)

		print(paste("Fold", j, "final modelling"))

		predictions.modelling <- modelling.set.y
		predictions.testing <- testing.set.y

		layer <- 0
		max.layers.reached <- rep(FALSE, n.targets[i])
		names(max.layers.reached) <- targets[[i]]

		chosen.layers <- rep(NA, n.targets[i])

		layer <- 0
		while(!all(max.layers.reached)) {
		  print(paste("Layer", layer))
			addressing <- as.logical(convergence.tracking[as.character(layer),])

			for(t in targets[[i]][addressing]) {
					regressor <- train_(modelling.set.x, modelling.set.y[,t], tech, targets[[i]])
					predictions.modelling[, paste(layer,t,sep=".")] <- predict_(regressor, modelling.set.x, tech, targets[[i]])
					predictions.testing[, paste(layer,t,sep=".")] <- predict_(regressor, testing.set.x, tech, targets[[i]])

				if(layer == as.numeric(convergence.layers[nrow(convergence.layers),t])) {
					max.layers.reached[t] <- TRUE
				}
			}

		  addressing <- which(addressing == TRUE)
		  chosen.layers[addressing] <- layer

			n.na <- !is.na(chosen.layers)
		  modelling.set.x[, targets[[i]][n.na]] <- predictions.modelling[, paste(chosen.layers[n.na], targets[[i]][n.na],sep=".")]
		  testing.set.x[, targets[[i]][n.na]] <- predictions.testing[, paste(chosen.layers[n.na], targets[[i]][n.na],sep=".")]

			layer <- layer + 1
		}

		rownames(predictions.modelling) <- modelling.names
		rownames(predictions.testing) <- testing.names

		write.csv(predictions.modelling, paste0(output.dir.dstars3, "/output_logs/modelling_raw_logs/", bases[i], "_", tech, "_modelling_predictions_fold", formatC(j, width=2, flag="0"), ".csv"))
		write.csv(predictions.testing, paste0(output.dir.dstars3, "/output_logs/testing_raw_logs/", bases[i], "_", tech, "_testing_predictions_fold", formatC(j, width=2, flag="0"), ".csv"))

		final.predictions <- testing.set.y
		final.predictions[, paste0(targets[[i]], ".pred")] <- predictions.testing[, paste(convergence.layers[nrow(convergence.layers),-1], targets[[i]],sep=".")]

		rownames(final.predictions) <- testing.names
		write.csv(final.predictions, paste0(output.dir.dstars3, "/output_logs/testing_final_logs/", bases[i], "_", tech, "_testing_final_predictions_fold", formatC(j, width=2, flag="0"), ".csv"))
	}
}

#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.dstars3, "/output_logs"))
i <<- 1
lapply(bases, function(b) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RMSE.", targets[[i]]))
	performance.log <<- data.frame(dataset=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
							simplify = F), names.perf.log)), stringsAsFactors = FALSE)


	repetition.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
							simplify = F), names.perf.log), stringsAsFactors = FALSE)
	lapply(1:folds.num, function(k) {
		log <- read.csv(paste0(getwd(),"/testing_final_logs/", b, "_", tech, "_testing_final_predictions_fold", formatC(k, width=2, flag="0"), ".csv"))
		repetition.log[nrow(repetition.log)+1, "aCC"] <<- aCC(log, targets[[i]])
		repetition.log[nrow(repetition.log), "ARE"] <<- ARE(log, targets[[i]])
		repetition.log[nrow(repetition.log), "MSE"] <<- MSE(log, targets[[i]])
		repetition.log[nrow(repetition.log), "aRMSE"] <<- aRMSE(log, targets[[i]])
		repetition.log[nrow(repetition.log), "aRRMSE"] <<- aRRMSE(log, targets[[i]])

		# targets
		for(t in targets[[i]]) {
			repetition.log[nrow(repetition.log), paste0("R2.", t)] <<- summary(lm(log[,t] ~ log[, paste0(t, ".pred")]))$r.squared
			repetition.log[nrow(repetition.log), paste0("RMSE.", t)] <<- RMSE(log[,t], log[, paste0(t, ".pred")])
		}

	})
	performance.log[nrow(performance.log)+1, 1] <<- paste(b, tech, sep="+")
	performance.log[nrow(performance.log), -1] <<- colMeans(repetition.log)
	write.csv(performance.log, paste0("../performance_DSTARS3_", tech, "_", b, ".csv"), row.names = FALSE)
	i <<- i + 1
})
setwd(actual.folder)
