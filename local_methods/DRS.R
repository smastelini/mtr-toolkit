layers <- c(1:number.layers)
dir.create(paste0(output.dir.drs, "/output_logs/tracking_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.drs, "/output_logs/modelling_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.drs, "/output_logs/testing_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.drs, "/output_logs/layers_errors"), showWarnings = FALSE, recursive = TRUE)

targets <- list()
maxs <- list()
mins <- list()

for (i in 1:length(bases)) {
  set.seed(exp.random.seeds[i])
  print(bases[i])
  
	dataset <- read.csv(paste0(datasets.folder, "/", bases[i], ".csv"))
	dataset <- remove.unique(dataset)
	targets[[i]] <- colnames(dataset)[(ncol(dataset)-n.targets[i]+1):ncol(dataset)]

	dataset <- dataset[sample(nrow(dataset)),] #shuffle
	sample.names <- rownames(dataset)

  #Center and Scaling
	dataset <- as.data.table(dataset)
	invisible(dataset[, names(dataset) := lapply(.SD, as.numeric)])

	maxs[[i]] <- as.numeric(dataset[, lapply(.SD, max)])
	names(maxs[[i]]) <- colnames(dataset)
	mins[[i]] <- as.numeric(dataset[, lapply(.SD, min)])
	names(mins[[i]]) <- colnames(dataset)

	dataset <- as.data.table(scale(dataset, center = mins[[i]], scale = maxs[[i]] - mins[[i]]))

	l.folds <- round(nrow(dataset)/folds.num)

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

  X <- dataset[, !targets[[i]], with = FALSE]
	Y <- dataset[, targets[[i]], with = FALSE]

	# Cross validation Evaluation
	for(k in 1:folds.num) {
	  print(paste("Fold", k))

		if(folds.num == 1) {
			if(length(bases.teste) > 0) {
				modelling.idx <- 1:(init.bound-1)
				testing.idx <- init.bound:nrow(dataset)
			} else {
				testing.idx <- 1:nrow(dataset)
				modelling.idx <- testing.idx
			}
		} else {
			testing.idx <- ((k-1)*l.folds + 1):(ifelse(k==folds.num, nrow(dataset), k*l.folds))
			modelling.idx <- setdiff(1:nrow(dataset), testing.idx)
		}

		moX <- X[modelling.idx]
		moY <- Y[modelling.idx]
		teX <- X[testing.idx]
		teY <- Y[testing.idx]

		lt.folds <- round(nrow(moX)/n.folds.tracking)

		targets2rank <- targets[[i]]
		for(r in 1:length(targets2rank)) { # Ranking targets
		  print(paste("Target", r))

			l.errors <- matrix(data= 0, nrow=length(targets2rank), ncol=number.layers)
			rownames(l.errors) <- targets2rank
			colnames(l.errors) <- paste0("l", layers)

			print("Tracking layers")

			for(m in 1:n.folds.tracking) {
				if(n.folds.tracking == 1) {
					validation.idx <- 1:nrow(moX)
					training.idx <- validation.idx
				} else {
					validation.idx <- ((m-1)*lt.folds + 1):(ifelse(m==n.folds.tracking, nrow(moX), m*lt.folds))
					training.idx <- setdiff(1:nrow(moX), validation.idx)
				}

				trX <- moX[training.idx]
				trY <- moY[training.idx]

				vaX <- moX[validation.idx]
				vaY <- moY[validation.idx]

				p.tr <- moY[training.idx]
				p.va <- moY[validation.idx]

				for(l in layers) {
					for(t in targets2rank) {
					  if(showProgress){pb$tick()}else{}
						regressor <- train_(trX, trY[[t]], tech, targets2rank)

						p.tr[,(paste(t, "pred", l, sep=".")) := predict_(regressor, trX, tech, targets2rank)]
						p.va[,(paste(t, "pred", l, sep=".")) := predict_(regressor, vaX, tech, targets2rank)]
					}

					trX[,(paste(targets2rank, "pred", l, sep=".")) := p.tr[,paste(targets2rank, "pred", l, sep="."), with = FALSE]]
					vaX[,(paste(targets2rank, "pred", l, sep=".")) := p.va[,paste(targets2rank, "pred", l, sep="."), with = FALSE]]
				}

				write.csv(data.frame(id=sample.names[modelling.idx][training.idx], p.tr, check.names = F), paste0(output.dir.drs, "/output_logs/tracking_logs/raw_training_", bases[i], "_", tech, "_ef", formatC(k, width=2, flag="0"), "_T", r, "_tf", formatC(m, width=2, flag="0"), ".csv"), row.names = F)
				write.csv(data.frame(id=sample.names[modelling.idx][validation.idx], p.va, check.names = F), paste0(output.dir.drs, "/output_logs/tracking_logs/raw_validation_", bases[i], "_", tech, "_ef", formatC(k, width=2, flag="0"), "_T", r, "_tf", formatC(m, width=2, flag="0"), ".csv"), row.names = F)

				# Calculate for each folder the RMSE of each regression layer
				tr.errors <- matrix(nrow=length(targets2rank), ncol=number.layers)
				rownames(tr.errors) <- targets2rank
				colnames(tr.errors) <- paste0("l", layers)
				for(t in targets2rank) {
					tr.errors[t,] <- sapply(layers, function(l, preds, tar) {
						RMSE(preds[[tar]], preds[[paste(tar, "pred", l, sep=".")]])
					}, preds = p.va, tar = t)
				}

				write.csv(tr.errors, paste0(output.dir.drs, "/output_logs/layers_errors/layers_errors_", bases[i], "_", tech, "_ef", formatC(k, width=2, flag="0"), "_step", r, "_tf", formatC(m, width=2, flag="0"), ".csv"))
				l.errors <- l.errors + tr.errors
			}
			print("Done")

			#Normalize accounted errors
			l.errors <- l.errors/n.folds.tracking
			write.csv(l.errors, paste0(output.dir.drs, "/output_logs/layers_errors/final_errors_", bases[i], "_", tech, "_ef", formatC(k, width=2, flag="0"), "_step", r, ".csv"))

			# Find target/layer with the smallest RMSE
			e.idxs <- which(l.errors == min(l.errors), arr.ind=TRUE)

			# Deepening for the "easiest" target

			iset.mo <- X[modelling.idx]
			iset.te <- X[testing.idx]

			p.mo <- Y[modelling.idx]
			p.te <- Y[testing.idx]

			# If it isn't a ST model
			if(e.idxs[2] > 1) {
				for(l in 1:(e.idxs[2]-1)) {
					for(t in targets2rank) {
						regressor <- train_(iset.mo, moY[[t]], tech, targets2rank)

						p.mo[,(paste(t, "pred", l, sep=".")) := predict_(regressor, iset.mo, tech, targets2rank)]
						p.te[,(paste(t, "pred", l, sep=".")) := predict_(regressor, iset.te, tech, targets2rank)]
					}
					iset.mo[,(paste(targets2rank, "pred", l, sep=".")) := p.mo[,paste(targets2rank, "pred", l, sep="."), with = F]]
					iset.te[,(paste(targets2rank, "pred", l, sep=".")) := p.te[,paste(targets2rank, "pred", l, sep="."), with = F]]
				}
			}

			regressor <- train_(iset.mo, moY[[targets2rank[e.idxs[1]]]], tech, targets2rank)
			p.mo[,(paste(targets2rank[e.idxs[1]], "pred", l+1, sep=".")) := predict_(regressor, iset.mo, tech, targets2rank)]
			p.te[,(paste(targets2rank[e.idxs[1]], "pred", l+1, sep=".")) := predict_(regressor, iset.te, tech, targets2rank)]

			write.csv(data.frame(id=sample.names[modelling.idx], p.mo, check.names = F), paste0(output.dir.drs, "/output_logs/modelling_logs/raw_modelling_", bases[i], "_", tech, "_ef", formatC(k, width=2, flag="0"), "_T", r, ".csv"), row.names = F)
			write.csv(data.frame(id=sample.names[testing.idx], p.te, check.names = F), paste0(output.dir.drs, "/output_logs/modelling_logs/raw_testing_", bases[i], "_", tech, "_ef", formatC(k, width=2, flag="0"), "_T", r, ".csv"), row.names = F)

			p.mo <- p.mo[,c(targets2rank[e.idxs[1]], paste(targets2rank[e.idxs[1]], "pred", l+1, sep=".")), with = F]
			p.te <- p.te[,c(targets2rank[e.idxs[1]], paste(targets2rank[e.idxs[1]], "pred", l+1, sep=".")), with = F]

			names(p.mo) <- c(targets2rank[e.idxs[1]], paste0(targets2rank[e.idxs[1]], ".pred"))
			names(p.te) <- c(targets2rank[e.idxs[1]], paste0(targets2rank[e.idxs[1]], ".pred"))

			write.csv(data.frame(id=sample.names[modelling.idx], p.mo, check.names = F), paste0(output.dir.drs, "/output_logs/testing_logs/predictions_", bases[i], "_", tech, "_ef", formatC(k, width=2, flag="0"), "_T", r, ".csv"), row.names = F)
			write.csv(data.frame(id=sample.names[testing.idx], p.te, check.names = F), paste0(output.dir.drs, "/output_logs/testing_logs/predictions_", bases[i], "_", tech, "_ef", formatC(k, width=2, flag="0"), "_T", r, ".csv"), row.names = F)

			moX[,(targets2rank[e.idxs[1]]) := p.mo[,paste0(targets2rank[e.idxs[1]], ".pred"), with = F]]
			teX[,(targets2rank[e.idxs[1]]) := p.te[,paste0(targets2rank[e.idxs[1]], ".pred"), with = F]]

			targets2rank <- targets2rank[-e.idxs[1]]
		}
	}
}

actual.folder <- getwd()
setwd(paste0(output.dir.drs, "/output_logs"))
for (i in 1:length(bases)) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE")
	names.perf.log <- c(names.perf.log, paste("R2", targets[[i]], sep="."))
	names.perf.log <- c(names.perf.log, paste("RMSE", targets[[i]], sep="."))

	performance.log <- data.frame(algorithm=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
		simplify = F), names.perf.log)), stringsAsFactors = FALSE)
	folds.log <- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
		simplify = F), names.perf.log), stringsAsFactors = FALSE)

	for(k in 1:folds.num) {
		for (rank in 1:n.targets[i]) {
			logt <- read.csv(paste0(getwd(), "/testing_logs/predictions_", bases[i], "_", tech, "_ef", formatC(k, width=2, flag="0"), "_T", rank, ".csv"))
			logt <- logt[,-c(1)]
			save.the.date <- if(rank == 1) logt else cbind(save.the.date, logt)

			r <- (maxs[[i]][colnames(logt)[1]]-mins[[i]][colnames(logt)[1]])*logt[,colnames(logt)[1]] + mins[[i]][colnames(logt)[1]]
			p <- (maxs[[i]][colnames(logt)[1]]-mins[[i]][colnames(logt)[1]])*logt[,colnames(logt)[2]] + mins[[i]][colnames(logt)[1]]

			folds.log[nrow(folds.log)+1, paste0("RMSE.", colnames(logt)[1])] <- RMSE(r, p)
			folds.log[nrow(folds.log), paste0("R2.", colnames(logt)[1])] <- summary(lm(logt[,colnames(logt)[1]] ~ logt[,colnames(logt)[2]]))$r.squared
		}

		folds.log[nrow(folds.log), "aCC"] <- aCC(save.the.date, targets[[i]])
		folds.log[nrow(folds.log), "ARE"] <- ARE(save.the.date, targets[[i]])
		folds.log[nrow(folds.log), "MSE"] <- MSE(save.the.date, targets[[i]])
		folds.log[nrow(folds.log), "aRMSE"] <- aRMSE(save.the.date, targets[[i]])
		folds.log[nrow(folds.log), "aRRMSE"] <- aRRMSE(save.the.date, targets[[i]])
	}

	performance.log[nrow(performance.log)+1, 1] <- paste0("DRS+", tech)
	performance.log[nrow(performance.log), -1] <- colMeans(folds.log, na.rm = TRUE)
	write.csv(performance.log, paste0("../performance_DRS_", tech, "_", bases[i], "_L", number.layers, ".csv"), row.names = FALSE)
}
setwd(actual.folder)
