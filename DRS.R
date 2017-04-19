# dir.create(paste0(output.dir.drs, "/prediction_logs/", tech), showWarnings = FALSE, recursive = TRUE)

dir.create(paste0(output.dir.drs, "/output_logs/internal_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.drs, "/output_logs/modelling_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.drs, "/output_logs/testing_logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.drs, "/output_logs/layers_errors"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.drs, "/output_logs/EV_raw"), showWarnings = FALSE, recursive = TRUE)

#number.layers<-10

targets <- list()
maxs <- list()
mins <- list()

for (b in 1:length(bases)) { ### para b bases
	print(bases[b])
	dataset <- read.csv(paste(datasets.folder, "/", bases[b], ".csv", sep=""))
	dataset <- remove.unique(dataset)
	number.columns <- ncol(dataset)
	targets[[b]] <- colnames(dataset)[(number.columns-n.targets[b]+1):number.columns]

	dataset <- dataset[sample(nrow(dataset)),] #shuffle
	sample.names <- rownames(dataset)

	dataset <- as.data.frame(sapply(dataset,function(x) as.numeric(x)))
	maxs[[b]] <- apply(dataset, 2, max)
	mins[[b]] <- apply(dataset, 2, min)
	dataset <- as.data.frame(scale(dataset, center = mins[[b]], scale = maxs[[b]] - mins[[b]]))


	len.fold <- round(nrow(dataset)/folds.num)

	######Usar um testing set
	if(length(bases.teste) > 0 && folds.num == 1) {
		dataset.teste <- read.csv(paste0(datasets.folder, "/", bases.teste[b], ".csv"))
		dataset.teste <- as.data.frame(sapply(dataset.teste, function(x) as.numeric(x)))
		dataset.teste <- as.data.frame(scale(dataset.teste, center = mins, scale = maxs - mins))
		init.bound <- nrow(dataset) + 1
		dataset <- rbind(dataset, dataset.teste)
		sample.names <- c(sample.names, rownames(dataset.teste))
	}
	rownames(dataset) <- 1:nrow(dataset)
	#######

	melhores.metricas<- as.data.frame(setNames(replicate(n.targets[b], numeric(1), simplify = F),
		paste(targets[[b]], "R2", sep=".")), check.names=FALSE)


	x1 <- dataset[,1:(number.columns-n.targets[b])]
	y1 <- dataset[,targets[[b]]]

	####começa o cross validation que separa para teste
	for(out in 1:folds.num){
		print(paste("Fold", out))

		if(folds.num == 1) {
			if(length(bases.teste) > 0) {
				train.out.idx <- 1:(init.bound-1)
				test.out.idx <- init.bound:nrow(dataset)
			} else {
				test.out.idx <- as.numeric(rownames(dataset))
				train.out.idx <- test.out.idx
			}
		} else {
			test.out.idx <- as.numeric(rownames(dataset[((out-1)*len.fold + 1):(ifelse(out==folds.num, nrow(dataset), out*len.fold)),]))
			train.out.idx <- as.numeric(rownames(dataset[-test.out.idx,]))
		}

		modelling.names <- sample.names[train.out.idx]

		x <- x1[train.out.idx,]
		y <- y1[train.out.idx,]

		rownames(x) <- 1:nrow(x)
		rownames(y) <- 1:nrow(y)

		testing.names <- sample.names[test.out.idx]

		x.teste <- x1[test.out.idx,]
		y.teste <- y1[test.out.idx,]
		len.fold.tuning <- round(nrow(x)/folds.num)


		#############loop para recortes, ao final de cada loop o target que obteve o menor rmse sera "removido"
		index.t <- 1
		for(paradas in 1:n.targets[b]){
			print(paste("Target",paradas))

			col.names.targets <- c()
			for(t in targets[[b]]) {
				col.names.targets <- c(col.names.targets, t)
				for(l in layers){
					col.names.targets <- c(col.names.targets, paste(t, ".pred.", l, sep = ""))
				}
			}
			print("Begin tracking")

			#####começa o cross validation "interno"
			for(k in 1:folds.num) {
				test.idx <- as.numeric(rownames(x[(ifelse(((k-1)*len.fold.tuning + 1)>nrow(x), nrow(x), (k-1)*len.fold.tuning + 1)):(ifelse(k==folds.num, nrow(x), k*len.fold.tuning)),]))
				train.idx <- if(folds.num == 1) test.idx else as.numeric(rownames(x[-test.idx,]))

				x.train <- x[train.idx,]
				y.train <- as.data.frame(y[train.idx,])
				colnames(y.train) <- (targets[[b]])

				x.validit <- x[test.idx,]
				y.validit <- as.data.frame(y[test.idx,])
				colnames(y.validit) <- (targets[[b]])

				predictions.training <-	y.train
				predictions.validation <- y.validit


				for (a in layers) {
					for(t in targets[[b]]) {
						# print(t)
						# training.output <- as.data.frame(y.train[,t])

						regressor <- train_(x.train, y.train[,t], tech, targets[[b]])

						predictions.training[, paste(t,"pred",a,sep=".")] <- predict_(regressor, x.train, tech, targets[[b]])
						predictions.validation[, paste(t,"pred",a,sep=".")] <- predict_(regressor, x.validit, tech, targets[[b]])
					}

					if(a==1){
						predictions.training <- as.data.frame(predictions.training[,-c(1:length(targets[[b]]))])
						predictions.validation <- as.data.frame(predictions.validation[,-c(1:length(targets[[b]]))])
						if(length(targets[[b]])==1){
							colnames(predictions.training) <- paste(t,"pred",a,sep=".")
							colnames(predictions.validation) <- paste(t,"pred",a,sep=".")
						}
					}
					x.train <- cbind(x.train, predictions.training[,c(paste(targets[[b]],"pred",a,sep="."))])
					colnames(x.train)[(ncol(x.train)-length(targets[[b]])+1):ncol(x.train)] <- c(paste(targets[[b]],"pred",a,sep="."))
					x.validit <- cbind(x.validit, predictions.validation[,c(paste(targets[[b]],"pred",a,sep="."))])
					colnames(x.validit)[(ncol(x.validit)-length(targets[[b]])+1):ncol(x.validit)] <- c(paste(targets[[b]],"pred",a,sep="."))
				}

				rownames(predictions.training) <- modelling.names[train.idx]
				rownames(predictions.validation) <- modelling.names[test.idx]

				predictions.training <- cbind(as.data.frame(y.train), predictions.training)
				predictions.validation <- cbind(as.data.frame(y.validit), predictions.validation)

				#print log de layers
				write.csv(predictions.training, paste0(output.dir.drs, "/output_logs/internal_logs/", bases[b], "_", tech, "_training_predictions_EV_fold_", formatC(out, width=2, flag="0"), "_step", paradas, "_TN_fold", formatC(k, width=2, flag="0"), ".csv"))
				write.csv(predictions.validation, paste0(output.dir.drs, "/output_logs/internal_logs/", bases[b], "_", tech, "_validation_predictions_EV_fold_", formatC(out, width=2, flag="0"), "_step", paradas, "_TN_fold", formatC(k, width=2, flag="0"), ".csv"))

			} #fim cross interno
			print("End tracking")

			#avalia 10 folds internos a cada loop externo
			actual.folder <- getwd()
			setwd(paste(output.dir.drs, "/output_logs/internal_logs/", sep = ""))

			names.perf.log <- c()
			for (l in layers){
				names.perf.log <- c(names.perf.log, paste("RMSE", targets[[b]], l, sep="."))
			}
			performance.log <- data.frame(algorithm=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
														simplify = F), names.perf.log)), stringsAsFactors = FALSE)
			folds.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
														simplify = F), names.perf.log), stringsAsFactors = FALSE)
			lapply(1:folds.num, function(k) {
				log <- read.csv(paste(getwd(),"/", bases[b], "_", tech, paste("_validation_predictions_EV_fold_", formatC(out, width=2, flag="0"), "_step", paradas, "_TN_fold", formatC(k, width=2, flag="0"), sep=""), ".csv", sep=""), header=TRUE, check.names = FALSE)
					# targets
					for (l in layers){
						for(t in targets[[b]]) {
							folds.log[k, paste("RMSE.", t, ".", l, sep = "")] <<- RMSE(log[,t], log[, paste(t, "pred", l, sep=".")])
						}
					}
			})
			performance.log[nrow(performance.log)+1, 1] <- tech
			performance.log[nrow(performance.log), -1] <- colMeans(folds.log)

			write.csv(performance.log, paste("../layers_errors/DRS_error_", bases[b], "_", tech, "_EV_", out, "_step", paradas, ".csv", sep = ""), row.names = FALSE)

			setwd(actual.folder)

			##obter minimos rmse's e maximos r2's para cada target nas suas médias de r2 e rmse
			mins.maxs <- data.frame("Target"=numeric(length(targets[[b]])), "Min.Value.Target"=numeric(length(targets[[b]])), "Min.Layer"=numeric(length(targets[[b]])))

			for (t in 1:length(targets[[b]])){
				mins.maxs[t,"Target"]<-targets[[b]][t]
				mins.maxs[t,"Min.Value.Target"] <- performance.log[, paste("RMSE", targets[[b]][t], 1, sep = ".")]
				mins.maxs[t,"Min.Layer"] <- 1
			}

			min.global <- mins.maxs[1,"Min.Value.Target"]
			pos.min.global <- 1


			for (t in 1:length(targets[[b]])){
				for (l in 2:number.layers){
					if(performance.log[, paste("RMSE", targets[[b]][t], l, sep = ".")] < mins.maxs[t,"Min.Value.Target"]){
						mins.maxs[t,"Min.Value.Target"] <- performance.log[, paste("RMSE", targets[[b]][t], l, sep = ".")]
						mins.maxs[t,"Min.Layer"] <- l
						if(mins.maxs[t,"Min.Value.Target"] < min.global){
							min.global <- mins.maxs[t,"Min.Value.Target"]
							pos.min.global <- l
						}
					}
				}
			}

			write.csv(mins.maxs, paste0(output.dir.drs, "/output_logs/layers_errors/", bases[b], "_", tech, "_melhores_camadas", formatC(out, width=2, flag="0"), "_step", paradas, ".csv"), row.names = F)

			#####
			##2a etapa- agrega o "melhor target" ao input###

			temporaria <- which.min(mins.maxs[,"Min.Value.Target"])
			maxima.camada <- mins.maxs[temporaria, "Min.Layer"]

			input <- x
			input.tes <- x.teste

			predictions.modelling <- as.data.frame(y)
			predictions.testing <- as.data.frame(y.teste)

			for (a in 1:maxima.camada){
				for(t in targets[[b]]){
					regressor <- train_(input, y[,t], tech, targets[[b]])

					predictions.modelling[, paste(t, "pred", a,sep=".")] <- predict_(regressor, input, tech, targets[[b]])
					predictions.testing[, paste(t,"pred", a, sep=".")] <- predict_(regressor, input.tes, tech, targets[[b]])
				}

				if(a==1){
					predictions.modelling <- as.data.frame(predictions.modelling[,-c(1:length(targets[[b]]))])
					predictions.testing <- as.data.frame(predictions.testing[,-c(1:length(targets[[b]]))])
					if(length(targets[[b]])==1){
						colnames(predictions.modelling)<-paste(t,"pred", a, sep=".")
						colnames(predictions.testing)<-paste(t,"pred", a, sep=".")
					}
				}
				input <- cbind(input , predictions.modelling[,paste(targets[[b]], "pred", a, sep=".")])
				colnames(input)[(ncol(input)-length(targets[[b]])+1):ncol(input)] <- c(paste(targets[[b]],"pred",a,sep="."))
				input.tes <- cbind(input.tes , predictions.testing[,paste(targets[[b]], "pred", a, sep=".")])
				colnames(input.tes)[(ncol(input.tes)-length(targets[[b]])+1):ncol(input.tes)] <- c(paste(targets[[b]],"pred",a,sep="."))
			}

			predictions.modelling <- cbind(y,predictions.modelling)
			predictions.testing <- cbind(y.teste,predictions.testing)

			rownames(predictions.modelling) <- modelling.names
			rownames(predictions.testing) <- testing.names

			write.csv(predictions.modelling, paste0(output.dir.drs, "/output_logs/EV_raw/", bases[b], "_", tech, "_modelling_predictions_fold", formatC(out, width=2, flag="0"), "_step", paradas, ".csv"))
			write.csv(predictions.testing, paste0(output.dir.drs, "/output_logs/EV_raw/", bases[b], "_", tech, "_testing_predictions_fold", formatC(out, width=2, flag="0"), "_step", paradas, ".csv"))

			predictions.modelling	<- predictions.modelling[,c(targets[[b]][temporaria],paste(targets[[b]][temporaria], "pred", maxima.camada, sep = "."))]
			predictions.testing	<- predictions.testing[,c(targets[[b]][temporaria],paste(targets[[b]][temporaria], "pred", maxima.camada, sep = "."))]

			colnames(predictions.modelling) <- c(targets[[b]][temporaria], paste0(targets[[b]][temporaria], ".pred"))
			colnames(predictions.testing) <- c(targets[[b]][temporaria], paste0(targets[[b]][temporaria], ".pred"))

			write.csv(predictions.modelling, paste0(output.dir.drs, "/output_logs/modelling_logs/", bases[b], "_", tech, "_T", index.t,"_modelling_predictions_fold", formatC(out, width=2, flag="0"), ".csv"))
			write.csv(predictions.testing, paste0(output.dir.drs, "/output_logs/testing_logs/", bases[b], "_", tech, "_T", index.t,"_testing_predictions_fold", formatC(out, width=2, flag="0"), ".csv"))

			index.t <- index.t + 1

			x <- cbind(x, predictions.modelling[,c(paste(targets[[b]][temporaria], "pred", sep = "."))])
			colnames(x)[ncol(x)] <- targets[[b]][temporaria]
			x.teste <- cbind(x.teste, predictions.testing[,c(paste(targets[[b]][temporaria], "pred", sep = "."))])
			colnames(x.teste)[ncol(x)] <- targets[[b]][temporaria]
			targets[[b]] <- targets[[b]][-temporaria]

			y <- as.data.frame(y[,c(targets[[b]])])
			y.teste <- as.data.frame(y.teste[,c(targets[[b]])])
			colnames(y) <- targets[[b]]
			colnames(y.teste) <- targets[[b]]

			##########

			if(length(targets[[b]])==0){
				targets[[b]]<-colnames(dataset)[(number.columns-n.targets[b]+1):number.columns]
				break
			}
		}#fim paradas
	}#fim cross externo
} #termina loop das bases

actual.folder <- getwd()
setwd(paste(output.dir.drs, "/output_logs", sep = ""))
for (b in 1:length(bases)) {
	#####Performance metrics com o out######
	names.perf.log2 <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE")
	names.perf.log2 <- c(names.perf.log2, paste("R2", targets[[b]], sep="."))
	names.perf.log2 <- c(names.perf.log2, paste("RMSE", targets[[b]], sep="."))

	performance.log2 <- data.frame(algorithm=character(0), as.data.frame(setNames(replicate(length(names.perf.log2),numeric(0),
		simplify = F), names.perf.log2)), stringsAsFactors = FALSE)
	folds.log2 <- as.data.frame(setNames(replicate(length(names.perf.log2),numeric(0),
		simplify = F), names.perf.log2), stringsAsFactors = FALSE)

	for(k in 1:folds.num) {
		for (aleats in 1:n.targets[b]){
			logt <- read.csv(paste(getwd(),"/testing_logs/", bases[b], "_", tech, "_T", aleats, "_testing_predictions", paste("_fold", formatC(k, width=2, flag="0"), sep=""),".csv", sep = ""), header=TRUE)
			logt <- logt[,-c(1)]
			# colnames(logt)<-c(targets[[b]][aleats], paste(targets[[b]][aleats],"pred", sep="."))

			save.the.date <- if(aleats == 1) logt else cbind(save.the.date, logt)

			r <- (maxs[[b]][t]-mins[[b]][t])*logt[,colnames(logt)[1]] + mins[[b]][t]
			p <- (maxs[[b]][t]-mins[[b]][t])*logt[,colnames(logt)[2]] + mins[[b]][t]

			folds.log2[nrow(folds.log2)+1, paste("RMSE.", colnames(logt)[1], sep = "")]<- RMSE(r, p)
			folds.log2[nrow(folds.log2), paste("R2.", targets[[b]][aleats], sep = "")] <- summary(lm(logt[,colnames(logt)[1]] ~ logt[, colnames(logt)[2]]))$r.squared
		}

		folds.log2[nrow(folds.log2), "aCC"] <- aCC(save.the.date, targets[[b]])
		folds.log2[nrow(folds.log2), "ARE"] <- ARE(save.the.date, targets[[b]])
		folds.log2[nrow(folds.log2), "MSE"] <- MSE(save.the.date, targets[[b]])
		folds.log2[nrow(folds.log2), "aRMSE"] <- aRMSE(save.the.date, targets[[b]])
		folds.log2[nrow(folds.log2), "aRRMSE"] <- aRRMSE(save.the.date, targets[[b]])
	}

	performance.log2[nrow(performance.log2)+1, 1] <- paste0("DRS+", tech)
	performance.log2[nrow(performance.log2), -1] <- colMeans(folds.log2, na.rm = TRUE)
	write.csv(performance.log2, paste("../performance_DRS_", tech, "_", bases[b], "_L", number.layers, ".csv", sep = ""), row.names = FALSE)
}
setwd(actual.folder)
