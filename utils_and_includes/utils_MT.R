###############################################################
###################Evaluation metrics##########################
###############################################################
# average Correlation Coefficient
aCC <- function(log, targets) {
	acc <- 0.0
	for(t in targets) {
		t.pred <- paste0(t, ".pred")
		den <- sqrt(sum((log[,t] - mean(log[,t]))^2)*sum((log[,t.pred] - mean(log[,t.pred]))^2)) + 0.00001
		den <- ifelse(is.na(den), 0, den)
		num <- sum((log[,t] - mean(log[,t]))*(log[,t.pred] - mean(log[,t.pred]))) + 0.00001
		acc <- acc + num/den

	}
	return(acc/length(targets))
}

# Average Relative Error
ARE <- function(log, targets) {
	are <- 0.0
	for(t in targets) {
		t.pred <- paste0(t, ".pred")
		are <- are + (1/nrow(log))*(sum(abs(log[,t] - log[,t.pred])/ifelse(log[,t] == 0, 1, log[,t])))
	}
	return(are/length(targets))
}

# Mean Squared Error
MSE <- function(log, targets) {
	mse <- 0.0
	for(t in targets) {
		t.pred <- paste0(t, ".pred")
		mse <- mse + (1/nrow(log))*sum((log[,t] - log[,t.pred])^2)
	}
	return(mse)
}

# average Root-Mean-Squared Error
aRMSE <- function(log, targets) {
	armse <- 0.0
	for(t in targets) {
		t.pred <- paste0(t, ".pred")
		armse <- armse + sqrt(sum((log[,t] - log[,t.pred])^2)/nrow(log))
	}
	return(armse/length(targets))
}

# average Relative Root-Mean-Squared Error
aRRMSE <- function(log, targets) {
	arrmse <- 0.0
	for(t in targets) {
		t.pred <- paste0(t, ".pred")
		num <- sum((log[,t] - log[,t.pred])^2)
		den <- sum((log[,t] - mean(log[,t]))^2)
		den <- ifelse(den == 0, 1, den)

		arrmse <- arrmse + sqrt(num/den)
	}

	return(arrmse/length(targets))
}

# Root-Mean-Squared Error
RMSE <- function(actual, predicted) {
	return(sqrt(mean((actual-predicted)^2)))
}

CorrCoef <- function(actual, predicted) {
	return(sum((actual - mean(actual))*(predicted - mean(predicted)))/
					 (sqrt(sum((actual - mean(actual)^2))*sum((predicted - mean(predicted))^2))))
}

RRMSE <- function(actual, predicted) {
	num <- sum((actual - predicted)^2)
	den <- sum((actual - mean(actual))^2)
	den <- ifelse(den == 0, 1, den)
	return(sqrt(num/den))
}

######################################################################################################

train.test <- new.env()


train_ <- function(x, y, tech='svm', targets) {
	if(use.pls) {
		which.are.targets <- targets %in% names(x)
		filtered <- targets[which.are.targets]
		if(length(filtered) > 0) {
			tgts <- x[,filtered, with = FALSE]
			# x[, (filtered) := NULL]
			x <- as.matrix(x[,!(filtered), with = FALSE])
			train.test$pls.model <- plsr(y ~ x, ncomp = train.test$comp.limit, validation = "CV")
			determination <- pls::R2(train.test$pls.model)$val[,1,-1]
			train.test$max.comp <- which.max(determination)

			x.extracted <- as.data.table(cbind(x %*% coef(train.test$pls.model, 1:train.test$max.comp, intercept = F)[,1,], tgts))
			names(x.extracted)[1:train.test$max.comp] <- paste0("comp", 1:train.test$max.comp)
		} else {
			x <- as.matrix(x)
			train.test$pls.model <- plsr(y ~ x, ncomp = train.test$comp.limit, validation = "CV")
			determination <- pls::R2(train.test$pls.model)$val[,1,-1]
			train.test$max.comp <- which.max(determination)

			x.extracted <- data.table(x %*% coef(train.test$pls.model, 1:train.test$max.comp, intercept = F)[,1,])
			names(x.extracted) <- paste0("comp", 1:ncol(x.extracted))
		}
		x <- x.extracted
	}

	regressor <- switch(tech,
		svm={
			svm(x,y)
		},
		rf={
			randomForest(x,y)
		},
		gbm={
			gbm.fit(x, y, distribution = "gaussian", verbose = FALSE, n.minobsinnode = 5)
		},
		mlp={
			grid.mlp <- data.frame(layer1=5,layer2=0,layer3=0)
			train(x, y, trControl = trainControl(method="none"), method = "mlpML", tuneGrid=grid.mlp)$finalModel
		},
		pls={
			if(ncol(x) < train.test$max.comp)
				grid.pls <- data.frame(ncomp=ncol(x))
			else
				grid.pls <- data.frame(ncomp=train.test$max.comp)
			train(x, y, trControl = trainControl(method="none"), method = "pls", tuneGrid=grid.pls)$finalModel
		},
		xgboost={
			x <- as.matrix(x)
			xgboost(x, y, nrounds = 100, early_stopping_rounds = 3, base_score = mean(y), silent = 1, print_every_n = 500, save_period = NULL)
		},
		cart={
			grid.cart <- data.frame(cp=0.01)
			train(x, y, trControl = trainControl(method="none"), method = "rpart", tuneGrid=grid.cart)$finalModel
		},
		ridge={
			x <- as.matrix(x)
			lm.ridge(y ~ x)
		},
		lr={
		  data2fit <- cbind(x,y)
		  colnames(data2fit)[ncol(data2fit)] <- "LR_Target"
		  lr.form <- as.formula(paste0("LR_Target ~ ", paste(colnames(x), collapse = " + ")))
			lm(lr.form, data = data2fit)
		},
		parrf={
			foreach(ntree = c(70,70,70,70,70,70,80), .combine = combine, .packages = "randomForest") %dopar% randomForest(x, y, ntree = ntree)
		},
		ranger={
			# set(x, NULL, "Ranger_Target", y)
			x[, Ranger_Target := y]
			ranger.form <- as.formula(paste0("Ranger_Target ~ ", paste(names(x)[-ncol(x)], collapse = " + ")))
			reg <- ranger(ranger.form, data = x, verbose = F)
			x[, Ranger_Target := NULL]
			reg
		}
	)
	return(regressor)
}

predict_ <- function(regressor, new.data, tech = 'svm', targets) {
	if(use.pls) {
		which.are.targets <- targets %in% colnames(new.data)
		filtered <- targets[which.are.targets]
		if(length(filtered) > 0) {
			tgts <- new.data[,filtered, with = FALSE]
			x_ <- new.data[, !filtered, with = FALSE]
			x_ <- as.matrix(x_)
			x.extracted <- data.table(cbind(x_ %*% coef(train.test$pls.model, 1:train.test$max.comp, intercept = F)[,1,], tgts))
			names(x.extracted)[1:train.test$max.comp] <- paste0("comp", 1:train.test$max.comp)
		} else {
			x_ <- as.matrix(new.data)
			x.extracted <- data.table(x_ %*% coef(train.test$pls.model, 1:train.test$max.comp, intercept = F)[,1,])
			names(x.extracted) <- paste0("comp", 1:ncol(x.extracted))
		}
		new.data <- x.extracted
	}

	predicted <- switch(tech,
		svm={
			predict(regressor, new.data)
		},
		rf={
			predict(regressor, new.data)
		},
		gbm={
			predict(regressor, new.data, n.trees = 100)
		},
		mlp={
			predict(regressor, new.data)
		},
		pls={
			temp <- predict(regressor, new.data)
			rowMeans(temp[,1,])
		},
		xgboost={
			predict(regressor, as.matrix(new.data))
		},
		cart={
			predict(regressor, new.data)
		},
		ridge={
			scale(as.matrix(new.data), center = regressor$xm, scale = regressor$scales)%*%regressor$coef + regressor$ym
		},
		lr={
		  predict(regressor, new.data)
		},
		parrf={
			predict(regressor, new.data)
		},
		ranger={
			predict(regressor, new.data)$predictions
		}
	)

	return(predicted)
}
###############################################################
# Remove columns with an unique value
remove.unique <- function(dataset) {
	uniquelength <- sapply(dataset, function(x) length(unique(x)))
	return(subset(dataset, select = uniquelength > 1))
}

getTargetImportance <- function(targets, method = "rf_imp") {
	if(method == "rf_imp") {
		timportance <- matrix(nrow = ncol(targets), ncol = ncol(targets))
		for(t in seq(targets)) {
			rf.aux <- randomForest::randomForest(targets[, -t, with = F], targets[[t]], importance = TRUE)
			imp.aux <- randomForest::importance(rf.aux, type = 1)
			imp.aux[imp.aux <= 0] <- -Inf
			timportance[t, -t] <- imp.aux
		}
	} else {
		timportance <- abs(cor(targets, method = "pearson"))
	}

	diag(timportance) <- -Inf
  rownames(timportance) <- colnames(timportance) <- colnames(targets)

  return(timportance)
}

getTargetCorrelations <- function(targets, method = "rf_imp") {
	if(method == "rf_imp") {
		timportance <- matrix(nrow = ncol(targets), ncol = ncol(targets))
		for(t in seq(targets)) {
			rf.aux <- randomForest::randomForest(targets, targets[[t]], importance = TRUE)
			imp.aux <- randomForest::importance(rf.aux, type = 1)
			timportance[t,] <- imp.aux
		}
	} else {
		timportance <- abs(cor(targets, method = "pearson"))
	}

	rownames(timportance) <- colnames(timportance) <- colnames(targets)

	return(timportance)
}

getImportance <- function(x, y, method = "rf_imp") {
	if(method == "rf_imp") {
		timportance <- matrix(nrow = ncol(y), ncol = ncol(x))
		for(t in seq(ncol(y))) {
			rf.aux <- randomForest::randomForest(x, y[[t]], importance = TRUE)
			imp.aux <- randomForest::importance(rf.aux, type = 1)
			timportance[t,] <- imp.aux
		}
	} else {
		timportance <- abs(cor(x, y, method = "pearson"))
	}

	rownames(timportance) <- colnames(y)
	colnames(timportance) <- colnames(x)

	return(timportance)
}
