###############################################################
###################Métricas de Avaliação#######################
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
		which.are.targets <- targets %in% colnames(x)
		filtered <- targets[which.are.targets]
		if(length(filtered) > 0) {
			tgts <- x[,filtered]
			x <- x[, !(colnames(x) %in% filtered)]
			x <- as.matrix(x)
			train.test$pls.model <- plsr(y ~ x, ncomp = train.test$comp.limit, validation = "CV")
			determination <- R2(train.test$pls.model)$val[,1,-1]
			train.test$max.comp <- which.max(determination)

			x.extracted <- cbind(x %*% coef(train.test$pls.model, 1:train.test$max.comp, intercept = F)[,1,], tgts)

		} else {
			x <- as.matrix(x)
			train.test$pls.model <- plsr(y ~ x, ncomp = train.test$comp.limit, validation = "CV")
			determination <- R2(train.test$pls.model)$val[,1,-1]
			train.test$max.comp <- which.max(determination)

			x.extracted <- as.data.frame(x %*% coef(train.test$pls.model, 1:train.test$max.comp, intercept = F)[,1,])
		}
		if(train.test$max.comp == 1)
		  colnames(x.extracted)[1] <- "1 comp"
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
			# grid.xgboost <- data.frame(nrounds=150, max_depth=13, eta=0.3, gamma=0, colsample_bytree=1, min_child_weight=1, subsample=1)
			# train(as.matrix(x), y, trControl = trainControl(method="none"), method = "xgbTree", tuneGrid=grid.xgboost)$finalModel
			x <- as.matrix(x)
			xgboost(x, y, nrounds = 150, early_stopping_rounds = 5, base_score = mean(y), silent = 1, save_period = NULL)

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
			x <- as.matrix(x)
			lm(y ~ x)
		},
		parrf={
			foreach(ntree = c(70,70,70,70,70,70,80), .combine = combine, .packages = "randomForest") %dopar% randomForest(x, y, ntree = ntree)
		}
	)
	return(regressor)
}

predict_ <- function(regressor, new.data, tech = 'svm', targets) {
	if(use.pls) {
		which.are.targets <- targets %in% colnames(new.data)
		filtered <- targets[which.are.targets]
		if(length(filtered) > 0) {
			tgts <- new.data[,filtered]
			x_ <- new.data[, !(colnames(new.data) %in% filtered)]
			x_ <- as.matrix(x_)
			x.extracted <- as.data.frame(cbind(x_ %*% coef(train.test$pls.model, 1:train.test$max.comp, intercept = F)[,1,], tgts))


		} else {
			x_ <- as.matrix(new.data)
			x.extracted <- as.data.frame(x_ %*% coef(train.test$pls.model, 1:train.test$max.comp, intercept = F)[,1,])
		}

		if(train.test$max.comp == 1)
		  colnames(x.extracted)[1] <- "1 comp"
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
		  regressor$coefficients[is.na(regressor$coefficients)] <- 0
		  as.numeric(as.matrix(new.data)%*%regressor$coefficients[-1] + regressor$coefficients[1])
		},
		parrf={
			predict(regressor, new.data)
		}
	)

	return(predicted)
}
###############################################################
# Remocao de atributos com um unico valor
remove.unique <- function(dataset) {
	uniquelength <- sapply(dataset, function(x) length(unique(x)))
	return(subset(dataset, select = uniquelength > 1))
}
