rm(list = ls())
library(data.table)
library(mtrToolkit)

n.folds <- 10

set.seed(23423)

datasets.folder <- "~/MEGA/K-fold_Split"
output.prefix <- "~/MEGA/Experimentos/exp_MORF/all"
output.sufix <- "morf_rf1"

# bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
# n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)

# bases <- c("rf1","rf2","scm1d","scm20d")
# n.targets <- c(8,8,16,16)

bases <- c("rf1")
n.targets <- c(8)

# bases <- c("atp1d","atp7d","oes97","oes10","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
# n.targets <- c(6,6,16,16,2,3,3,3,14,2,3,6,12,3)

# Ensemble
n.trees <- 20

ftest.signf = 0.05
min.size = 5
max.depth = Inf


# Beggining of the Experiment
# source("../utils_and_includes/utils_MT.R")
source("~/mtr-toolkit/utils_and_includes/utils_MT.R")

dir.create(output.prefix, showWarnings = FALSE, recursive = TRUE)

log <- data.frame(
					dataset = rep(bases, n.targets+1),
					target_index = unlist(sapply(n.targets, function(j) rep(seq(from = 0, to = j)))),
					target_name = character(sum(n.targets+1)),
					aRRMSE = numeric(sum(n.targets+1)),
					mean_R2 = numeric(sum(n.targets+1)),
					as.data.frame(setNames(replicate(n.folds, numeric(sum(n.targets+1)), simplify = F),
							 paste0("RRMSE.fold", formatC(seq(n.folds), width=2, flag="0"))
					)),
					as.data.frame(setNames(replicate(n.folds, numeric(sum(n.targets+1)), simplify = F),
							paste0("R2.fold", formatC(seq(n.folds), width=2, flag="0"))
					))
			 )

init <- 1

for(i in seq_along(bases)) {
	print(bases[i])
	for(k in seq(n.folds)) {
		print(paste("Fold", formatC(k, width=2, flag="0")))
		train <- fread(paste0(datasets.folder, "/", bases[i], "_fold", formatC(k, width=2, flag="0"), "_train.csv"))
		test <- fread(paste0(datasets.folder, "/", bases[i], "_fold", formatC(k, width=2, flag="0"), "_test.csv"))

		targets <- colnames(test)[(ncol(test)-n.targets[i]+1):ncol(test)]

		x.train <- train[, !targets, with = FALSE]
		y.train <- train[, targets, with = FALSE]

		x.test <- test[, !targets, with = FALSE]
		y.test <- test[, targets, with = FALSE]

		mtrt <- MORF(x.train, y.train, parallel = T, n.trees = n.trees)
		rm(train, test, x.train, y.train)


		browser()


		predictions <- predict(mtrt, x.test, parallel = F)

		rm(mtrt)

		errors <- sapply(seq(n.targets[i]), function(j, y, y.pred) RRMSE(y[[j]], y.pred[[j]]), y = y.test, y.pred = predictions)
		set(log, init:(init + n.targets[i]), paste0("RRMSE.fold", formatC(k, width=2, flag="0")), c(mean(errors), errors))

		rsquareds <- sapply(seq(n.targets[i]), function(j, y, y.pred) summary(lm(y[[j]] ~ y.pred[[j]]))$r.squared, y = y.test, y.pred = predictions)
		set(log, init:(init + n.targets[i]), paste0("R2.fold", formatC(k, width=2, flag="0")), c(mean(rsquareds), rsquareds))

		# Making free memory
		rm(x.test, y.test, predictions)
	}
	set(log, init:(init + n.targets[i]), "target_name", c("all", targets))
	init <- init + n.targets[i] + 1
}

log[["aRRMSE"]] <- rowMeans(log[,6:15])
log[["mean_R2"]] <- rowMeans(log[, 16:25])
write.csv(log, paste0(output.prefix, "/", output.sufix, ".csv"), row.names = F)
