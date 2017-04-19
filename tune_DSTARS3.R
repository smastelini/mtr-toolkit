rm(list = ls())
library(e1071)
library(RSNNS)
library(plyr)
library(gbm)
library(pls)
library(rpart)
library(MASS)
library(randomForest)
library(xgboost)
library(caret)

use.pls <- FALSE
source("utils_MT.R")
# cmpfile("utils_MT.R")
#Numero maximo de componentes para avaliar
train.test$comp.limit <- 40

library("foreach")
library("doSNOW")
registerDoSNOW(makeCluster(7, type="SOCK"))

dstars.n.folds.tracking <- 5


###############################################################################
#########################CONFIGURAÇÕES GERAIS##################################
###############################################################################

# bases <- c("andro","atp1d","atp7d","edm","enb","jura","oes10","oes97","osales","rf1")
# n.targets <- c(6,6,6,2,2,3,16,16,12,8)

bases <- c("NIRWB_means_training_PCA")
bases <- NULL
n.targets <- c(6)
techs <- c("parrf","svm","ridge")
folds.num <- 10

datasets.folder <- "~/MEGA/Experimentos/exp_NIRWBMeans/datasets"
output.dir.prefix <- "~/Desktop/TUNE_DSTARS3_NIRWB_MEANS_PCA"
mt.techs <- c("DSTARS3")
seed <- 5465

#TUNING GRID
seq.delta <- c(0.0001)
seq.phi <- seq(0.0,0.5, 0.03)
par.grid <- expand.grid(delta=seq.delta, phi=seq.phi)

for(i in 1:nrow(par.grid)) {
	for(tech in techs) {
		d <- par.grid[i,1]
		p <- par.grid[i,2]
		output.dir.dstars3 <- paste0(output.dir.prefix, "/tuning/Delta", toString(d), "Phi", toString(p))

		dstars.delta <- d
		dstars.phi <- p
		set.seed(seed)
		source("DSTARS3.R")
	}
}

sapply(bases, function(base) {
  sapply(techs, function(tech) {
		merged <<- data.frame()
		for(i in 1:nrow(par.grid)) {
			d <- par.grid[i,1]
			p <- par.grid[i,2]
			r.path <- paste0(output.dir.prefix, "/tuning/Delta", toString(d), "Phi", toString(p), "/performance_DSTARS3_", tech, "_", base, ".csv")
			to.merge <- read.csv(r.path, stringsAsFactors = F)
			merged <<- rbind(merged, to.merge)
			merged[nrow(merged),1] <<- paste0(tech,"-delta=",toString(d),"-phi=",toString(p))
		}
		write.csv(merged, paste0(output.dir.prefix, "/performance_", tech, "_", base, ".csv"), row.names = FALSE)
  })
})
