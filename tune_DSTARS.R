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
# Max PLS components to evaluate
train.test$comp.limit <- 40

library("foreach")
library("doSNOW")
registerDoSNOW(makeCluster(7, type="SOCK"))

dstars.n.folds.tracking <- 10


###############################################################################
#########################CONFIGURAÇÕES GERAIS##################################
###############################################################################

bases <<- c("andro","atp1d","atp7d","edm","enb","jura","oes10","oes97","osales")
n.targets <<- c(6,6,6,2,2,3,16,16,12)


bases.teste <<- NULL

techs <<- c("parrf", "svm", "cart", "xgboost")
folds.num <<- 10

datasets.folder <<- "~/MT_datasets"
output.dir.prefix <- "~/mastelini/MT_DSTARS_tuning"
seed <- 5465

#TUNING GRID
seq.delta <- c(0.0001)
seq.phi <- seq(0,1, 0.1)
par.grid <- expand.grid(delta=seq.delta, phi=seq.phi)

for(i in 1:nrow(par.grid)) {
	for(tec in techs) {
		tech <<- tec
		d <- par.grid[i,1]
		p <- par.grid[i,2]
		output.dir.dstars <<- paste0(output.dir.prefix, "/tuning/Delta", toString(d), "Phi", toString(p))

		dstars.delta <<- d
		dstars.phi <<- p
		set.seed(seed)
		source("DSTARS.R")
	}
}

sapply(bases, function(base) {
  sapply(techs, function(tech) {
		merged <<- data.frame()
		for(i in 1:nrow(par.grid)) {
			d <- par.grid[i,1]
			p <- par.grid[i,2]
			r.path <- paste0(output.dir.prefix, "/tuning/Delta", toString(d), "Phi", toString(p), "/performance_DSTARS_", tech, "_", base, ".csv")
			to.merge <- read.csv(r.path, stringsAsFactors = F)
			merged <<- rbind(merged, to.merge)
			merged[nrow(merged),1] <<- paste0(tech,"-delta=",toString(d),"-phi=",toString(p))
		}
		write.csv(merged, paste0(output.dir.prefix, "/performance_", tech, "_", base, ".csv"), row.names = FALSE)
  })
})
