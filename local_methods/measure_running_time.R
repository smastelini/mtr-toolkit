rm(list = ls())
suppressMessages(library(rbenchmark))
suppressMessages(library(data.table))
# SVM
suppressMessages(library(e1071))
# MLP
suppressMessages(library(RSNNS))
suppressMessages(library(plyr))
# GBM
suppressMessages(library(gbm))
# PLS
suppressMessages(library(pls))
# CART
suppressMessages(library(rpart))
# RIDGE REGRESSION
suppressMessages(library(MASS))
# RF
suppressMessages(library(randomForest))
# XGBoost
suppressMessages(library(xgboost))
# Calling of multiple techniques
suppressMessages(library(caret))
# Auxiliary functions
suppressMessages(library(BBmisc))
# Auxiliary functions
suppressMessages(library(permute))
# suppressMessages(library(kernlab))
# RF optimized
suppressMessages(library(ranger))
#Extra libs
source("../utils_and_includes/utils_MT.R")
# DSTARST and DSTARS -> Defaults
# Used only by DSTARST
n.folds.tracking <- 10
# Used by both DSTARS* versions
dstars.delta <- 0.0001

#DRS -> Default
number.layers <- 10


###### Configuration ######

tech <- "ranger"

mt.tech <- "MTRS"

bases.iter <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scfp")
n.targets.iter <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)

bases.test <- NULL
folds.num <- 10
use.pls <- FALSE
datasets.folder <- "~/mastelini/mtr_datasets"
output.prefix <- "~/mastelini/running_time_R"

###########################

exp.random.seeds <- rep(5465, length(bases.iter))

cat("Generated random seeds:\n")
cat(paste0(paste(exp.random.seeds, collapse="-"), "\n"))

#Max. PLS components to evaluate
train.test$comp.limit <- 40

# Creates output directory
assign(paste0("output.dir.", tolower(mt.tech)),
       paste0(output.prefix, "/", mt.tech))

# Performs evaluation
for(index.dataset in seq(bases.iter)) {
  bases <- bases.iter[index.dataset]
  n.targets <- n.targets.iter[index.dataset]

  time.per.dataset <- benchmark(evaluation={
    cat("\n\n####################################\n\n")
    cat(paste0("                ", mt.tech))
    cat("\n\n####################################\n\n")

    source(paste0(mt.tech, ".R"))
  }, replications=30)
  write.csv(time.per.dataset,
          paste0(output.prefix, "/running_time_", mt.tech, "_", tech, "_",
          bases.iter[index.dataset], ".csv"))
}
