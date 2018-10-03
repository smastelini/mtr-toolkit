###############################################################################
#############################General settings##################################
###############################################################################
use.pls <- FALSE
norm.method <- "min-max"

# Adicionar tabela com grid
dstars.phis <- c(0.4)
dstars.epsilons <- c(10^-4)

bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scfp")
n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)

bases.test <- NULL

techs <- c("ranger", "svm", "gbm", "xgboost", "lr", "ridge", "pls", "mlp", "cart")

folds.num <- 10

datasets.folder <- "~/Desktop/mtr_datasets"
output.prefix <- "~/OUTPUT_MTR2"

mt.techs <- c("ST", "SST", "ERC", "DSTARS", "DSTARST", "MTAS", "MTSG", "ESR", "MOTC", "ORC", "DRS")

must.compare <- TRUE
generate.final.table <- TRUE
generate.nemenyi.frame <- TRUE
###############################################################################
###############################################################################
###############################################################################
