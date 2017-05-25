###############################################################################
#############################General settings##################################
###############################################################################

use.pls <- FALSE
bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scfp")
n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)
bases.teste <- NULL

# techs <- c("parrf","svm","xgboost","cart")
techs <- c("svm","xgboost","cart")

folds.num <- 10

datasets.folder <- "~/MEGA/MT_datasets"
output.prefix <- "~/MEGA/exp_benchmarks_18dts"

# mt.techs <- c("ST", "MTRS", "ERC", "DSTARSIT")
mt.techs <- c("DSTARSIT")

#Progress bar and remaining time exhibition
showProgress <- FALSE

must.compare <- FALSE
generate.final.table <- FALSE
###############################################################################
###############################################################################
###############################################################################
