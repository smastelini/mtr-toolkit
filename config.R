###############################################################################
#############################General settings##################################
###############################################################################

use.pls <- FALSE
bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scfp")
n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)

bases.teste <- NULL

techs <- c("svm")

folds.num <- 10

# datasets.folder <- "~/MEGA/Experimentos/exp_A1-ExpertSystems/datasets"
datasets.folder <- "~/MEGA/MT_datasets"
output.prefix <- "~/Desktop/DSTARST_BENCH_V.4-TEST"

mt.techs <- c("DSTARST")

#Progress bar and remaining time exhibition
showProgress <- FALSE

must.compare <- FALSE
generate.final.table <- FALSE
###############################################################################
###############################################################################
###############################################################################
