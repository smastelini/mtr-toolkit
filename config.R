###############################################################################
#############################General settings##################################
###############################################################################

use.pls <- TRUE
#bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scfp")
#n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)

bases <- c("base_PM_TR", "base_WB_TR")
n.targets <- c(5,6)
bases.teste <- NULL
# bases.teste <- c("base_PM_TS", "base_WB_TS")

techs <- c("parrf", "svm", "lr")

folds.num <- 10

datasets.folder <- "~/MEGA/Experimentos/exp_A1-ExpertSystems/datasets"
output.prefix <- "~/MEGA/Experimentos/exp_A1-ExpertSystems/outputCV"

mt.techs <- c("DSTARST")

#Progress bar and remaining time exhibition
showProgress <- FALSE

must.compare <- TRUE
generate.final.table <- TRUE
###############################################################################
###############################################################################
###############################################################################
