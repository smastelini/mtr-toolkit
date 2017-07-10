###############################################################################
#############################General settings##################################
###############################################################################
exp.seed <- 5465
use.pls <- FALSE
bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)

bases.teste <- NULL

techs <- c("ranger")

folds.num <- 10

# datasets.folder <- "~/MEGA/MT_datasets"
# output.prefix <- "~/MEGA/Experimentos/exp_benchmarks_18dts"
datasets.folder <- "~/MT_datasets"
output.prefix <- "~/mastelini/FULLB"

mt.techs <- c("ST","MTRS","ERC","ORC")

#Progress bar and remaining time exhibition
showProgress <- FALSE

must.compare <- TRUE
generate.final.table <- TRUE
###############################################################################
###############################################################################
###############################################################################
