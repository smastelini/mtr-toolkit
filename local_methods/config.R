###############################################################################
#############################General settings##################################
###############################################################################
use.pls <- FALSE
# bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
# n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)

# Adicionar tabela com grid
dstars.phis <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
dstars.epsilons <- c(10^-1, 10^-2, 10^-3, 10^-4)


bases <- c("atp1d","oes97","osales","andro")
n.targets <- c(6,16,12,6)

bases.teste <- NULL

techs <- c("ranger", "svm")

folds.num <- 10

datasets.folder <- "~/Desktop/mtr_datasets"
output.prefix <- "~/Desktop/GRID_TEST"

mt.techs <- c("DSTARST")
# mt.techs <- c("ST")

#Progress bar and remaining time exhibition
showProgress <- FALSE

must.compare <- FALSE
generate.final.table <- FALSE
generate.nemenyi.frame <- FALSE
###############################################################################
###############################################################################
###############################################################################
