###############################################################################
#############################General settings##################################
###############################################################################
use.pls <- FALSE
# bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
# n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)

bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)

# bases <- c("intacta_r_training", "intacta_rc_training", "intacta_rl_training", "intacta_rcl_training",
# 					 "intacta_pca_training", "intacta_pcac_training", "intacta_pcal_training", "intacta_pcacl_training",
# 					 "moida_r_training", "moida_rc_training", "moida_rl_training", "moida_rcl_training",
# 					 "moida_pca_training", "moida_pcac_training", "moida_pcal_training", "moida_pcacl_training")

# n.targets <- rep(9, length(bases))

bases.teste <- NULL

techs <- c("ranger", "svm", "xgboost", "cart")

folds.num <- 10

datasets.folder <- "~/MEGA/MT_datasets"
output.prefix <- "~/Desktop"

mt.techs <- c("ST", "MTRS", "ERC", "DSTARST")

#Progress bar and remaining time exhibition
showProgress <- FALSE

must.compare <- TRUE
generate.final.table <- TRUE
generate.nemenyi.frame <- TRUE
###############################################################################
###############################################################################
###############################################################################
