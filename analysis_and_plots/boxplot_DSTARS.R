datasets <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")

techs <- c("ranger", "svm", "xgboost", "cart")

regs <- c("RF", "SVM", "XGBoost", "CART")
names(regs) <- techs

datasets.f <- "~/MEGA/MT_datasets"
logf <- "~/MEGA/Experimentos/exp_benchmarks_18dts/DSTARST"
where.save <- "~/Desktop"


library(ggplot2)
datap <- data.frame(dataset=character(0), aRRMSE=numeric(0), stringsAsFactors=F)
for(tech in techs) {
	median.log <- data.frame(aRRMSE.mean=numeric(length(datasets)), aRRMSE.std=numeric(length(datasets)), median.phi=numeric(length(datasets)))
	rownames(median.log) <- datasets

  for(d in datasets) {
    log <- read.csv(paste0(logf, "/", "performance_DSTARST_", tech, "_", d, ".csv"))
    r <- nrow(datap)

    datap[(r+1):(r+11), ] <- list(rep(d, 11), log[, "aRRMSE"])

    median.log[d, "aRRMSE.mean"] <- mean(log[, "aRRMSE"])
    median.log[d, "aRRMSE.std"] <- sd(log[, "aRRMSE"])
    median.log[d, "median.phi"] <- seq(0,1,0.1)[which(log[, "aRRMSE"] == median(log[, "aRRMSE"]))[1]]
  }
  g <- ggplot(aes(y = aRRMSE, x = dataset, fill=dataset), data = datap) + geom_boxplot() + 
  	guides(fill=FALSE) + scale_fill_hue(l=40) + xlab("Base de dados")

  ggsave(paste0(where.save, "/boxplot_DSTARS_", regs[tech], ".pdf"), g, width=10, height=5)
  write.csv(median.log, paste0("~/Desktop/phi_median_", tech, ".csv"), row.names=T)
}
