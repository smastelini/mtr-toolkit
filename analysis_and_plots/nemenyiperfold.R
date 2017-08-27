bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)
n.folds <- 10

mt.techs <- c("ST", "MTRS", "ERC", "MOTC")
techs <- c("ranger", "svm")

log.folder <- "~/MEGA/Experimentos/exp_benchmarks_18dts"
datasets.folder <- "~/MEGA/MT_datasets"

# aRRMSE function
source("../utils_and_includes/utils_MT.R")

n.nemenyi <- n.folds*length(bases)

nem.names <- as.vector(outer(mt.techs, techs, paste, sep="."))

nemenyi <- data.frame(setNames(replicate(length(nem.names),numeric(n.nemenyi), simplify = F),
							nem.names))
for(mt in mt.techs) {
	print(mt)
	for(tch in techs) {
		print(tch)
		indx <- 1
		for(b in seq(bases)) {
			print(bases[b])
			dataset <- read.csv(paste0(datasets.folder, "/", bases[b], ".csv"))
			targets <- colnames(dataset)[(ncol(dataset)-n.targets[b]+1):ncol(dataset)]

			for(k in seq(n.folds)) {
				print(paste("Fold", k))
				log <- read.csv(paste0(log.folder, "/", mt, "/prediction_logs/", tch, "/predictions_",
								mt, "_", bases[b], "_fold", formatC(k, width=2, flag = "0"), ".csv"))

				nemenyi[indx, paste(mt,tch,sep=".")] <- aRRMSE(log, targets)

				indx <- indx + 1
			}
		}
	}
}

write.csv(nemenyi, paste0(log.folder, "/nemenyiperfold.csv"), row.names = F)