bases <- c("1st_period","2nd_period","3rd_period","4th_period","all_period")
n.targets <- c(6,6,6,6,6)
n.folds <- 10

mt.techs <- c("ST", "MTRS", "ERC", "DSTARST", "MORF")
techs <- c("ranger", "svm")

log.folder <- "~/Dropbox/SBSI - Stock Prices/Saidas"
datasets.folder <- "~/Dropbox/SBSI - Stock Prices/Datasets"

# aRRMSE function
source("../utils_and_includes/utils_MT.R")

n.nemenyi <- n.folds*length(bases)

nem.names <- as.vector(outer(mt.techs, techs, paste, sep="."))

nemenyi <- data.frame(setNames(replicate(length(nem.names),numeric(n.nemenyi), simplify = F),
							nem.names))

phis <- data.frame(ranger=c(0.3,0.5,0.4,0.3,0.5), svm=c(0.5,0.5,0.3,0.7,0.3))
for(mt in mt.techs) {
	print(mt)
	for(tch in techs) {
		print(tch)
		indx <- 1
		dataset.cont <- 1
		for(b in seq(bases)) {
			print(bases[b])
			dataset <- read.csv(paste0(datasets.folder, "/", bases[b], ".csv"))
			targets <- colnames(dataset)[(ncol(dataset)-n.targets[b]+1):ncol(dataset)]

			for(k in seq(n.folds)) {
				print(paste("Fold", k))
				if(mt == "DSTARST") {
					log <- read.csv(paste0(log.folder, "/", mt, "/output_logs/testing_final_logs/phi=", phis[dataset.cont,tch], "/", bases[b], "_",
					tch, "_testing_final_predictions_fold", formatC(k, width=2, flag = "0"), ".csv"))
				} else if(mt == "MORF")
					log <- read.csv(paste0(log.folder, "/", mt, "/prediction_logs/morf/predictions_",
					mt, "_", bases[b], "_fold", formatC(k, width=2, flag = "0"), ".csv"))
				else {
					log <- read.csv(paste0(log.folder, "/", mt, "/prediction_logs/", tch, "/predictions_",
					mt, "_", bases[b], "_fold", formatC(k, width=2, flag = "0"), ".csv"))
				}

				nemenyi[indx, paste(mt,tch,sep=".")] <- aRRMSE(log, targets)

				indx <- indx + 1
			}
			dataset.cont <- dataset.cont + 1
		}
	}
}

write.csv(nemenyi, paste0(log.folder, "/nemenyiperfold.csv"), row.names = F)
