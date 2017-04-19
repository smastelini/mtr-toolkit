dataset.name <- "NIRWB_means_training"
dataset.folder <- "~/MEGA/Experimentos/exp_NIRWBMeans/datasets"

dataset <- read.csv(paste0(dataset.folder, "/",dataset.name,".csv"))
techs <- c("parrf", "svm", "ridge")
MT.techs <- c("ST", "MTRS", "ERC", "DSTARST")
log.folder <- "~/MEGA/Experimentos/exp_NIRWBMeans/resultsTesting/PLS"

n.targets <- 6
n.folds <- 1

phi.values <- c(0.6, 0.8, 0.7)

targets <- colnames(dataset)[(ncol(dataset)-n.targets+1):ncol(dataset)]
maxs <- apply(dataset[,targets], 2, max)
mins <- apply(dataset[,targets], 2, min)

denormalized.log <- data.frame(rep(NA, 0), matrix(nrow = 0, ncol = n.targets), stringsAsFactors = F)
colnames(denormalized.log) <- c("tech",targets)

# Root-Mean-Squared Error
RMSE <- function(actual, predicted) {
  return(sqrt(mean((actual-predicted)^2)))
}

for(mt in MT.techs) {
  phi.ind <- 1
  for(tech in techs) {
    log.RMSE.CV <- data.frame(matrix(nrow=n.folds, ncol=n.targets))
    colnames(log.RMSE.CV) <- colnames(dataset)[(ncol(dataset)-n.targets+1):ncol(dataset)]
    for(i in 1:n.folds) {
      if(mt != "DSTARST")
        log <- read.csv(paste0(log.folder,"/", mt, "/prediction_logs/", tech, "/predictions_", mt, "_", dataset.name, "_fold", formatC(i, width=2, flag="0"), ".csv"))
      else
       log <- read.csv(paste0(log.folder,"/", mt, "/output_logs/testing_final_logs/phi=", phi.values[phi.ind], "/", dataset.name, "_", tech, "_testing_final_predictions_fold", formatC(i, width=2, flag="0"), ".csv"))

      for(t in targets) {
        r <- (maxs[t]-mins[t])*log[,t] + mins[t]
        p <- (maxs[t]-mins[t])*log[,paste0(t, ".pred")] + mins[t]

        log.RMSE.CV[i,t] <- RMSE(r,p)
      }
    }
    log.RMSE.CV[n.folds+1,] <- colMeans(log.RMSE.CV)
    denormalized.log[nrow(denormalized.log)+1,1] <- paste(mt, tech, sep="-")
    denormalized.log[nrow(denormalized.log),2:ncol(denormalized.log)] <- log.RMSE.CV[n.folds+1,]

    phi.ind <- phi.ind + 1
  }
  # denormalized.log[nrow(denormalized.log)+1,] <- rep(NA, ncol(denormalized.log))
}


write.csv(denormalized.log, "~/Desktop/DENORM.csv", row.names = F)
