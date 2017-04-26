# dataset.path <- "~/MEGA/Experimentos/exp_NIRWBMeans/datasets/"
# output.path <- "~/MEGA/Experimentos/exp_NIRWBMeans/datasets/"
#
# d.training <- "NIRWB_means_training"
# d.testing <- "NIRWB_means_testing"
#
# num.targets <- 6
#
# data.training <- read.csv(paste0(dataset.path, d.training, ".csv"))
# data.testing <- read.csv(paste0(dataset.path, d.testing, ".csv"))
#
# t.training <- data.training[,(ncol(data.training)-num.targets+1):ncol(data.training)]
# t.testing <- data.testing[,(ncol(data.testing)-num.targets+1):ncol(data.testing)]
#
# data.training <- data.training[,1:(ncol(data.training)-num.targets)]
# data.testing <- data.testing[,1:(ncol(data.testing)-num.targets)]
#
#
# pca.m <- prcomp(data.training, center = TRUE, scale. = TRUE)
# summary(pca.m)
#
# transf.data.training <- pca.m$x[,1:23]
# transf.data.testing <- predict(pca.m, data.testing)[,1:23]
#
#
# transf.data.training <- cbind(transf.data.training, t.training)
# transf.data.testing <- cbind(transf.data.testing, t.testing)
#
# write.csv(transf.data.training, paste0(output.path, d.training, "_PCA.csv"), row.names = F)
# write.csv(transf.data.testing, paste0(output.path, d.testing, "_PCA.csv"), row.names = F)

dataset <- read.csv("~/Desktop/Base_NIR.csv", stringsAsFactors = F)

amostras <- dataset[,1]
dataset <- dataset[,-1]
pca.m <- prcomp(dataset, center = TRUE, scale. = TRUE)
# Component choice
summary(pca.m)

output <- matrix(nrow=nrow(dataset), ncol=14, 0)

for(i in 1:nrow(dataset)) {
  pca.m <- prcomp(dataset[-i,], center = TRUE, scale. = TRUE)
  output[i,] <- predict(pca.m, dataset[i,])[,1:14]
}

colnames(output) <- paste0("PC", 1:14)
output <- as.data.frame(output)
output <- cbind(amostras, output)

targets <- read.csv("~/Desktop/Resultados_WB.csv")[,-1]
full10t <- cbind(output, targets[,1:10])
#Full dataset
write.csv(full10t, "~/Desktop/NIR_Wooden_FULL10t.csv", row.names = F)

half12t <- cbind(output[c(1:20, 41:60),], targets[c(1:20, 41:60),])
write.csv(half12t, "~/Desktop/NIR_Wooden_HALF12t.csv", row.names = F)
