dataset <- read.csv("~/MEGA/MT_datasets/atp7d.csv", stringsAsFactors = F)

X <- dataset[, -c(412:ncol(dataset))]
X <- remove.unique(X)
Y <- dataset[, 412:ncol(dataset)]

source("~/mtr-toolkit/utils_and_includes/utils_MT.R")


# pca.m <- prcomp(X, center = TRUE, scale. = TRUE)
# # Component choice
# summary(pca.m)

num.comp <- 60

output <- matrix(nrow=nrow(dataset), ncol=num.comp, 0)

for(i in 1:nrow(dataset)) {
  pca.m <- prcomp(X[-i,], center = TRUE, scale. = TRUE)
  output[i,] <- predict(pca.m, X[i,])[,1:num.comp]
}

colnames(output) <- paste0("PC", 1:num.comp)
output <- as.data.frame(output)


final <- cbind(output, Y)
write.csv(final, "~/Desktop/atp7d_PCA.csv", row.names = F)
