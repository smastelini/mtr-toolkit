include.testing.set <-TRUE

datasets.folder <- "~/Documents/mtr-fluorescencia/dataset/"
dataset <- read.csv(paste0(datasets.folder,"data_train.csv"), stringsAsFactors = F, row.names = 1)

n.targets <- 10

X <- dataset[, c(1:(ncol(dataset)-n.targets))]
X <- remove.unique(X)
Y <- dataset[, (ncol(dataset)-n.targets+1) :ncol(dataset)]

source("~/Documents/mtr-fluorescencia/utils_and_includes/utils_MT.R")


# pca.m <- prcomp(X, center = TRUE, scale. = TRUE)
# # # Component choice
# summary(pca.m)
# library("factoextra")
# factoextra::fviz_screeplot(pca.m, ncp=40)

componentes <- c(5, 10, 15)

for (num.comp in componentes){
  
  output <- matrix(nrow=nrow(dataset), ncol=num.comp, 0)
  
  
  for(i in 1:nrow(dataset)) {
    pca.m <- prcomp(X[-i,], center = TRUE, scale. = TRUE)
    output[i,] <- predict(pca.m, X[i,])[,1:num.comp]
  }
  
  colnames(output) <- paste0("PC", 1:num.comp)
  output <- as.data.frame(output)
  
  
  final <- cbind(output, Y)
  write.csv(final, paste0(datasets.folder,"PCA",num.comp,".csv"), row.names = F)
  
}


if(include.testing.set==TRUE){
  pca.m <- prcomp(X[,], center = TRUE, scale. = TRUE) ##new samples wouldn`t have access to new scores, so it uses the scores of the training set
  
  dataset <- read.csv(paste0(datasets.folder,"data_test.csv"), stringsAsFactors = F, row.names = 1)
  
  X <- dataset[, c(1:(ncol(dataset)-n.targets))]
  X <- remove.unique(X)
  Y <- dataset[, (ncol(dataset)-n.targets+1) :ncol(dataset)]
  
  componentes <- c(5, 10, 15)
  
  for (num.comp in componentes){
    
    output <- matrix(nrow=nrow(dataset), ncol=num.comp, 0)
    
    for(i in 1:nrow(dataset)) {
      output[i,] <- predict(pca.m, X[i,])[,1:num.comp]
    }
    
    colnames(output) <- paste0("PC", 1:num.comp)
    output <- as.data.frame(output)
    
    final <- cbind(output, Y)
    write.csv(final, paste0(datasets.folder,"PCA_test",num.comp,".csv"), row.names = F)
    
  }
}
