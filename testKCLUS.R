library(data.table)
rm(list = ls())

source("utils_MT.R")

dataset <- read.csv("~/MEGA/MT_datasets/atp1d.csv")
dataset <- remove.unique(dataset)


i <- 1

targets <- colnames(dataset)[(ncol(dataset)-5):ncol(dataset)]

#Center and Scaling
dataset <- as.data.table(dataset)
invisible(dataset[, names(dataset) := lapply(.SD, as.numeric)])

maxs <- as.numeric(dataset[, lapply(.SD, max)])
names(maxs) <- colnames(dataset)
mins <- as.numeric(dataset[, lapply(.SD, min)])
names(mins) <- colnames(dataset)

dataset <- as.data.table(scale(dataset, center = mins, scale = maxs - mins))



x <- dataset[, !targets, with = FALSE]
y <- dataset[, targets, with = FALSE]

x.train <- x[1:300]
y.train <- y[1:300]

x.test <- x[301:337]
y.test <- y[301:337]

source("KCLUS.R")

n.trees <- 10

preds <- list()

for(i in seq(n.trees)) {
  idxs <- sample(nrow(x), replace = T)
  x.boost <- x[idxs]
  y.boost <- y[idxs]
  
  
  kclus <- KCLUS$train(x.train, y.train, k = 3, max.depth = 10, var.improvp = 0.1)
  preds[[i]] <- KCLUS$predict(kclus, x.test)
  
}

predictions <- as.data.frame(apply(simplify2array(lapply(preds, as.matrix)),1:2, mean, na.rm = TRUE))

log <- copy(y.test)
log[, (paste0(colnames(y),".pred")) := predictions]
print(aRMSE(as.data.frame(log), names(y)))
