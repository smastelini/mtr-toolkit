library(data.table)
rm(list = ls())

source("../utils_and_includes/utils_MT.R")

dataset <- read.csv("~/MEGA/MT_datasets/andro.csv")
dataset <- remove.unique(dataset)

dataset <- dataset[sample(nrow(dataset)),]

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

x.train <- x[1:40]
y.train <- y[1:40]

x.test <- x[41:49]
y.test <- y[41:49]

source("KCLUS.R")
# Ensemble
n.trees <- 100

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
print(aRMSE(as.data.frame(log), targets))
