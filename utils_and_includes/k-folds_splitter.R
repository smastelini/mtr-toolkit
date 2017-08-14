rseed <- 5465

datasets.folder <- "~/MEGA/MT_datasets"
output.folder <- "~/MEGA/K-fold_Split"
n.folds <- 10

datasets <-  c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")

source("utils_MT.R")

print("Splitting")
for(i in seq_along(datasets)) {
  dataset <- read.csv(paste0(datasets.folder, "/", datasets[i], ".csv"), stringsAsFactors = T)
  dataset <- remove.unique(dataset)

  dataset <- dataset[sample(nrow(dataset)),]

  dataset <- as.data.frame(sapply(dataset, function(x) as.numeric(x)))

  maxs <- apply(dataset, 2, max)
  mins <- apply(dataset, 2, min)

	dataset <- as.data.frame(scale(dataset, center = mins, scale = maxs - mins))

  len.fold <- round(nrow(dataset)/n.folds)

  for(k in seq(n.folds)) {
    test.idx <- ((k-1)*len.fold + 1):(ifelse(k==n.folds, nrow(dataset), k*len.fold))
    train.idx <-  setdiff(1:nrow(dataset), test.idx)

    write.csv(dataset[train.idx,], paste0(output.folder, "/", datasets[i], "_fold", formatC(k, width=2, flag="0"), "_train.csv"), row.names = F)
    write.csv(dataset[test.idx,], paste0(output.folder, "/", datasets[i], "_fold", formatC(k, width=2, flag="0"), "_test.csv"), row.names = F)
  }
  print(datasets[i])
}
