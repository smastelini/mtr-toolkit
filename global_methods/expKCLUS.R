rm(list = ls())
library(data.table)

n.folds <- 10

set.seed(23423)

datasets.folder <- "~/MEGA/K-fold_Split"
output.prefix <- "~/MEGA/Experimentos/exp_KCLUS"
output.sufix <- "results_14dts_100trees_maxdepth_0.01improv_5k_mean"

# bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
# n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)

bases <- c("atp1d","atp7d","oes97","oes10","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
n.targets <- c(6,6,16,16,2,3,3,3,14,2,3,6,12,3)

# Ensemble
n.trees <- 100

#kClus config
ramification.factor = 5
max.depth = Inf
var.improvp = 0.01
min.kclus.size <- NULL
pred.type <- "mean"

# Beggining of the Experiment
source("../utils_and_includes/utils_MT.R")
source("../global_methods/KCLUS.R")

dir.create(output.prefix, showWarnings = FALSE, recursive = TRUE)

log <- data.frame(
          dataset = rep(bases, n.targets+1),
          target_index = unlist(sapply(n.targets, function(j) rep(seq(from = 0, to = j)))),
          target_name = character(sum(n.targets+1)),
          as.data.frame(setNames(replicate(n.folds+1, numeric(sum(n.targets+1)), simplify = F),
               c(paste0("fold", formatC(seq(n.folds), width=2, flag="0")), "mean")
          ))
       )

init <- 1
for(i in seq_along(bases)) {
  print(bases[i])
  for(k in seq(n.folds)) {
    print(paste("Fold", formatC(k, width=2, flag="0")))
    train <- read.csv(paste0(datasets.folder, "/", bases[i], "_fold", formatC(k, width=2, flag="0"), "_train.csv"))
    test <- read.csv(paste0(datasets.folder, "/", bases[i], "_fold", formatC(k, width=2, flag="0"), "_test.csv"))

    targets <- colnames(test)[(ncol(test)-n.targets[i]+1):ncol(test)]

    train <- as.data.table(train)
    test <- as.data.table(test)

    x.train <- train[, !targets, with = FALSE]
    y.train <- train[, targets, with = FALSE]

    x.test <- test[, !targets, with = FALSE]
    y.test <- test[, targets, with = FALSE]

    mtry <- max(floor(log2(ncol(x.train) + 1)), 1)
    #######################################################
    preds <- list()
    wgts <- list()

    for(trs in seq(n.trees)) {
      idxs <- sample(nrow(x.train), replace = T)
      x.boost <- x.train[idxs]
      y.boost <- y.train[idxs]

      sampled.cols <- sample(ncol(x.train), mtry)

      kclus <- KCLUS$train(x.boost[, sampled.cols, with = F], y.boost, ramification.factor, max.depth, var.improvp, pred.type, min.kclus.size)

      outcomes <- KCLUS$predict(kclus, x.test[, sampled.cols, with = F])

      if(pred.type == "mean")
        preds[[trs]] <- outcomes
      else {
        preds[[trs]] <- outcomes$predictions
        wgts[[trs]] <- outcomes$weights
      }
    }

    if(pred.type == "mean")
      predictions <- as.data.table(apply(simplify2array(lapply(preds, as.matrix)), 1:2, mean, na.rm = TRUE))
    else {
      sum.wgts <- apply(simplify2array(lapply(wgts, as.matrix)), 1:2, sum, na.rm = TRUE)
      sum.wgts <- 1/sum.wgts

      wgts <- lapply(wgts, function(w, sum.w) w * sum.w, sum.w = sum.wgts)

      predictions <- as.data.table(
        apply(
          simplify2array(
            lapply(
              lapply(seq(n.trees), function(j, p, w) p[[j]] * w[[j]], p = preds, w = wgts),
              as.matrix
            )
          ),
          1:2, sum, na.rm = TRUE)
      )
    }

    errors <- sapply(seq(n.targets[i]), function(j, y, y.pred) RRMSE(y[[j]], y.pred[[j]]), y = y.test, y.pred = predictions)

    set(log, init:(init + n.targets[i]), paste0("fold", formatC(k, width=2, flag="0")), c(mean(errors), errors))
  }
  set(log, init:(init + n.targets[i]), "target_name", c("all", targets))
  init <- init + n.targets[i] + 1
}

log[["mean"]] <- rowMeans(log[, 5:(ncol(log)-1)])
write.csv(log, paste0(output.prefix, "/", output.sufix, ".csv"), row.names = F)
