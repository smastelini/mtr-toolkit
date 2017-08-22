log.folder <- "~/MEGA/Experimentos/exp_benchmarks_18dts/MOTC/raw_logs"

n.folds <- 10
datasets <-  c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)
techs = c("ranger", "svm")

model.count <- data.frame(dataset = character(length(datasets)), n.models.rf = numeric(length(datasets)),
  n.models.svm = numeric(length(datasets)), stringsAsFactors = F)

for(b in seq(datasets)) {
  model.count[b,1] <- datasets[b]
  for(tech in seq(techs)) {
    fold.count <- c()
    for(k in seq(n.folds)) {
      target.count <- 0
      for(t in seq(n.targets[b])) {
        log <- read.csv(paste0(log.folder, "/", techs[tech], "/raw_MOTC_testing_", datasets[b], "_fold", formatC(k, width=2, flag="0"), "_T", formatC(t, width=2, flag="0"), ".csv"))
        target.count <- target.count + (ncol(log) - 1)
      }
      fold.count <- c(fold.count, target.count)
    }
    model.count[b,tech+1] <- mean(fold.count)
  }
}

write.csv(model.count, "~/Desktop/MOTC_model_count.csv", row.names = F)
