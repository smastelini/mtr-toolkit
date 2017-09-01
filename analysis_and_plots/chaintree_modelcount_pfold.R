bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)
n.folds <- 10

compared.mt.techs <- c("ST", "MTRS", "ERC")
techs <- c("ranger", "svm")

log.folder <- "~/MEGA/Experimentos/exp_benchmarks_18dts"

n.nemenyi <- n.folds*length(bases)

nem.names <- c(compared.mt.techs, paste("MOTC", techs, sep="."))

nemenyi <- data.frame(setNames(replicate(length(nem.names),numeric(n.nemenyi), simplify = F),
              nem.names))

for(mt in compared.mt.techs) {
  print(mt)
  indx <- 1
  for(b in seq(bases)) {
    print(bases[b])
    for(k in seq(n.folds)) {
      if(mt == "ST")
        nemenyi[indx, mt] <- n.targets[b]
      else if(mt == "MTRS")
        nemenyi[indx, mt] <- 2*n.targets[b]
      else if(mt == "ERC") {
        if(n.targets[b] <= 3)
          nemenyi[indx, mt] <- factorial(n.targets[b])*n.targets[b]
        else
          nemenyi[indx, mt] <- 10*n.targets[b]
      }
      indx <- indx + 1
    }
  }
}

print("MOTC")
for(tch in techs) {
  print(tch)
  indx <- 1
  for(b in seq(bases)) {
    print(bases[b])
    for(k in seq(n.folds)) {
      print(paste("Fold", k))
      target.count <- 0
      # Discount redundant leaf nodes
      leaf.cont <- 0
      for(t in seq(n.targets[b])) {
        log <- read.csv(paste0(log.folder, "/MOTC/raw_logs/", tch, "/raw_MOTC_testing_", bases[b],
          "_fold", formatC(k, width=2, flag="0"), "_T", formatC(t, width=2, flag="0"), ".csv"))
        target.count <- target.count + (ncol(log) - 1)
        leaf.cont <- leaf.cont + length(which(grepl('leaf.', colnames(log))))
      }
      nemenyi[indx, paste("MOTC",tch,sep=".")] <- target.count - leaf.cont + n.targets[b]

      indx <- indx + 1
    }
  }
}

write.csv(nemenyi, paste0(log.folder, "/nemenyimemperfold.csv"), row.names = F)
