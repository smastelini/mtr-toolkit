rm(list = ls())

library(reshape2)
library(ggplot2)
library(randomForest)

plot_corr <- function(dplot, out.path, nt) {
  colnames(dplot) <- c("Targetsx", "Targetsy", "value")
  
  ggplot(data = dplot, aes(x=Targetsx, y=Targetsy, fill = value)) +
    geom_tile(color = "black") + 
    scale_fill_gradient2(mid = "gray", high = "black", limit = c(0,0.45)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1),
          axis.text.y = element_text(angle = 0, vjust = 0.5, 
                                     size = 12, hjust = 0)) +
    coord_fixed() +
    geom_text(aes(Targetsx, Targetsy, label = sprintf("%0.2f", round(value, digits = 2))), 
              color = "white", size = 5) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.major = element_blank(),
      # panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none")
  
  if(nt <= 3) {
    ggsave(file=paste0(out.path, "/", bases[b], "_corr.pdf"), width = 3, height = 3)
  } else if(nt <= 8) {
    ggsave(file=paste0(out.path, "/", bases[b], "_corr.pdf"), width = 5, height = 5)  
  } else if(nt <= 12) {
    ggsave(file=paste0(out.path, "/", bases[b], "_corr.pdf"), width = 7, height = 7)  
  } else {
    ggsave(file=paste0(out.path, "/", bases[b], "_corr.pdf"), width = 10, height = 10)  
  }
}

datasets.folder <- "~/MEGA/MT_datasets"
logs.folder <- "~/MEGA/Experimentos/exp_benchmarks_18dts/DSTARST/output_logs/convergence_layers_logs"
out.path <- "~/Desktop/dissertacao_corr"

bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)

targets <- list()
targets[["atp1d"]] <- c("ALLminpA", "ALLminp0", "aDLminpA", "aCOminpA", "aFLminpA", "aUAminpA")
targets[["atp7d"]] <- c("ALLminpA", "ALLminp0", "aDLminpA", "aCOminpA", "aFLminpA", "aUAminpA")
targets[["oes97"]] <- c("58028", "15014", "32511", "15017", "98502", "92965", "32314", "13008",
  "21114", "85110", "27311", "98902", "65032", "92998", "27108", "53905")
targets[["oes10"]] <- c("513021", "292071", "392021", "151131", "151141", "291069", "119032", 
  "432011", "419022", "292037", "519061", "291051", "172141", "431011", "291127", "412021")
targets[["rf1"]] <- c("CHSI2", "NASI2", "EADM7", "SCLM7", "CLKM7", "VALI2", "NAPM7", "DLDI4")
targets[["rf2"]] <- c("CHSI2", "NASI2", "EADM7", "SCLM7", "CLKM7", "VALI2", "NAPM7", "DLDI4")
targets[["scm1d"]] <- c("LBL", "MTLp2", "MTLp3", "MTLp4", "MTLp5", "MTLp6", "MTLp7", "MTLp8", 
  "MTLp9", "MTLp10", "MTLp11", "MTLp12", "MTLp13", "MTLp14", "MTLp15", "MTLp16")
targets[["scm20d"]] <- c("LBL", "MTLp2A", "MTLp3A", "MTLp4A", "MTLp5A", "MTLp6A", "MTLp7A", "MTLp8A",
 "MTLp9A", "MTLp10A", "MTLp11A", "MTLp12A", "MTLp13A", "MTLp14A", "MTLp15A", "MTLp16A")
targets[["edm"]] <- c("DFlow", "DGap")
targets[["sf1"]] <- c("c.class", "m.class", "x.class")
targets[["sf2"]] <- c("c.class", "m.class", "x.class")
targets[["jura"]] <- c("Cd", "Co", "Cu")
targets[["wq"]] <- c("25400", "29600", "30400", "33400", "17300", "19400", "34500", "38100", "49700", 
  "50390", "55800", "57500", "59300", "37880")
targets[["enb"]] <- c("Y1", "Y2")
targets[["slump"]] <- c("slump", "flow", "cpr_str")
targets[["andro"]] <- c("Target", "Target_2", "Target_3", "Target_4", "Target_5", "Target_6")
targets[["osales"]] <- c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12")
targets[["scpf"]] <- c("views", "votes", "comments")

n.folds <- 10

for(b in seq(length(bases))) {
  print(bases[b])
  for(k in seq(n.folds)) {
    if(k == 1) {
      log <- read.csv(paste0(logs.folder, "/", bases[b], "_ranger_RF_importance_", formatC(k, width=2, flag="0"), ".csv"))
      log <- log[,-1]
    } else {
      aux <- read.csv(paste0(logs.folder, "/", bases[b], "_ranger_RF_importance_", formatC(k, width=2, flag="0"), ".csv"))
      aux <- aux[,-1]
      log <- log + aux
    }
  }

  log <- log/n.folds
  log <- log/100
  log <- as.matrix(log)


  rownames(log) <- colnames(log) <- targets[[bases[b]]]
  dplot <- melt(log)

  dplot[, 1] <- as.character(dplot[, 1])
  dplot[, 2] <- as.character(dplot[, 2])

  plot_corr(dplot, out.path, n.targets[b])
}