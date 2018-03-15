rm(list = ls())

library(reshape2)
library(ggplot2)
library(randomForest)

dataset.folder <- "~/MEGA/MT_datasets"
out.path <- "~/Desktop/dissertacao_corr"

bases <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)


deps <- list()

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

plot_corr <- function(dplot, plot.title, out.path) {
  colnames(dplot) <- c("Targetsx", "Targetsy", "value")
  
  ggplot(data = dplot, aes(x=Targetsx, y=Targetsy, fill = abs(value))) +
    geom_tile(color = "white") + 
    scale_fill_gradient2(name=plot.title, low = "gray", mid = "darkgray", high = "black", limit = c(0,1)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 0.5),
          axis.text.y = element_text(angle = 0, vjust = 0.5, 
                                     size = 12, hjust = 0)) +
    coord_fixed() +
    geom_text(aes(Targetsx, Targetsy, label = sprintf("%0.2f", round(value, digits = 2))), 
              color = "white", size = 5) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      # legend.justification = c(1, 1),
      legend.position = "none") + ggtitle(plot.title) +
      theme(plot.title = element_text(hjust = 0.5, size = 16))
  
  ggsave(file=paste0(out.path, "/", bases[b], "_", plot.title, "_corr.pdf"), width = 6, height = 5)
}


for(b in 1:length(bases)) {
  dplot <- data.frame(matrix(nrow=0,ncol=3), stringsAsFactors = F)
  colnames(dplot) <- NA
  
  
  dataset <- read.csv(paste0(dataset.folder, "/", bases[b], ".csv"))
  targets <- dataset[,(ncol(dataset)-n.targets[b]+1):ncol(dataset)]
  # colnames(targets) <- paste("Target", 1:n.targets[b])
  
  # dplot <- melt(get_lower_tri(cor(targets, method = "pearson")), na.rm = T)
  dplot <- melt(cor(targets, method = "pearson"), na.rm = T)
  plot_corr(dplot, "Pearson Correlation", out.path)
  
  # dplot <- melt(get_lower_tri(cor(targets, method = "spearman")), na.rm = T)
  dplot <- melt(cor(targets, method = "spearman"), na.rm = T)
  plot_corr(dplot, "Spearman Correlation", out.path)
  
  rf.imp <- matrix(nrow=ncol(targets), ncol=ncol(targets))
  for(j in 1:ncol(targets)) {
    regressor <- randomForest(x = targets, y = targets[,j], importance = TRUE)
    rf.imp[,j] <- importance(regressor)[,1]/100.0
  }
  
  rownames(rf.imp) <- colnames(rf.imp) <- colnames(targets)
  
  dplot <- melt(rf.imp)
  
  plot_corr(dplot, "Random Forest Importance", out.path)
}

