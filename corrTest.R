rm(list = ls())

library(reshape2)
library(ggplot2)

dataset.folder <- "~/MT_datasets"
bases <- c("atp1d", "atp7d")
n.targets <- c(6, 6)


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


for(b in 1:length(bases)) {
  dplot <- data.frame(matrix(nrow=0,ncol=4), stringsAsFactors = F)
  colnames(dplot) <- NA
  
  
  dataset <- read.csv(paste0(dataset.folder, "/", bases[b], ".csv"))
  targets <- dataset[,(ncol(dataset)-n.targets[b]+1):ncol(dataset)]
  colnames(targets) <- paste("Target", 1:n.targets[b])
  
  
  aux <- melt(get_lower_tri(cor(targets, method = "pearson")), na.rm = T)
  aux <- cbind(aux, rep("Pearson", nrow(aux)))
  colnames(aux) <- NA
  dplot <- rbind(dplot, aux)
  
  aux <- melt(get_lower_tri(cor(targets, method = "spearman")), na.rm = T)
  aux <- cbind(aux, rep("Spearman", nrow(aux)))
  colnames(aux) <- NA
  dplot <- rbind(dplot, aux)
  
  colnames(dplot) <- c("Targetsx", "Targetsy", "value", "Test")
  
  ggplot(data = dplot, aes(x=Targetsx, y=Targetsy, fill = value)) +
    geom_tile(color = "white") + facet_grid(. ~ Test)+
    scale_fill_gradient2(mid = "white", high = "black", limit = c(0,1), space = "Lab", 
                         name="Correlation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 0, vjust = 1, 
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
      legend.justification = c(1, 1),
      legend.position = "none", strip.text.x = element_text(size = 20)) 
  
  ggsave(file=paste0("~/Desktop/", bases[b], "_corr.pdf"), width = 10, height = 5)
}

