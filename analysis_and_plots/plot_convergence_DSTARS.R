logs.folder <- "~/MEGA/Experimentos/exp_A1-ExpertSystems/outputP/DSTARST/output_logs"
out.folder <- "~/MEGA/Experimentos/exp_A1-ExpertSystems/outputP/convergence_plot"
n.targets <- 6
base.learners <- c("svm", "lr")
# phi.values <- c(0,0,0.2)
# phi.values <- c(0.8,0.5,0.9)
phi.values <- c(0.5,0.9)
dataset.name <- "base_WB_TR"
fold.number <- 1

######################################################################

RMSE <- function(actual, predicted) {
  return(sqrt(mean((actual-predicted)^2)))
}

compute.rmse <- function(result.log, targets, tracking) {
  result <- as.data.frame(matrix(nrow = nrow(tracking), ncol = length(targets), data = NA))
  colnames(result) <- targets
  # rownames(result) <- rownames(tracking)
  for(t in targets) {
    result[tracking[,t],t] <- sapply(which(tracking[,t])-1, function(x, log, target) {
      RMSE(log[,target], log[,paste0("X",x,".", target)])
    }, log = result.log, target = t)
  }

  return(result)
}

phi.idx <- 1
for(learner in base.learners) {
  modelling <- read.csv(paste0(logs.folder, "/modelling_raw_logs/phi=", phi.values[phi.idx], "/", dataset.name, "_", learner, "_modelling_predictions_fold", formatC(fold.number, width=2, flag="0"), ".csv"))[,-1]
  testing <- read.csv(paste0(logs.folder, "/testing_raw_logs/phi=", phi.values[phi.idx], "/", dataset.name, "_", learner, "_testing_predictions_fold", formatC(fold.number, width=2, flag="0"), ".csv"))[,-1]
  convergence.tracking <- read.csv(paste0(logs.folder, "/convergence_layers_logs/phi=", phi.values[phi.idx], "/", dataset.name, "_", learner, "_convergence_tracking_EV_fold_", formatC(fold.number, width=2, flag="0"), ".csv"))[,-1]

  targets <- colnames(testing)[1:n.targets]
  # targets.translated <- c("pH", "L*", "a*", "b*", "WHC")

  max.row.tracking <- max(sapply(convergence.tracking, function(x) BBmisc::which.last(x)))
  convergence.tracking <- convergence.tracking[1:max.row.tracking,]
  rownames(convergence.tracking) <- 0:(nrow(convergence.tracking)-1)

  rmse.modelling <- compute.rmse(modelling, targets, convergence.tracking)
  rmse.testing <- compute.rmse(testing, targets, convergence.tracking)
  max.error <- ceiling(max(rmse.modelling, rmse.testing, na.rm = T)*10)/10
  
  
  pdf(file = paste0(out.folder, "/", dataset.name, "_", learner, ".pdf"), width = 10, height = 5)
  layout(matrix(c(1:n.targets, if(n.targets %% 2 == 0) rep(n.targets+1, 2) else c(n.targets+1,rep(n.targets+2, 2))), ncol=2, byrow = T), 
         heights=c(rep(0.9/ceiling(n.targets/2), ceiling(n.targets/2)), 0.1), 
         respect=FALSE)
  par(mar = c(0,0,0.55,0), oma=c(0.5, 4.5, 0.3, 0.5), las=1)

  counter <- 1
  for(t in targets) {
    plot(1, ann=FALSE, xlim = c(0,max.row.tracking), ylim = c(0,max.error), axes = FALSE, frame.plot=TRUE)
    axis(side = 1,
         at=if(counter >= n.targets - 1) 0:max.row.tracking else FALSE,
         labels = if(counter >= n.targets - 1) 0:max.row.tracking else FALSE)
  
    text(4.9, 0, targets[counter], cex = 1, pos = 3, offset = 9)
    
    error.seq <- seq(0,max.error, 0.05)
    axis(side = 2, at = if(counter %% 2 > 0) error.seq else FALSE, labels = if(counter %% 2 > 0) format(error.seq, nsmall=2) else FALSE)
    
    if(max(which(convergence.tracking[,t])-1) > 0) {
      for(coord.x in 0:(max(which(convergence.tracking[,t])-1)-1)) {
        x0 <- coord.x
        if(is.na(rmse.modelling[coord.x+2,t]))
          next
        y0.m <- rmse.modelling[x0+1,t]
        y0.t <- rmse.testing[x0+1,t]
        x1 <- x0 + 1
        y1.m <- rmse.modelling[x1+1,t]
        y1.t <- rmse.testing[x1+1,t]
    
        segments(x0,y0.m,x1,y1.m)
        segments(x0,y0.t,x1,y1.t)
      }
    }
    points(x = which(convergence.tracking[,t])-1, y = rmse.modelling[convergence.tracking[,t],t], pch = 2)
    points(x = which(convergence.tracking[,t])-1, y = rmse.testing[convergence.tracking[,t],t], pch = 15)
    abline(v=0, lty=3)
    text(0.1, 0.005, "ST", cex = 0.8)
    abline(v=1, lty=2)
    text(1.18, 0.005, "MTRS", cex = 0.8)
    abline(v=max(which(convergence.tracking[,t])-1))
    text(max(which(convergence.tracking[,t])-1) + 0.23, if(max(which(convergence.tracking[,t])-1) > 1) 0.005 else 0.02, "DSTARS", cex = 0.8, pos = 3, offset = -0.2)
    counter <- counter + 1
  }
  
  if(n.targets %% 2 > 0) {
    plot(1, ann=FALSE, xlim = c(0,max.row.tracking), ylim = c(0,max.error), axes = FALSE, frame.plot=FALSE)
  }

  mtext("Layer", 1, -1.5, outer=TRUE, las = 0)
  mtext("RMSE", 2, 2.7, outer=TRUE, las=0)
  par(mar=c(0,0,2.15,0), xpd=NA)
  plot(0, type = "n", axes=FALSE, xlab="", ylab="")
  legend("topright", legend = c("Modelling", "Testing"), pch = c(2,15), lty = c(1,1), horiz = T)
  dev.off()
  phi.idx <- phi.idx + 1
}