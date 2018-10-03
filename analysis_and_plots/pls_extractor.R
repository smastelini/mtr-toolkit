library(pls)
library(data.table)
library(mixOmics)

include.testing.set <- TRUE

n.targets <- 10

datasets.folder <- "~/Documents/mtr-fluorescencia/dataset/"
dataset <-
  read.csv(
    paste0(datasets.folder, "data_train.csv"),
    stringsAsFactors = F,
    row.names = 1
  )

source("~/Documents/mtr-fluorescencia/utils_and_includes/utils_MT.R")

X <- dataset[, c(1:(ncol(dataset) - n.targets))]
X <- as.matrix(remove.unique(X))
Y <- as.matrix(dataset[, (ncol(dataset) - n.targets + 1):ncol(dataset)])

comprimentos <- c(5, 10, 15)


for (num.comp in comprimentos) {
  output <- matrix(nrow = nrow(dataset), ncol = num.comp, 0)
  for (i in 1:nrow(dataset)) {
    pls.m <- pls(X[-i, ], Y[-i, ], ncomp = num.comp)
    newdata <- t(as.matrix(X[i, ]))
    output[i, ] <- predict(pls.m, newdata)$variates
  }
  
  colnames(output) <- paste0("LV", 1:num.comp)
  output <- as.data.frame(output)
  
  final <- cbind(output, Y)
  write.csv(final,
            paste0(datasets.folder, "PLS", num.comp, ".csv"),
            row.names = F)
  
}

if (include.testing.set == TRUE) {
  pls.m <- pls(X[, ], Y[, ], ncomp = max(comprimentos))
  
  dataset <-
    read.csv(
      paste0(datasets.folder, "data_test.csv"),
      stringsAsFactors = F,
      row.names = 1
    )
  
  X <- dataset[, c(1:(ncol(dataset) - n.targets))]
  X <- as.matrix(remove.unique(X))
  Y <- dataset[, (ncol(dataset) - n.targets + 1):ncol(dataset)]
  
  for (num.comp in comprimentos) {
    output <- matrix(nrow = nrow(dataset), ncol = num.comp, 0)
    
    for (i in 1:nrow(dataset)) {
      newdata <- t(as.matrix(X[i, ]))
      output[i, ] <- predict(pls.m, newdata)$variates
    }
    
    colnames(output) <- paste0("LV", 1:num.comp)
    output <- as.data.frame(output)
    
    final <- cbind(output, Y)
    write.csv(final,
              paste0(datasets.folder, "PLS_test", num.comp, ".csv"),
              row.names = F)
  }
}
