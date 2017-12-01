# datasets <- c("atp1d","atp7d","oes97","oes10","rf1","rf2","scm1d","scm20d","edm","sf1","sf2","jura","wq","enb","slump","andro","osales","scpf")
# n.targets <- c(6,6,16,16,8,8,16,16,2,3,3,3,14,2,3,6,12,3)
datasets <- c("atp1d","atp7d","oes97","oes10")
n.targets <- c(6,6,16,16)

mt.techs <- c("ST", "MTRS", "ERC", "DSTARST")
techs <- c("ranger", "svm", "xgboost", "cart")

datasets.f <- "~/MEGA/MT_datasets"
logf <- "~/MEGA/Experimentos/exp_benchmarks_18dts"
where.save <- "~/Desktop"



names(n.targets) <- datasets
phi.seq <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

phi <- matrix(nrow=length(datasets), ncol=length(techs))

rownames(phi) <- datasets
colnames(phi) <- techs

for(d in datasets) {
  for(tech in techs) {
    result <- read.csv(paste0(logf, "/DSTARST/performance_DSTARST_", tech, "_", d, ".csv"))

    phi[d, tech] <- phi.seq[which.min(result[, "aRRMSE"])]
  }
}

write.csv(phi, paste0(where.save, "/phi_values.csv"), row.names = TRUE)

n.folds <- 10

RRMSE <- function(actual, predicted) {
	num <- sum((actual - predicted)^2)
	den <- sum((actual - mean(actual))^2)
	den <- ifelse(den == 0, 1, den)
	return(sqrt(num/den))
}

targets <- list()
row.n <- c()

dataset.c <- 1
for(d in datasets) {
  dataset <- read.csv(paste0(datasets.f, "/", d, ".csv"))
  targets[[d]] <- colnames(dataset)[(ncol(dataset) - n.targets[dataset.c] + 1):ncol(dataset)]

  row.n <- c(row.n, paste0("all_", d), targets[[d]])

  dataset.c <- dataset.c + 1
}

col.n <- apply(expand.grid(mt.techs, techs), 1, paste, collapse=".")

# Error table
ft <- data.frame(matrix(nrow=sum(n.targets + 1), ncol=length(col.n)))
colnames(ft) <- col.n
rownames(ft) <- row.n

init <- 1
for(d in datasets) {
  full.names <- c(paste0("all_", d), targets[[d]])

  for(mt in mt.techs) {
    for(tech in techs) {
      foldl <- matrix(nrow=n.folds, ncol=length(targets[[d]]))
      colnames(foldl) <- targets[[d]]

      for(k in seq(n.folds)) {
        if(mt != "DSTARST") {
          log <- read.csv(paste0(logf, "/", mt, "/prediction_logs/", tech, "/predictions_", mt, "_", d, "_fold", formatC(k, width=2, flag="0"), ".csv"))
        } else {
          log <- read.csv(paste0(logf, "/DSTARST/output_logs/testing_final_logs/phi=", phi[d, tech], "/", d, "_", tech, "_testing_final_predictions_fold", formatC(k, width=2, flag="0"), ".csv"))
        }

        for(t in targets[[d]]) {
          foldl[k, t] <- RRMSE(log[, t], log[, paste0(t, ".pred")])
        }
      }

      errors <- colMeans(foldl)
      errors <- c(mean(errors), errors)

      ft[full.names, paste(mt, tech, sep=".")] <- errors
    }
  }
}

write.csv(ft, paste0(where.save, "/complete_table.csv"), row.names=TRUE)

# DSTARS layer account table
layers <- data.frame(matrix(nrow=sum(n.targets + 1), ncol=length(techs)))
rownames(layers) <- row.n
colnames(layers) <- techs

for(d in datasets) {
  full.names <- c(paste0("all_", d), targets[[d]])
  for(tech in techs) {
    lcount <- rep(0, n.targets[d])
    for(k in seq(n.folds)) {
      accnt <- read.csv(paste0(logf, "/DSTARST/output_logs/convergence_layers_logs/phi=", phi[d, tech], "/", d, "_", tech, "_convergence_layers_EV_fold_", formatC(k, width=2, flag="0"), ".csv"))
      lcount <- lcount + as.numeric(accnt[nrow(accnt), -1]) + 1
    }
    lcount <- lcount/n.folds
    lcount <- c(mean(lcount), lcount)

    layers[full.names, tech] <- lcount
  }
}

write.csv(layers, paste0(where.save, "/complete_layers.csv"), row.names=TRUE)
