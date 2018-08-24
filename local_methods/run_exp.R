rm(list = ls())
suppressMessages(library(data.table))
# SVM
suppressMessages(library(e1071))
# MLP
suppressMessages(library(RSNNS))
suppressMessages(library(plyr))
# GBM
suppressMessages(library(gbm))
# PLS
suppressMessages(library(pls))
# CART
suppressMessages(library(rpart))
# RIDGE REGRESSION
suppressMessages(library(MASS))
# RF
suppressMessages(library(randomForest))
# XGBoost
suppressMessages(library(xgboost))
# Calling of multiple techniques
suppressMessages(library(caret))
# Auxiliary functions
suppressMessages(library(BBmisc))
# Auxiliary functions
suppressMessages(library(permute))
# suppressMessages(library(kernlab))
# RF optimized
suppressMessages(library(ranger))
#Extra libs
source("../utils_and_includes/utils_MT.R")
# DSTARST and DSTARS -> Defaults
# Used only by DSTARST
n.folds.tracking <- 10
# Used by both DSTARS* versions
dstars.delta <- 0.0001

#DRS -> Default
number.layers <- 10

args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0) {
  stop("At least one argument must be supplied (input file)", call.=FALSE)
}

#Loads configuration file
# source("teste_config.R")
source(args[1])

# exp.random.seeds <- sample(99999, length(bases))
exp.random.seeds <- rep(5465, length(bases))

cat("Generated random seeds:\n")
cat(paste0(paste(exp.random.seeds, collapse="-"), "\n"))

#Max. PLS components to evaluate
train.test$comp.limit <- 40

# Creates output directories
for(mt in mt.techs) {
	assign(paste0("output.dir.", tolower(mt)), paste0(output.prefix, "/", mt))
}

# Performs evaluation
for(mt in mt.techs) {
  cat("\n\n####################################\n\n")
	cat(paste0("                ", mt))
  cat("\n\n####################################\n\n")
  if(mt != "DSTARST") {
    for(tech in techs) {
      cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
    	cat(paste0("               ", tech))
      cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n")
      source(paste0(mt, ".R"))
    }
  } else {
    prefix.folder.dstarst <- output.dir.dstarst
    for(dstars.delta in dstars.epsilons) {
      cat(paste0("Epsilon = ", dstars.delta, "\n"))
      output.dir.dstarst <- paste0(prefix.folder.dstarst, "_eps_", dstars.delta)
      for(tech in techs) {
        source("DSTARST.R")
      }
    }
  }
}

if(must.compare) {
	comparison.folder <- paste0(output.prefix, "/comparison_results")
	dir.create(comparison.folder, showWarnings = FALSE, recursive = TRUE)
	sapply(mt.techs, function(MT) {
		sapply(bases, function(base) {
			merged <<- data.frame()
			sapply(techs, function(tech) {
				if(MT == "DRS")
					to.merge <- read.csv(paste0(paste0(output.prefix, "/", MT), "/performance_", MT, "_", tech, "_", base, "_L", number.layers, ".csv"), stringsAsFactors = F)
				else if(MT == "DSTARST") {
          to.merge <- data.frame()
          for(epsilon in dstars.epsilons) {
            eps.aux <- read.csv(paste0(paste0(output.prefix, "/", MT, "_eps_", epsilon), "/performance_", MT, "_", tech, "_", base, ".csv"), stringsAsFactors = F)
            best.phi <- which.min(eps.aux[, "aRRMSE"])
            to.merge <- rbind(to.merge, eps.aux[best.phi,])
          }
        } else {
					to.merge <- read.csv(paste0(paste0(output.prefix, "/", MT), "/performance_", MT, "_", tech, "_", base, ".csv"), stringsAsFactors = F)
        }

				merged <<- rbind(merged, to.merge)
			})
			write.csv(merged, paste0(comparison.folder, "/performance_", MT, "_", base, ".csv"), row.names = FALSE)
		})
	})
}

if(generate.final.table) {
	col.num <- 2*max(n.targets) + 6
	tabela <- data.frame(matrix(nrow=0, ncol=col.num), stringsAsFactors = F)
	b <- 1
	for(i in bases) {
		cnt <- 1
		for(mt in mt.techs) {
			log <- read.csv(paste0(paste0(output.prefix, "/comparison_results"), "/performance_", mt, "_", i, ".csv"), stringsAsFactors = F)
			if(cnt == 1) {
				c.nms <- c("Method/Algorithm", colnames(log)[2:ncol(log)])
				tabela[nrow(tabela) + 1, 1:length(c.nms)] <- c.nms
			}
			tabela[nrow(tabela)+1,1] <- paste0("#", i, " -> ", mt)

			for(a in seq(nrow(log))) {
				tabela[nrow(tabela)+1, 1:ncol(log)] <- log[a,]
			}
			cnt <- cnt + 1
		}
		b <- b + 1
		tabela[nrow(tabela)+1,1] <- ""
	}
	write.table(tabela, paste0(output.prefix, "/final_table.csv"), na = "", row.names = F, col.names = F, quote = F, sep = ",")
}

if(generate.nemenyi.frame) {
  nemenyi.cols <- apply(expand.grid(mt.techs, techs), 1, paste, collapse=".")

  nemenyi <- data.frame(matrix(nrow=length(bases), ncol = length(nemenyi.cols)))
  colnames(nemenyi) <- nemenyi.cols
  rownames(nemenyi) <- bases

  for(b in seq(length(bases))) {
    for(mt in mt.techs) {
      log <- read.csv(paste0(paste0(output.prefix, "/comparison_results"), "/performance_", mt, "_", bases[b], ".csv"), stringsAsFactors = F)
      for(i in seq(length(techs))) {
        nemenyi[b, paste(mt, techs[i], sep = ".")] <- log[i, "aRRMSE"]
      }
    }
  }
  write.csv(nemenyi, paste0(output.prefix, "/nemenyi_comparison.csv"), row.names = T)
}
