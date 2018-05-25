rm(list = ls())
suppressMessages(library(data.table))
suppressMessages(library(mtrToolkit))
suppressMessages(library(e1071))
suppressMessages(library(RSNNS))
suppressMessages(library(plyr))
suppressMessages(library(gbm))
suppressMessages(library(pls))
suppressMessages(library(rpart))
suppressMessages(library(MASS))
suppressMessages(library(randomForest))
suppressMessages(library(xgboost))
suppressMessages(library(caret))
suppressMessages(library(BBmisc))
suppressMessages(library(permute))
suppressMessages(library(kernlab))
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

#Loads configuration file
source("config.R")

# exp.random.seeds <- sample(99999, length(bases))
exp.random.seeds <- rep(5465, length(bases))

print("Generated random seeds:")
print(exp.random.seeds)

#ProgressBar creation
if(showProgress){
  source("progress_exp.R")
}

#Max. PLS components to evaluate
train.test$comp.limit <- 40

# Creates output directories
for(mt in mt.techs) {
	assign(paste0("output.dir.", tolower(mt)), paste0(output.prefix, "/", mt))
}

for(mt in mt.techs) {
	print(mt)
	if(mt != "MORF") {
		for(tech in techs) {
			if(tech == "parrf") {
				suppressMessages(library(foreach))
				suppressMessages(library(doSNOW))
				registerDoSNOW(makeCluster(7, type="SOCK"))
			}
			source(paste0(mt, ".R"))
		}
	} else {
		source(paste0(mt, ".R"))
	}
}

if(must.compare) {
	# mt.techs <- gsub("DSTARST", "DSTARS", mt.techs)
	comparison.folder <- paste0(output.prefix, "/comparison_results")
	dir.create(comparison.folder, showWarnings = FALSE, recursive = TRUE)
	sapply(mt.techs, function(MT) {
		sapply(bases, function(base) {
			merged <<- data.frame()
			if(MT != "MORF") {
				sapply(techs, function(tech) {
					if(MT == "DRS")
						to.merge <- read.csv(paste0(paste0(output.prefix, "/", MT), "/performance_", MT, "_", tech, "_", base, "_L", number.layers, ".csv"), stringsAsFactors = F)
					else
						to.merge <- read.csv(paste0(paste0(output.prefix, "/", MT), "/performance_", MT, "_", tech, "_", base, ".csv"), stringsAsFactors = F)

        	if(MT == "DSTARST") {
          	best.phi <- which.min(to.merge[,"aRRMSE"])
          	to.merge <- to.merge[best.phi,]
       		}
					merged <<- rbind(merged, to.merge)
				# merged[nrow(merged),1] <<- tech
				})
			} else {
				to.merge <- read.csv(paste0(paste0(output.prefix, "/", MT), "/performance_", MT, "_", base, ".csv"), stringsAsFactors = F)
				merged <<- rbind(merged, to.merge)
			}

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

			if(mt != "MORF") {
				rownames(log) <- techs
				for(a in techs) {
					tabela[nrow(tabela)+1, 1:ncol(log)] <- log[a,]
				}
			} else {
				rownames(log) <- mt
				tabela[nrow(tabela)+1, 1:ncol(log)] <- log
			}
			cnt <- cnt + 1
		}	
		b <- b + 1
		tabela[nrow(tabela)+1,1] <- ""
	}
	write.table(tabela, paste0(output.prefix, "/final_table.csv"), na = "", row.names = F, col.names = F, quote = F, sep = ",")
}

if(generate.nemenyi.frame) {
  not.morf <- mt.techs != "MORF"
  nemenyi.cols <- apply(expand.grid(mt.techs[not.morf], techs), 1, paste, collapse=".")
  if("MORF" %in% mt.techs)
    nemenyi.cols <- c(nemenyi.cols, "MORF")

  nemenyi <- data.frame(matrix(nrow=length(bases), ncol = length(nemenyi.cols)))
  colnames(nemenyi) <- nemenyi.cols
  rownames(nemenyi) <- bases

  for(b in seq(length(bases))) {
    for(mt in mt.techs) {
      log <- read.csv(paste0(paste0(output.prefix, "/comparison_results"), "/performance_", mt, "_", bases[b], ".csv"), stringsAsFactors = F)

      if(mt != "MORF") {
        for(i in seq(length(techs))) {
          nemenyi[b, paste(mt, techs[i], sep = ".")] <- log[i, "aRRMSE"]
        }
      } else {
        nemenyi[b, mt] <- log[1, "aRRMSE"]
      }
    }
  }
  write.csv(nemenyi, paste0(output.prefix, "/nemenyi_comparison.csv"), row.names = T)
}
