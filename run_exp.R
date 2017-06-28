rm(list = ls())
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
suppressMessages(library(data.table))
suppressMessages(library(kernlab))
suppressMessages(library(ranger))
#Extra libs
source("utils_MT.R")
# DSTARS -> Default
n.folds.tracking <- 10
dstars.delta <- 0.0001
#DRS -> Default
number.layers <- 10

#Loads configuration file
source("config.R")

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

for(tech in techs) {
	if(tech == "parrf") {
		suppressMessages(library(foreach))
		suppressMessages(library(doSNOW))
		registerDoSNOW(makeCluster(7, type="SOCK"))
	}

  if(tech == "h2o.rf") {
    suppressMessages(library(h2o))
    h2o.init(nthreads = -1,
      max_mem_size = "6G")
  }
	#Make an experiment
	for(mt in mt.techs) {
		set.seed(5465)
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

			write.csv(merged, paste0(comparison.folder, "/performance_", MT, "_", base, ".csv"), row.names = FALSE)
		})
	})
}

if(generate.final.table) {
	tabela <- data.frame(alg=character(0), aCC=numeric(0), ARE=numeric(0), MSE=numeric(0),
					aRMSE=numeric(0), aRRMSE = numeric(0), mean.R2 = numeric(0), stringsAsFactors = F)
	b <- 1
	for(i in bases) {
		for(mt in mt.techs) {
			tabela[nrow(tabela)+1,1] <- paste0("#",i," -> ",mt)
			log <- read.csv(paste0(paste0(output.prefix, "/comparison_results"), "/performance_", mt, "_", i, ".csv"), stringsAsFactors = F)
			rownames(log) <- log[,1]
			for(a in techs) {
				tabela[nrow(tabela)+1,1] <- a
				tabela[nrow(tabela),2:ncol(tabela)] <- c(log[a,2:6], mean(as.numeric(log[a,7:(6+n.targets[b])])))
			}
		}
		b <- b + 1
		tabela[nrow(tabela)+1,1] <- ""
	}
	write.csv(tabela, paste0(output.prefix, "/final_table.csv"), na = "", row.names = F)
}
