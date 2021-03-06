motc.importance.tech <- "rf_imp"
delta <- 10e-7

dir.create(paste0(output.dir.motc, "/prediction_logs/",tech), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.motc, "/out_imp_assessment/",tech), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.motc, "/raw_logs/",tech), showWarnings = FALSE, recursive = TRUE)

hoeffding.bound <- function(observations, range, delta = 10^-6) {
	return(sqrt(((range^2)*log(1/delta))/(2*observations)))
}

getChainingTree <- function(imp, tar, hb, max.level) {
	chain <- new.env()
	chain$tree <- data.table(orig=numeric(0), dest=numeric(0))
	chain$hash <- list()
	chain$imp <- imp
	chain$hb <- hb
	chain$max.level <- max.level
	chain$leafs <- list()

	letitchaining <- function(target = tar, node.id = 1, level = 1) {
		chain$hash[node.id] <- target
		chain$leafs[node.id] <- FALSE

		max.i <- which.max(chain$imp[target,])
		if(level < chain$max.level && !is.infinite(chain$imp[target,max.i])) {
			# filter relevant targets
			if(is.null(chain$hb))
				rel.idx <- which(chain$imp[target,] > 0)
			else
				rel.idx <- which(chain$imp[target,]/chain$imp[target, max.i] >= 1 - chain$hb)
			rel <- colnames(chain$imp)[rel.idx]

			next.t <- node.id + 1
			for(r in rel) {
				chain$tree <- rbindlist(list(chain$tree, list(orig=node.id, dest=next.t)))
				next.t <- letitchaining(r, next.t, level+1)
			}
			return(next.t)
		} else {
			chain$tree <- rbindlist(list(chain$tree, list(orig=node.id, dest=NA)))
			chain$leafs[node.id] <- TRUE
			return(node.id + 1)
		}
	}
	letitchaining()
	return(list(tree = chain$tree, hash = as.character(chain$hash), leafs = as.logical(chain$leafs), depth = chain$max.level))
}

buildChainTree <- function(motc, x.train, y.train, x.test, tech, targets, t.id) {
	root.node <- 1
	bct <- new.env()
	# len.preds <- length(unique(motc$hash[motc$leafs])) + length(motc$hash[!motc$leafs])

	# bct$pred.tr <- data.table(matrix(nrow=nrow(x.train), ncol=len.preds))
	# bct$pred.ts <- data.table(matrix(nrow=nrow(x.test), ncol=len.preds))

	bct$xtr <- x.train
	bct$xts <- x.test
	bct$ytr <- y.train

	chainTravel <- function(t.node = 1, f.node = 0) {
		edg <- which(motc$tree$orig == t.node)

		# Leaf node
		if(length(edg) == 1 && is.na(motc$tree[edg,dest])) {
			# Verifies if the leaf node's ST model was already trained
			if(is.null(mp$leafs.tr[[paste0("l.", motc$hash[t.node])]]) &&
				is.null(mp$tr[[motc$hash[t.node]]])) {

				regressor <- train_(bct$xtr, bct$ytr[[motc$hash[t.node]]], tech, targets)
				mp$model.count <- mp$model.count + 1

				if(f.node == 0) {
					mp$tr[[motc$hash[t.node]]] <- predict_(regressor, bct$xtr, tech, targets)
					mp$ts[[motc$hash[t.node]]] <- predict_(regressor, bct$xts, tech, targets)
				} else {
					mp$leafs.tr[[paste0("l.", motc$hash[t.node])]] <- predict_(regressor, bct$xtr, tech, targets)
					mp$leafs.ts[[paste0("l.", motc$hash[t.node])]] <- predict_(regressor, bct$xts, tech, targets)
				}
			} else if(!is.null(mp$leafs.tr[[paste0("l.", motc$hash[t.node])]]) &&
				is.null(mp$tr[[motc$hash[t.node]]])) {
				mp$tr[[motc$hash[t.node]]] <- mp$leafs.tr[[paste0("l.", motc$hash[t.node])]]
				mp$ts[[motc$hash[t.node]]] <- mp$leafs.ts[[paste0("l.", motc$hash[t.node])]]
			}
		} else {
			for(e in edg)
				chainTravel(motc$tree[e,dest], t.node)

			leaf.sons <- motc$leafs[motc$tree[edg,dest]]
			sons.names <- motc$hash[motc$tree[edg,dest]]

			# Get training set augments
			augments.tr <- lapply(seq(leaf.sons), function(p, leaf, sonsn) {
				if(leaf[p]) {
					if(is.null(mp$tr[[sonsn[p]]]))
						return(mp$leafs.tr[[paste0("l.", sonsn[p])]])
					else
						return(mp$tr[[sonsn[p]]])
				} else
					return(mp$nodes.tr[[paste(t.id, t.node, sonsn[p], sep = ".")]])

			}, leaf = leaf.sons, sonsn = sons.names)

			# Get testing set augments
			augments.ts <- lapply(seq(leaf.sons), function(p, leaf, sonsn) {
				if(leaf[p]) {
					if(is.null(mp$ts[[sonsn[p]]]))
						return(mp$leafs.ts[[paste0("l.", sonsn[p])]])
					else
						return(mp$ts[[sonsn[p]]])
				} else
					return(mp$nodes.ts[[paste(t.id, t.node, sonsn[p], sep = ".")]])

			}, leaf = leaf.sons, sonsn = sons.names)

			# Make augmented sets
			set(bct$xtr, NULL, motc$hash[motc$tree[edg,dest]], augments.tr)
			set(bct$xts, NULL, motc$hash[motc$tree[edg,dest]], augments.ts)

			regressor <- train_(bct$xtr, bct$ytr[[motc$hash[t.node]]], tech, targets)
			mp$model.count <- mp$model.count + 1

			# Save predictions
			# Root
			if(f.node == 0) {
				mp$tr[[motc$hash[t.node]]] <-
					predict_(regressor, bct$xtr, tech, targets)
				mp$ts[[motc$hash[t.node]]] <-
					predict_(regressor, bct$xts, tech, targets)
			} else { # Other nodes
				mp$nodes.tr[[paste(t.id, f.node, motc$hash[t.node], sep = ".")]] <-
					predict_(regressor, bct$xtr, tech, targets)
				mp$nodes.ts[[paste(t.id, f.node, motc$hash[t.node], sep = ".")]] <-
					predict_(regressor, bct$xts, tech, targets)
			}

			# Remove augmented features
			bct$xtr[, motc$hash[motc$tree[edg,dest]] := NULL]
			bct$xts[, motc$hash[motc$tree[edg,dest]] := NULL]
		}
		return(NULL)
	}
	chainTravel()

	rm(bct)
	return(NULL)
}

getPrintableChainTree <- function(motc) {
	len.tree <- nrow(motc$tree)
	prtbl <- new.env()
	prtbl$tree <- data.table(orig=character(len.tree), dest=character(len.tree))

	sapply(1:len.tree, function(idx, ptree, tree, hash) {
		prtbl$tree[idx,1] <- paste(tree[idx,orig], hash[tree[idx,orig]], sep = ".")
		if(!is.na(tree[idx,dest]))
			prtbl$tree[idx,2] <- paste(tree[idx,dest], hash[tree[idx,dest]], sep = ".")
	}, tree = motc$tree, hash = motc$hash)

	return(prtbl$tree)
}

targets <- list()
centers <- list()
scales <- list()

for(i in 1:length(bases)) {
	set.seed(exp.random.seeds[i])
	dataset <- read.csv(paste0(datasets.folder, "/", bases[i], ".csv"))
	dataset <- remove.unique(dataset)

	targets[[i]] <- colnames(dataset)[(ncol(dataset)-n.targets[i]+1):ncol(dataset)]

	dataset <- dataset[sample(nrow(dataset)),]
	sample.names <- rownames(dataset)

	#Center and Scaling
	dataset <- as.data.table(dataset)
	invisible(dataset[, names(dataset) := lapply(.SD, as.numeric)])

	norm.params <- get.normalization.params(dataset, norm.method)
	centers[[i]] <- norm.params[["center"]]
	scales[[i]] <- norm.params[["scale"]]

	dataset <- as.data.table(scale(dataset, center = centers[[i]], scale = scales[[i]]))

	len.fold <- round(nrow(dataset)/folds.num)

	###################################Use a testing set#####################################
	if(length(bases.test) > 0 && folds.num == 1) {
		dataset.test <- read.csv(paste0(datasets.folder, "/", bases.test[i], ".csv"))
		dataset.test <- as.data.table(dataset.test[, colnames(dataset)])
		invisible(dataset.test[, names(dataset.test) := lapply(.SD, as.numeric)])

		dataset.test <- as.data.table(scale(dataset.test, center = centers[[i]], scale = scales[[i]]))
		init.bound <- nrow(dataset) + 1

		dataset <- rbindlist(list(dataset, dataset.test))
		sample.names <- c(sample.names, rownames(dataset.test))
	}
	#########################################################################################

	x <- dataset[, !targets[[i]], with = FALSE]
	y <- dataset[, targets[[i]], with = FALSE]

	cat(paste0(bases[i], "\n"))

	model.count <- data.table(fold = seq(folds.num), model_count = rep(0, folds.num))

	# Cross validation
	for(k in 1:folds.num) {
		cat(paste0("Fold ", k, "\n"))

		if(folds.num == 1) {
			if(length(bases.test) > 0) {
				train.idx <- 1:(init.bound-1)
				test.idx <- init.bound:nrow(dataset)
			} else {
				test.idx <- as.numeric(rownames(dataset))
				train.idx <- test.idx
			}
		} else {
			test.idx <- ((k-1)*len.fold + 1):(ifelse(k==folds.num, nrow(dataset), k*len.fold))
			train.idx <- setdiff(1:nrow(dataset), test.idx)
		}

		x.train <- remove.unique(x[train.idx])
		y.train <- y[train.idx]

		x.test <- x[test.idx, names(x.train), with = FALSE]
		y.test <- y[test.idx]

		###########################################Importance calc##############################################
		timportance <- getTargetImportance(y.train, motc.importance.tech)
		write.csv(timportance, paste0(output.dir.motc, "/out_imp_assessment/", tech, "/", bases[i], "_importance_fold", formatC(k, width=2, flag="0"), ".csv"))
		########################################################################################################

		t.names <- c(targets[[i]], paste0(targets[[i]], ".pred"))
		prediction.log <- as.data.table(setNames(replicate(length(t.names),numeric(nrow(x.test)), simplify = F), t.names))
		t.cont <- 1

		motc.max.depth <- round(ifelse(n.targets[i] >= 6, log2(n.targets[i]), 2*log2(n.targets[i])))		

		mp <- new.env()
		mp$tr <- list()
		mp$ts <- list()
		mp$nodes.tr <- list()
		mp$nodes.tr <- list()
		mp$leafs.tr <- list()
		mp$leafs.ts <- list()
		mp$model.count <- 0

		aux.i <- timportance
		diag(aux.i) <- 0
		sum.imps <- apply(aux.i, 2, sum)
		ord <- order(sum.imps)
		t.ordered <- targets[[i]][ord]

		hb <- hoeffding.bound(nrow(x.train), range = 1, delta = delta)
		# hb <- NULL

		for(t in t.ordered) {
				motc <- getChainingTree(timportance, t, hb, motc.max.depth)

				write.csv(getPrintableChainTree(motc), paste0(output.dir.motc, "/out_imp_assessment/", tech, "/",
					bases[i], "_chain_tree_fold", formatC(k, width=2, flag="0"), "_T",
					formatC(t.cont, width=2, flag="0"), ".csv"), row.names = FALSE)

				buildChainTree(motc, x.train, y.train, x.test, tech, targets[[i]], ord[t.cont])
				t.cont <- t.cont + 1
		}

		# Save the model accountage
		set(model.count, k, "model_count", mp$model.count)

		general.log.tr <- as.data.table(c(mp$leafs.tr, mp$nodes.tr, mp$tr))
		general.log.ts <- as.data.table(c(mp$leafs.ts, mp$nodes.ts, mp$ts))

		write.csv(data.frame(id=sample.names[train.idx], general.log.tr, check.names = F),
			paste0(output.dir.motc, "/raw_logs/", tech, "/raw_MOTC_training_",
				bases[i], "_fold", formatC(k, width=2, flag="0"), ".csv"),
			row.names = FALSE)

		write.csv(data.frame(id=sample.names[test.idx], general.log.ts, check.names = F),
			paste0(output.dir.motc, "/raw_logs/", tech, "/raw_MOTC_testing_",
				bases[i], "_fold", formatC(k, width=2, flag="0"), ".csv"),
			row.names = FALSE)

		for(t in targets[[i]]) {
			set(prediction.log, NULL, t, y.test[[t]])
			set(prediction.log, NULL, paste0(t, ".pred"), general.log.ts[[t]])
		}

		write.csv(data.frame(id=sample.names[test.idx], prediction.log, check.names = F),
			paste0(output.dir.motc, "/prediction_logs/", tech,"/predictions_MOTC_", bases[i],
				paste0("_fold", formatC(k, width=2, flag="0")),
			".csv"), row.names = FALSE)
	}

	rbindlist(list(model.count, list("mean", mean(model.count[, model_count]))))
	write.csv(model.count, paste0(output.dir.motc, "/out_imp_assessment/", tech, "/",
		bases[i], "_model_count.csv"), row.names = FALSE)

	rm(mp)
}

#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.motc, "/prediction_logs"))
i <<- 1

lapply(bases, function(b) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RRMSE.", targets[[i]]), paste0("RMSE.", targets[[i]]))
	performance.log <<- data.frame(algorithm=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
												simplify = F), names.perf.log)), stringsAsFactors = FALSE)

	folds.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
										simplify = F), names.perf.log), stringsAsFactors = FALSE)
	lapply(1:folds.num, function(k) {
		log <- read.csv(paste0(getwd(),"/", tech, "/predictions_MOTC_", b, paste0("_fold", formatC(k, width=2, flag="0")),".csv"), header=TRUE)
		folds.log[nrow(folds.log)+1, "aCC"] <<- aCC(log, targets[[i]])
		folds.log[nrow(folds.log), "ARE"] <<- ARE(log, targets[[i]])
		folds.log[nrow(folds.log), "MSE"] <<- MSE(log, targets[[i]])
		folds.log[nrow(folds.log), "aRMSE"] <<- aRMSE(log, targets[[i]])
		folds.log[nrow(folds.log), "aRRMSE"] <<- aRRMSE(log, targets[[i]])

		# targets
		for(t in targets[[i]]) {
			folds.log[nrow(folds.log), paste0("R2.", t)] <<- summary(lm(log[,t] ~ log[, paste0(t, ".pred")]))$r.squared

			r <- scales[[i]][t]*log[, t] + centers[[i]][t]
			p <- scales[[i]][t]*log[, paste0(t, ".pred")] + centers[[i]][t]

			folds.log[nrow(folds.log), paste0("RMSE.", t)] <<- RMSE(r, p)
			folds.log[nrow(folds.log), paste0("RRMSE.", t)] <<- RRMSE(r, p)
		}
	})
	performance.log[nrow(performance.log)+1, 1] <<- tech
	performance.log[nrow(performance.log), -1] <<- colMeans(folds.log)

	write.csv(performance.log, paste0("../performance_MOTC_", tech, "_", b, ".csv"), row.names = FALSE)
	i <<- i + 1
})
setwd(actual.folder)
