orc.importance.tech <- "rf_imp"
delta <- 10e-7

dir.create(paste0(output.dir.orc, "/prediction_logs/", tech), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.orc, "/out_imp_assessment/", tech), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(output.dir.orc, "/raw_logs/", tech), showWarnings = FALSE, recursive = TRUE)

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
	chain$leaves <- list()

	letitchaining <- function(target = tar, node.id = 1, level = 1) {
		chain$hash[node.id] <- target
		chain$leaves[node.id] <- FALSE

		max.i <- which.max(chain$imp[target,])
		if(level < chain$max.level && !is.infinite(chain$imp[target,max.i])) {
			# filter relevant targets
			if(is.null(chain$hb))
				rel.idx <- which(chain$imp[target,] > 0)
			else
				rel.idx <- which(chain$imp[target,] >= chain$imp[target, max.i] - chain$hb)
			rel <- colnames(chain$imp)[rel.idx]

			next.t <- node.id + 1
			for(r in rel) {
				chain$tree <- rbindlist(list(chain$tree, list(orig=node.id, dest=next.t)))
				next.t <- letitchaining(r, next.t, level+1)
			}
			return(next.t)
		} else {
			chain$tree <- rbindlist(list(chain$tree, list(orig=node.id, dest=NA)))
			chain$leaves[node.id] <- TRUE
			return(node.id + 1)
		}
	}
	letitchaining()

	retr <- list(tree = chain$tree, hash = as.character(chain$hash), leaves = as.logical(chain$leaves), depth = chain$max.level)
	rm(chain)
	return(retr)
}

getPrintableChainTree <- function(orc) {
	len.tree <- nrow(orc$tree)
	prtbl <- new.env()
	prtbl$tree <- data.table(orig=character(len.tree), dest=character(len.tree))

	sapply(1:len.tree, function(idx, ptree, tree, hash) {
		prtbl$tree[idx,1] <- paste(tree[idx,orig], hash[tree[idx,orig]], sep = ".")
		if(!is.na(tree[idx,dest]))
			prtbl$tree[idx,2] <- paste(tree[idx,dest], hash[tree[idx,dest]], sep = ".")
	}, tree = orc$tree, hash = orc$hash)

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
		timportance <- getTargetImportance(y.train, orc.importance.tech)
		write.csv(timportance, paste0(output.dir.orc, "/out_imp_assessment/", tech, "/", bases[i], "_importance_fold",
			formatC(k, width=2, flag="0"), ".csv"))
		########################################################################################################

		logs.tr <- y.train
		logs.ts <- y.test

		leaves.tr <- as.data.table(matrix(nrow=nrow(x.train), ncol=n.targets[i], NA))
		leaves.ts <- as.data.table(matrix(nrow=nrow(x.test), ncol=n.targets[i], NA))
		trained.leaves <- rep(FALSE, n.targets[i])

		names(leaves.tr) <- names(leaves.ts) <- names(trained.leaves) <- targets[[i]]


		orc.max.depth <- round(ifelse(n.targets[i] >= 6, log2(n.targets[i]+1), 2*log2(n.targets[i]+1)))


		aux.i <- timportance
		diag(aux.i) <- 0
		sum.imps <- apply(aux.i, 2, sum)
		ord <- order(sum.imps)
		t.ordered <- targets[[i]][ord]

		hb <- hoeffding.bound(n.targets[i] * nrow(x.train), range = max(timportance), delta = delta)
		# hb <- NULL

		t.cont <- 1
		model.num <- 0

		for(t in t.ordered) {
			orc <- getChainingTree(timportance, t, hb, orc.max.depth)

			write.csv(getPrintableChainTree(orc), paste0(output.dir.orc, "/out_imp_assessment/", tech, "/",
				bases[i], "_chain_tree_fold", formatC(k, width=2, flag="0"), "_T",
					formatC(t.cont, width=2, flag="0"), ".csv"), row.names = FALSE)

			raw.tr <- data.table(tmp=y.train[[t]])
			raw.ts <- data.table(tmp=y.test[[t]])
			names(raw.tr) <- names(raw.ts) <- t

			leaves <- which(orc$leaves)

			rc.count <- 1

			lastt.rc <- c()
			for(l in leaves) {
				# The desired target was already predicted by another CT
				if(paste0(orc$hash[l], ".pred") %in% names(logs.tr)) {
					preds.tr <- logs.tr[[paste0(orc$hash[l], ".pred")]]
					preds.ts <- logs.ts[[paste0(orc$hash[l], ".pred")]]
				} else if(trained.leaves[orc$hash[l]]) {
					preds.tr <- leaves.tr[[orc$hash[l]]]
					preds.ts <- leaves.ts[[orc$hash[l]]]
				} else { # Leaf model does not exist -> needs to be induced
					regressor <- train_(x.train, y.train[[orc$hash[l]]], tech, targets[[i]])
					# Updates models count
					model.num <- model.num + 1
					preds.tr <- predict_(regressor, x.train, tech, targets[[i]])
					preds.ts <- predict_(regressor, x.test, tech, targets[[i]])

					set(leaves.tr, NULL, orc$hash[l], preds.tr)
					set(leaves.ts, NULL, orc$hash[l], preds.ts)

					trained.leaves[orc$hash[l]] <- TRUE
				}

				rc.pos <- 1
				set(raw.tr, NULL, paste0("RC", formatC(rc.count, width=3, flag="0"), ".",
					formatC(rc.pos, width=2, flag="0"), ".", orc$hash[l]), preds.tr)
				set(raw.ts, NULL, paste0("RC", formatC(rc.count, width=3, flag="0"), ".",
					formatC(rc.pos, width=2, flag="0"), ".", orc$hash[l]), preds.ts)
				# This node is the root node
				if(l == 1) {
					set(logs.tr, NULL, paste0(orc$hash[1], ".pred"), preds.tr)
					set(logs.ts, NULL, paste0(orc$hash[1], ".pred"), preds.ts)
				} else {
					xtr <- x.train
					xts <- x.test
					set(xtr, NULL, orc$hash[l], preds.tr)
					set(xts, NULL, orc$hash[l], preds.ts)


					f.n <- unlist(orc$tree[which(orc$tree$dest == l), 1], use.names=FALSE)
					while(length(f.n) > 0) {
						regressor <- train_(xtr, y.train[[orc$hash[f.n]]], tech, targets[[i]])
						# Updates models count
						model.num <- model.num + 1
						preds.tr <- predict_(regressor, xtr, tech, targets[[i]])
						preds.ts <- predict_(regressor, xts, tech, targets[[i]])

						set(xtr, NULL, orc$hash[f.n], preds.tr)
						set(xts, NULL, orc$hash[f.n], preds.ts)

						rc.pos <- rc.pos + 1

						set(raw.tr, NULL, paste0("RC", formatC(rc.count, width=3, flag="0"), ".",
							formatC(rc.pos, width=2, flag="0"), ".", orc$hash[f.n]), preds.tr)
						set(raw.ts, NULL, paste0("RC", formatC(rc.count, width=3, flag="0"), ".",
							formatC(rc.pos, width=2, flag="0"), ".", orc$hash[f.n]), preds.ts)

						f.n <- unlist(orc$tree[which(orc$tree$dest == f.n), 1], use.names=FALSE)
					}

					lastt.rc <- c(lastt.rc, rc.pos)
				}
				rc.count <- rc.count + 1
			}


			if(nrow(orc$tree) > 1) {
				cols.n <- paste0("RC", formatC(seq(rc.count - 1), width=3, flag="0"), ".",
							formatC(lastt.rc, width=2, flag="0"), ".", orc$hash[1])

				preds.tr <- rowMeans(raw.tr[, cols.n, with=FALSE])
				preds.ts <- rowMeans(raw.ts[, cols.n, with=FALSE])

				set(raw.tr, NULL, paste0("root.", t), preds.tr)
				set(raw.ts, NULL, paste0("root.", t), preds.ts)

				set(logs.tr, NULL, paste0(t, ".pred"), preds.tr)
				set(logs.ts, NULL, paste0(t, ".pred"), preds.ts)
			}

			write.csv(data.frame(id=sample.names[train.idx], raw.tr, check.names = F),
				paste0(output.dir.orc, "/raw_logs/", tech, "/raw_ORC_training_",
					bases[i], "_fold", formatC(k, width=2, flag="0"), "_T",
						formatC(t.cont, width=2, flag="0"), ".csv"), row.names = FALSE)

			write.csv(data.frame(id=sample.names[test.idx], raw.ts, check.names = F),
				paste0(output.dir.orc, "/raw_logs/", tech, "/raw_ORC_testing_",
					bases[i], "_fold", formatC(k, width=2, flag="0"), "_T",
						formatC(t.cont, width=2, flag="0"), ".csv"), row.names = FALSE)

			t.cont <- t.cont + 1
		}

		# Save the model accountage
		set(model.count, k, "model_count", model.num)

		write.csv(data.frame(id=sample.names[test.idx], logs.ts, check.names = F),
			paste0(output.dir.orc, "/prediction_logs/", tech,"/predictions_ORC_", bases[i],
				paste0("_fold", formatC(k, width=2, flag="0")), ".csv"),
					row.names = FALSE)
	}

	rbindlist(list(model.count, list(NA, mean(model.count[, model_count]))))
	write.csv(model.count, paste0(output.dir.orc, "/out_imp_assessment/", tech, "/",
		bases[i], "_model_count.csv"), row.names = FALSE)
}

#Performance metrics
actual.folder <- getwd()
setwd(paste0(output.dir.orc, "/prediction_logs"))
i <<- 1

lapply(bases, function(b) {
	names.perf.log <- c("aCC", "ARE", "MSE", "aRMSE", "aRRMSE", paste0("R2.", targets[[i]]), paste0("RRMSE.", targets[[i]]), paste0("RMSE.", targets[[i]]))
	performance.log <<- data.frame(algorithm=character(0), as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
												simplify = F), names.perf.log)), stringsAsFactors = FALSE)

	folds.log <<- as.data.frame(setNames(replicate(length(names.perf.log),numeric(0),
										simplify = F), names.perf.log), stringsAsFactors = FALSE)
	lapply(1:folds.num, function(k) {
		log <- read.csv(paste0(getwd(),"/", tech, "/predictions_ORC_", b, paste0("_fold", formatC(k, width=2, flag="0")),".csv"), header=TRUE)
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

	write.csv(performance.log, paste0("../performance_ORC_", tech, "_", b, ".csv"), row.names = FALSE)
	i <<- i + 1
})
setwd(actual.folder)
