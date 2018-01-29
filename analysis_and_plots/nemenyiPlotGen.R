library(scmamp)

# nemenyi <- read.csv("~/MEGA/Experimentos/exp_benchmarks_18dts/nemenyi_targets_MTs.csv")
nemenyi <- read.csv("~/Desktop/nemenyiperfold_rfs2.csv")
nemenyi <- nemenyi[,-1]
colnames(nemenyi) <- c("ST", "MTRS", "ERC", "DSTARS")
# colnames(nemenyi) <- c("ST-RF", "SST-RF", "ERC-RF", "DSTARS-RF", "ST-SVM", "SST-SVM", "ERC-SVM", "DSTARS-SVM", "MORF")
# rownames(nemenyi) <- nemenyi[,1]

# colnames(nemenyi) <- c("ST", "MTRS", "ERC", "DSTARS")
# colnames(nemenyi) <- c("ST-RF", "MTRS-RF", "ERC-RF", "DSTARS-RF", "ST-SVM", "MTRS-SVM", "ERC-SVM", "DSTARS-SVM", "ST-XGBoost", "MTRS-XGBoost", "ERC-XGBoost", "DSTARS-XGBoost", "ST-CART", "MTRS-CART", "ERC-CART", "DSTARS-CART")

pdf("~/Desktop/nemenyi_rfs2_perfold.pdf", width = 10, height = 5)
plotCD(nemenyi, cex = 0.75, decreasing = F)
dev.off()

friedmanTest(nemenyi)
nemenyiTest(nemenyi)


# library(scmamp)
#
# nemenyi <- read.csv("~/MEGA/Experimentos/exp_benchmarks_18dts/nemenyi_mem2.csv")
# rownames(nemenyi) <- nemenyi[,1]
# nemenyi <- nemenyi[,-1]
#
# colnames(nemenyi) <- c("ST", "MTRS", "ERC", "MOTC")
#
# pdf("~/Desktop/nemenyi18dts_mem.pdf", width = 10, height = 5)
# plotCD(nemenyi, cex = 0.5, decreasing = F)
# dev.off()
#
# friedmanTest(nemenyi)
# nemenyiTest(nemenyi)
