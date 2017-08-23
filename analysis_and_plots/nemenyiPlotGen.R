library(scmamp)

nemenyi <- read.csv("~/MEGA/Experimentos/exp_benchmarks_18dts/nemenyi_comparison.csv")
rownames(nemenyi) <- nemenyi[,1]
nemenyi <- nemenyi[,-1]

colnames(nemenyi) <- c("ST-RF", "MTRS-RF", "ERC-RF", "MOTC-RF", "ST-SVM", "MTRS-SVM", "ERC-SVM", "MOTC-SVM", "ST-XGBoost",
                        "MTRS-XGBoost", "ERC-XGBoost", "MOTC-XGBoost", "ST-CART", "MTRS-CART", "ERC-CART", "MOTC-CART")

pdf("~/Desktop/nemenyi18dts.pdf", width = 10, height = 5)
plotCD(nemenyi, cex = 0.5, decreasing = F)
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
