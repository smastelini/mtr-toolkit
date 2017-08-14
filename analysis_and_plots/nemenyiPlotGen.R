library(scmamp)

nemenyi <- read.csv("~/MEGA/Experimentos/exp_benchmarks_18dts/nemenyi_comparison.csv")
rownames(nemenyi) <- nemenyi[,1]
nemenyi <- nemenyi[,-1]

colnames(nemenyi) <- c("ST-RF", "MTRS-RF", "ERC-RF", "MOTC-RF", "ST-SVM", "MTRS-SVM", "ERC-SVM", "MOTC-SVM", "ST-XGBoost",
                        "MTRS-XGBoost", "ERC-XGBoost", "MOTC-XGBoost", "ST-CART", "MTRS-CART", "ERC-CART", "MOTC-CART")
plotCD(nemenyi, cex = 0.5, decreasing = F)
friedmanTest(nemenyi)
nemenyiTest(nemenyi)
