library(fmsb)

setwd("~/Documents/mtr-fluorescencia/results")

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)

###chose if relative ou absolute values
relative<-TRUE

regressors<-c("svm_lin", #"svm_poly", 
              "svm_rad", "ranger")
n.regressors<-length(regressors)

techs<-c("ST", "MTRS", "ERC", "MTAS", "MTSG", "ESR", "MOTC", "ORC", "DRS", "DSTARS", "DSTARST")
n.techs<-length(techs)

n.targets<-10

data.name<- "r2raw" 

data<-read.csv(paste0(data.name, ".csv"), header=TRUE,sep=",", dec = ",")[1:(n.regressors*n.techs),]
#this data should be displaced as follows: the columns corresponds to the boarders of the carts (targets); 
#the rows represent the combination of regressors (that will be represented by polygons) and tecniques (in the order: ST+rf, ST+svm, ST+cart, MTRS+rf, MTRS+svm....)


data1<-data[1:n.regressors,]
data2<-data[(n.regressors+1):(n.regressors*2),]
data3<-data[(n.regressors*2+1):(n.regressors*3),]
data4<-data[(n.regressors*3+1):(n.regressors*4),]
data5<-data[(n.regressors*4+1):(n.regressors*5),]
data6<-data[(n.regressors*5+1):(n.regressors*6),]
data7<-data[(n.regressors*6+1):(n.regressors*7),]
data8<-data[(n.regressors*7+1):(n.regressors*8),]
data9<-data[(n.regressors*8+1):(n.regressors*9),]
data10<-data[(n.regressors*9+1):(n.regressors*10),]
data11<-data[(n.regressors*10+1):(n.regressors*11),]

data.rp1<- data1/data2
data.rp2<- data1/data3
data.rp3<- data1/data4
data.rp4<- data1/data5
data.rp5<- data1/data6
data.rp6<- data1/data7
data.rp7<- data1/data8
data.rp8<- data1/data9
data.rp9<- data1/data10
data.rp10<- data1/data11
  
rownames(data.rp1)<-regressors
rownames(data.rp2)<-regressors
rownames(data.rp3)<-regressors
rownames(data.rp4)<-regressors
rownames(data.rp5)<-regressors
rownames(data.rp6)<-regressors
rownames(data.rp7)<-regressors
rownames(data.rp8)<-regressors
rownames(data.rp9)<-regressors
rownames(data.rp10)<-regressors



# require(grDevices)
# barplot(as.matrix(data.rp1), 
#         main = techs[2], xlab='Target', ylab='RPT',
#         beside = T, col= c("blue", "gray", "darkgreen", "brown"),  
#         ylim = c(0,1.5), 
#         legend.text = T, args.legend = list(x="topright", bty="n"))
# 
# abline(h=1, lty=2, lwd =0.5)


data.rpNames<-list(data.rp1, data.rp2, data.rp3, data.rp4, data.rp5, 
                   data.rp6, data.rp7, data.rp8, data.rp9, data.rp10)




pdf(paste0(data.name, "PorMetodo.pdf"), width = 14, height = 13)
#par(mar=c(1,1,1,1))
par(mfrow=c(5, 2))


for (i in 1:(n.techs-1)){
  barplot(as.matrix(data.rpNames[[i]]), 
          main = techs[i+1], xlab='Target', ylab='RPT',
          beside = T, col= c("blue", "gray", "darkgreen"),  
          ylim = c(0,1.7), 
          legend.text = T, args.legend = list(x="topright", bty="n"), xpd=FALSE)
  
  abline(h=1, lty=2, lwd =0.5)
}

#legend(x=1.5, y=0.4, legend = rownames(data5[-c(1,2),]), bty = "o", pch=20 , col=colors_border , text.col = "grey", cex=1.1, pt.cex=3)

dev.off()

