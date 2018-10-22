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

data<-read.csv("r2pca_5.csv", header=TRUE,sep=",", dec = ",")[1:(n.regressors*n.techs),]
#this data should be displaced as follows: the columns corresponds to the boarders of the carts (targets); 
#the rows represent the combination of regressors (that will be represented by polygons) and tecniques (in the order: ST+rf, ST+svm, ST+cart, MTRS+rf, MTRS+svm....)


data1<-data[1:n.regressors,]
data2<-data[(n.regressors+1):(n.regressors*2),]
data3<-data[(n.regressors*2+1):(n.regressors*3),]
data4<-data[(n.regressors*3+1):(n.regressors*4),]
data5<-data[(n.regressors*4+1):(n.regressors*5),]
data6<-data[(n.regressors*5+1):(n.regressors*6),]
data7<-data[(n.regressors*5+1):(n.regressors*6),]
data8<-data[(n.regressors*5+1):(n.regressors*6),]
data9<-data[(n.regressors*5+1):(n.regressors*6),]
data10<-data[(n.regressors*5+1):(n.regressors*6),]
data11<-data[(n.regressors*5+1):(n.regressors*6),]


rownames(data1)<-regressors
rownames(data2)<-regressors
rownames(data3)<-regressors
rownames(data4)<-regressors
rownames(data5)<-regressors
rownames(data6)<-regressors
rownames(data7)<-regressors
rownames(data8)<-regressors
rownames(data9)<-regressors
rownames(data10)<-regressors
rownames(data11)<-regressors

if(relative==TRUE){
  data1=rbind(rep(1, n.targets) , rep(0, n.targets) , data1)
  data2=rbind(rep(1, n.targets) , rep(0, n.targets) , data2)
  data3=rbind(rep(1, n.targets) , rep(0, n.targets) , data3)
  data4=rbind(rep(1, n.targets) , rep(0, n.targets) , data4)
  data5=rbind(rep(1, n.targets) , rep(0, n.targets) , data5)
  data6=rbind(rep(1, n.targets) , rep(0, n.targets) , data6)
  data7=rbind(rep(1, n.targets) , rep(0, n.targets) , data7)
  data8=rbind(rep(1, n.targets) , rep(0, n.targets) , data8)
  data9=rbind(rep(1, n.targets) , rep(0, n.targets) , data9)
  data10=rbind(rep(1, n.targets) , rep(0, n.targets) , data10)
  data11=rbind(rep(1, n.targets) , rep(0, n.targets) , data11)
}else{
  data1=rbind(c(max(data1),max(data1),max(data1),max(data1),max(data1),max(data1),max(data1),max(data1),max(data1),max(data1),max(data1),max(data1) ) , c(min(data1), min(data1), min(data1), min(data1), min(data1), min(data1), min(data1), min(data1), min(data1), min(data1), min(data1), min(data1)) , data1)
  data2=rbind(c(max(data2),max(data2),max(data2),max(data2),max(data2),max(data2),max(data2),max(data2),max(data2),max(data2),max(data2),max(data2) ) , c(min(data2), min(data2), min(data2), min(data2), min(data2), min(data2), min(data2), min(data2), min(data2), min(data2), min(data2), min(data2)) , data2)
  data3=rbind(c(max(data3),max(data3),max(data3),max(data3),max(data3),max(data3),max(data3),max(data3),max(data3),max(data3),max(data3),max(data3) ) , c(min(data3), min(data3), min(data3), min(data3), min(data3), min(data3), min(data3), min(data3), min(data3), min(data3), min(data3), min(data3)) , data3)
  data4=rbind(c(max(data4),max(data4),max(data4),max(data4),max(data4),max(data4),max(data4),max(data4),max(data4),max(data4),max(data4),max(data4) ) , c(min(data4), min(data4), min(data4), min(data4), min(data4), min(data4), min(data4), min(data4), min(data4), min(data4), min(data4), min(data4)) , data4)
  data5=rbind(c(max(data5),max(data5),max(data5),max(data5),max(data5),max(data5),max(data5),max(data5),max(data5),max(data5),max(data5),max(data5) ) , c(min(data5), min(data5), min(data5), min(data5), min(data5), min(data5), min(data5), min(data5), min(data5), min(data5), min(data5), min(data5)) , data5)
#  data6=rbind(c(max(data6),max(data6),max(data6),max(data6),max(data6),max(data6),max(data6),max(data6),max(data6),max(data6),max(data6),max(data6) ) , c(min(data6), min(data6), min(data6), min(data6), min(data6), min(data6), min(data6), min(data6), min(data6), min(data6), min(data6), min(data6)) , data6)
}


pdf("myplot.pdf", width = 14, height = 13)
#par(mar=c(1,1,1,1))
par(mfrow=c(4, 3))



#=================
# Plot3: If you remove the 2 first lines, the function compute the max and min of each variable with the available data:

colors_border=c( rgb(0.1,0.1,0.1,0.9), rgb(0.1,0.5,0.1,0.9) , rgb(0.9,0.1,0.1,0.9), rgb(0.9,0.1,0.1,0.9) )
colors_in=c( rgb(0.1,0.1,0.1,0.9), rgb(0.1,0.5,0.1,0.9) , rgb(0.1,0.1,0.5,0.9), rgb(0.9,0.9,0.9,0.9))
# radarchart( data1[-c(1,2),]  , axistype=1, maxmin=F, 
radarchart( data1  , axistype=4, maxmin=T, seg=10, centerzero = T, pty=32,
            
            pcol=colors_border , #pfcol=colors_in , 
            plwd=1 , plty=5,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.5,
            #custom labels
            vlcex=1, title=techs[1], palcex=1
)
#legend(x=1.7, y=1, legend = rownames(data1[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=0.8, pt.cex=3)


# colors_border=c( rgb(0.1,0.1,0.1,0.9), rgb(0.1,0.5,0.1,0.9) , rgb(0.1,0.1,0.5,0.9), rgb(0.9,0.9,0.9,0.9) )
# colors_in=c( rgb(0.1,0.1,0.1,0.9), rgb(0.1,0.5,0.1,0.9) , rgb(0.1,0.1,0.5,0.9), rgb(0.9,0.9,0.9,0.9) )
radarchart( data2  , axistype=4 , maxmin=T, seg=10, centerzero = T, pty=32,
            #custom polygon
            pcol=colors_border , #pfcol=colors_in , 
            plwd=1 , plty=5, 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
            #custom labels
            vlcex=1, title=techs[2]
)
#legend(x=1.7, y=1, legend = rownames(data2[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=0.8, pt.cex=3)


radarchart( data3  , axistype=4 , maxmin=T, seg=10, centerzero = T, pty=32,
            #custom polygon
            pcol=colors_border , #pfcol=colors_in , 
            plwd=1 , plty=5, 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
            #custom labels
            vlcex=1, title=techs[3]
)
#legend(x=1.7, y=1, legend = rownames(data3[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=0.8, pt.cex=3)

radarchart( data4  , axistype=4 , maxmin=T, seg=10, centerzero = T, pty=32,
            #custom polygon
            pcol=colors_border , #pfcol=colors_in , 
            plwd=1 , plty=5, 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
            #custom labels
            vlcex=1, title=techs[4]
)
#legend(x=1.7, y=-0.5, legend = rownames(data4[-c(1,2),]), bty = "o", pch=20 , col=colors_in , text.col = "grey", cex=1.1, pt.cex=3)

radarchart( data5  , axistype=4 , maxmin=T, seg=10, centerzero = T, pty=32,
            #custom polygon
            pcol=colors_border , #pfcol=colors_in , 
           plwd=1 , plty=5, 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
            #custom labels
            vlcex=1, title=techs[5]
)

radarchart( data6 , axistype=4 , maxmin=T, seg=10, centerzero = T, pty=32,
            #custom polygon
            pcol=colors_border , #pfcol=colors_in , 
            plwd=1 , plty=5,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
            #custom labels
            vlcex=1, title=techs[6]
)

radarchart( data7 , axistype=4 , maxmin=T, seg=10, centerzero = T, pty=32,
            #custom polygon
            pcol=colors_border , #pfcol=colors_in , 
            plwd=1 , plty=5,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
            #custom labels
            vlcex=1, title=techs[7]
)

radarchart( data8 , axistype=4 , maxmin=T, seg=10, centerzero = T, pty=32,
            #custom polygon
            pcol=colors_border , #pfcol=colors_in , 
            plwd=1 , plty=5,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
            #custom labels
            vlcex=1, title=techs[8]
)

radarchart( data9 , axistype=4 , maxmin=T, seg=10, centerzero = T, pty=32,
            #custom polygon
            pcol=colors_border , #pfcol=colors_in , 
            plwd=1 , plty=5,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
            #custom labels
            vlcex=1, title=techs[9]
)

radarchart( data10 , axistype=4 , maxmin=T, seg=10, centerzero = T, pty=32,
            #custom polygon
            pcol=colors_border , #pfcol=colors_in , 
            plwd=1 , plty=5,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
            #custom labels
            vlcex=1, title=techs[10]
)

radarchart( data11 , axistype=4 , maxmin=T, seg=10, centerzero = T, pty=32,
            #custom polygon
            pcol=colors_border , #pfcol=colors_in ,
            plwd=1 , plty=5,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
            #custom labels
            vlcex=1, title=techs[11]
)

legend(x=1.5, y=0.4, legend = rownames(data5[-c(1,2),]), bty = "o", pch=20 , col=colors_border , text.col = "grey", cex=1.1, pt.cex=3)

dev.off()

