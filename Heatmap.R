#Author Shabhonam.caim@quadram.ac.uk
#########################################################


### Loading required packages and setting working directory
#########################################################
setwd("~/Desktop/Testdata/Mel/ThreeBaby/Lipid-heatmap/")
library(gplots)
library(RColorBrewer)

#setwd("")    #set working directory

#########################################################
### reading in data and transform it to matrix format
#########################################################

data <- read.csv("48H_BabyX.csv")
head(data)
rnames <- data[,1]                            # assign labels in column 1 to "rnames"
mat_data <- data.matrix(data[,2:ncol(data)])
log_mat_data <- log10(mat_data)# transform column 2-5 into a matrix
head(log_mat_data)
min(log_mat_data)
max(log_mat_data)
mean(log_mat_data)
rownames(log_mat_data) <- rnames # assign row names
t_log_mat_data <- t(log_mat_data)
head(t_log_mat_data)

#########################################################
### customizing and plotting heatmap
#########################################################

# creates a own color palette from red to green
my_palette <- colorRampPalette(c("red","yellow", "green"))(n = 299)
# (optional) defines the color breaks manually for a "skewed"color transition
col_breaks = c(seq(0,7,length=100),  # for blue
               seq(7.1,8,length=100),           # for yellow
               seq(8.1,11,length=100))             # for red

# creates a 5 x 5 inch image - if you want to save the picture directly into a new file which you cannot visualise in the R studio
#png("HeatmapBabyX.png",
 #   width = 5*300,        # 5 x 300 pixels
  #  height = 5*300,
   # res = 300,            # 300 pixels per inch
  #  pointsize = 8)        # smaller font size
pdf("48H_BabyX.pdf", 9,7)



heatmap.2(t_log_mat_data,
          cexRow=0.5,
          cexCol =0.5,
          srtCol  = 90,
          breaks=col_breaks,      # color breaks
          main = "Metabolite data Baby X", # heat map title
          notecol = "black",      # change font color of cell labels to black#
          density.info = "none",  # turns off density plot inside color legend
          trace = "none",         # turns off trace lines inside the heat map
          margins = c(20,5),     # widens margins around plot
          col = my_palette,      # use on color palette defined earlier
          dendrogram="none",
          colsep=0.00000000001:ncol(t_log_mat_data),
          rowsep=0.00000000001:nrow(t_log_mat_data), # apply default clustering method
          Rowv=FALSE,
          Colv=FALSE
)


dev.off()



######trying to merge the plots


#Read the tables for three babies in all conditions
#babyX
X_A <- read.csv("XA-lipids.csv",row.names=1)
X_B <- read.csv("XB-lipids.csv",row.names=1)
X_C <- read.csv("XC-lipids.csv",row.names=1)
X_D <- read.csv("XD-lipids.csv",row.names=1)

#babyY
Y_A <- read.csv("YA-lipids.csv",row.names=1)
Y_B <- read.csv("YB-lipids.csv",row.names=1)
Y_C <- read.csv("YC-lipids.csv",row.names=1)
Y_D <- read.csv("YD-lipids.csv",row.names=1)

#baby Z
Z_A <- read.csv("ZA-lipids.csv",row.names=1)
Z_B <- read.csv("ZB-lipids.csv",row.names=1)
Z_C <- read.csv("ZC-lipids.csv",row.names=1)
Z_D <- read.csv("ZD-lipids.csv",row.names=1)


#Sum and combine the tables for each baby
#babyX
XA<-colSums(X_A)
XB<-colSums(X_B)
XC<-colSums(X_C)
XD<-colSums(X_D)

dataX<-rbind(XA, XB, XC, XD)

#babyY

YA<-colSums(Y_A)
YB<-colSums(Y_B)
YC<-colSums(Y_C)
YD<-colSums(Y_D)

dataY<-rbind(YA, YB, YC, YD)


#babyZ
ZA<-colSums(Z_A)
ZB<-colSums(Z_B)
ZC<-colSums(Z_C)
ZD<-colSums(Z_D)

dataZ<-rbind(ZA, ZB, ZC, ZD)

dataAll <-rbind(XA, XB, XC, XD,YA, YB, YC, YD,ZA, ZB, ZC, ZD)
##################




log_data_bindX <- log10(dataX)
log_data_bindY <- log10(dataY)
log_data_bindZ <- log10(dataZ)
log_data_bindAll <-log10(dataAll)
#transposing them to invert the axis
t_log_data_bindX <- t(log_data_bindX)
t_log_data_bindY <- t(log_data_bindY)
t_log_data_bindZ <- t(log_data_bindZ)
t_log_data_bindAll <- t(log_data_bindAll)
head(log_data_bindZ)
min(log_data_bindZ)
max(log_data_bindZ)
mean(log_data_bindZ)

my_palette <- colorRampPalette(c("green","yellow", "red"))(n = 299)
# (optional) defines the color breaks manually for a "skewed"color transition
col_breaks = c(seq(-5,-1,length=100),  # for blue
               seq(1,1.5,length=100),           # for yellow
               seq(1.6,5,length=100))             # for red

# creates a 5 x 5 inch image - if you want to save the picture directly into a new file which you cannot visualise in the R studio
#png("HeatmapBabyX.png",
#   width = 5*300,        # 5 x 300 pixels
#  height = 5*300,
# res = 300,            # 300 pixels per inch
#  pointsize = 8)        # smaller font size
pdf("BabyZ-lipids-redone.pdf", 10,14)



heatmap.2(t_log_data_bindZ,
          cexRow=0.5,
          cexCol =0.5,
          srtCol  = 90,
          breaks=col_breaks,      # color breaks
          main = "lipidomics data", # heat map title
          notecol = "black",      # change font color of cell labels to black#
          density.info = "none",  # turns off density plot inside color legend
          trace = "none",         # turns off trace lines inside the heat map
          margins = c(20,7),     # widens margins around plot
          col = my_palette,      # use on color palette defined earlier
          dendrogram="none",
          colsep=0.00000000001:ncol(t_log_data_bindZ),
          rowsep=0.00000000001:nrow(t_log_data_bindZ), # apply default clustering method
          Rowv=FALSE,
          Colv=FALSE
)


dev.off()

















