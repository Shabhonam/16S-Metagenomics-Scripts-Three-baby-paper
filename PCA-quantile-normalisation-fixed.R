setwd("~/Desktop/PCA_lipids/")

library(devtools)
#install_github("ggbiplot", "vqv")
#install_github('sinhrks/ggfortify')
#source("https://bioconductor.org/biocLite.R")
#biocLite("preprocessCore")
library(ggfortify)
library(cluster)
require("gplots")
require("ggplot2")
require("RColorBrewer")

library("preprocessCore")
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

file <- read.csv("BMvsF.csv")

data <- file

head(data)
ncol(data)
rownames(data) <- data[,1]
data_mat <- data.matrix(data[,-1]) 
head(data_mat)
data_norm <- normalize.quantiles(data_mat, copy = FALSE)
head(data_norm)

log.data <-log(data_norm)
pca.data <-prcomp(data_norm)
head(data)

png("BMvsF.png" , 
    height = 5*600, 
    width = 5*600,         # 5 x 300 pixels
    res = 300,             # 300 pixels per inch
    pointsize = 10)
autoplot(pca.data,data=data,colour='ID', label=FALSE, loadings.label = FALSE, loadings.label.size = 3,frame = TRUE, frame.type = 'norm')+scale_color_manual(values=c("Red","Blue","Green","Purple","Brown","Orange","Black"))+
  scale_fill_manual(values=c("Red","Blue","Green","Purple","Brown","Orange","Black"))+theme_bw()+ylab("PC2")+geom_point(aes(shape=ID, color=ID)) +scale_shape_manual(values=c(3, 16, 17, 12, 15, 16))
#loadings.label = FALSE, loadings.label.size = 3,frame = TRUE, frame.type = 'norm' this is for loading and eigenvectors
dev.off()
pdf("BMvsF.pdf", 7,7)
autoplot(pca.data,data=data,colour='ID', label=FALSE, loadings.label = FALSE, loadings.label.size = 3,frame = TRUE, frame.type = 'norm')+scale_color_manual(values=c("Red","Blue","Green","Purple","Brown","Orange","Black"))+
  scale_fill_manual(values=c("Red","Blue","Green","Purple","Brown","Orange","Black"))+theme_bw()+ylab("PC2")+geom_point(aes(shape=ID, color=ID)) +scale_shape_manual(values=c(3, 16, 17, 12, 15, 16))
#loadings.label = FALSE, loadings.label.size = 3,frame = TRUE, frame.type = 'norm' this is for loading and eigenvectors
dev.off()
