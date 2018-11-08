setwd("~/Desktop/Testdata/NewcastleData/New/")
library(Hotelling)
library(vegan)
disturbed <- read.csv("NewCastle-data-abundance_forBarPlot.csv",row.names=1)
#disturebd <- data[is.na(disturbed)] <- 0
#pdf("rarecurve.pdf")
#rarecurve(disturbed)
#dev.off()
#estimateR(disturbed)
pdf("AlphaNewcastle.pdf")
alpha <-fisher.alpha(disturbed, MARGIN = 1, se = FALSE)
plot(alpha, main= "Alpha diversity", col= "red", pch = 19, cex = 1, lty = "solid", lwd=2)
text(alpha, labels=row.names(disturbed), cex= 0.7, pos=3)
dev.off()
pdf("AlphaNewcastle2.pdf")
boxplot(alpha)
dev.off()




########trying boxplots



X_A <- read.csv("NE.CSV",row.names=1)
X_B <- read.csv("control.csv",row.names=1)
X_C <- read.csv("subNE.CSV",row.names=1)
X_D <- read.csv("Z_D_Comparison-ex.csv",row.names=1)

NE <-fisher.alpha(X_A, MARGIN = 1, se = FALSE)
Control <-fisher.alpha(X_B, MARGIN = 1, se = FALSE)
SubNE <-fisher.alpha(X_C, MARGIN = 1, se = FALSE)
D <-fisher.alpha(X_D, MARGIN = 1, se = FALSE)

data<-rbind(NE,Control,SubNE)
newdata <- t(data)
pdf("AlphaX-boxplot.pdf")
boxplot(newdata, main= "Alpha diversity", col= "darkgreen", pch = 15, cex = 1, lty = "solid", lwd=2)
dev.off()
head(newdata)
