setwd("~/Desktop/Testdata/Mel/ThreeBaby/BabyZ_all/")
library(Hotelling)
library(vegan)
disturbed <- read.csv("Z_D_Comparison-ex.csv",row.names=1)
#disturebd <- data[is.na(disturbed)] <- 0
#pdf("rarecurve.pdf")
#rarecurve(disturbed)
#dev.off()
#estimateR(disturbed)
pdf("AlphaZ-D.pdf")
alpha <-fisher.alpha(disturbed, MARGIN = 1, se = FALSE)
plot(alpha, main= "Alpha diversity", col= "red", pch = 19, cex = 1, lty = "solid", lwd=2)
text(alpha, labels=row.names(disturbed), cex= 0.7, pos=3)
dev.off()
pdf("AlphaZ-D-boxplot.pdf")
boxplot(alpha)
dev.off()




########trying boxplots



X_A <- read.csv("Z_A_Comparison-ex.csv",row.names=1)
X_B <- read.csv("Z_B_Comparison-ex.csv",row.names=1)
X_C <- read.csv("Z_C_Comparison-ex.csv",row.names=1)
X_D <- read.csv("Z_D_Comparison-ex.csv",row.names=1)

A <-fisher.alpha(X_A, MARGIN = 1, se = FALSE)
B <-fisher.alpha(X_B, MARGIN = 1, se = FALSE)
C <-fisher.alpha(X_C, MARGIN = 1, se = FALSE)
D <-fisher.alpha(X_D, MARGIN = 1, se = FALSE)

data<-rbind(A,B,C,D)
newdata <- t(data)
pdf("AlphaX-boxplot.pdf")
boxplot(newdata, main= "Alpha diversity Baby Z", col= "orange", pch = 19, cex = 1, lty = "solid", lwd=2)
dev.off()
head(newdata)
