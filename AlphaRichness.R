setwd("~/Desktop/Testdata/Mel/ThreeBaby/BoxPLOTS/")
library(Hotelling)
library(vegan)
library(plyr)

X_A <- read.csv("X_A_Comparison-ex.csv",row.names=1)
X_B <- read.csv("Y_B_Comparison-ex.csv",row.names=1)
X_C <- read.csv("Y_C_Comparison-ex.csv",row.names=1)
X_D <- read.csv("Y_D_Comparison-ex.csv",row.names=1)
head(X_A)
A<-colSums(X_A)
B<-colSums(X_B)
C<-colSums(X_C)
D<-colSums(X_D)
head(D)
data<-rbind(A, B, C, D)
head(data)
#data[is.na(data)] <- 0
head(data)
pdf("rarecurve_all.pdf")
rarecurve(data)
dev.off()
estimateR(data)
pdf("alpha.pdf")
alpha <-fisher.alpha(data, MARGIN = 1, se = FALSE)
plot(alpha, main= "Alpha diversity", col= "red", pch = 19, cex = 1, lty = "solid", lwd = 2)
text(alpha, labels=row.names(data), cex= 0.7, pos=3)
dev.off()
shann <- diversity(data)
shann
pdf("shannon.pdf")

hist(shann,labels = TRUE)


dev.off()
##### sum all the columns

#sumdisturbed<-colSums(disturbed)
#head(sumdisturbed)


#specpool(disturbed)

##########
BCI <-data
head(data)
H <- diversity(BCI)
simp <- diversity(BCI, "simpson")
invsimp <- diversity(BCI, "inv")
r.2 <- rarefy(BCI, 2)
alpha <- fisher.alpha(BCI)
pdf("summary.pdf")
pairs(cbind(H, simp, invsimp, r.2, alpha), pch="+", col="blue")
dev.off()
## Species richness (S) and Pielou's evenness (J):
S <- specnumber(BCI) ## rowSums(BCI > 0) does the same...
J <- H/log(S)

