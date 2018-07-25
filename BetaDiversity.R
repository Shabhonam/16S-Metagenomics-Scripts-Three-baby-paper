# Plot for beta diversity 
setwd("~/Desktop/Testdata/Mel/ThreeBaby/BoxPLOTS/")

#Read the tables for three babies in all conditions
#babyX
X_A <- read.csv("XA-binary.csv",row.names=1)
X_B <- read.csv("XB-binary.csv",row.names=1)
X_C <- read.csv("XC-binary.csv",row.names=1)
X_D <- read.csv("XD-binary.csv",row.names=1)

#babyY
Y_A <- read.csv("YA-binary.csv",row.names=1)
Y_B <- read.csv("YB-binary.csv",row.names=1)
Y_C <- read.csv("YC-binary.csv",row.names=1)
Y_D <- read.csv("YD-binary.csv",row.names=1)

#baby Z
Z_A <- read.csv("ZA-binary.csv",row.names=1)
Z_B <- read.csv("ZB-binary.csv",row.names=1)
Z_C <- read.csv("ZC-binary.csv",row.names=1)
Z_D <- read.csv("ZD-binary.csv",row.names=1)


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
head(dataX)

#write these into a tables for binary conversion (use the awk script) and  fututre use
write.csv(dataX, "DataX.csv")
write.csv(dataY, "DataY.csv")
write.csv(dataZ, "DataZ.csv")

#use above tables in beta diversity 

X <-read.csv("DataXSum-binary.csv", row.names=1)
Y <-read.csv("DataYSum-binary.csv", row.names=1)
Z <-read.csv("DataZSum-binary.csv", row.names=1)


#now calculate the beta diversity using betapart
library(betapart)
betaA <- betapart.core(X)
betaB <- betapart.core(Y)
betaC <- betapart.core(Z)
head(betaA)
betaAsamp <- beta.sample(X, sites=10, samples=100)
betaBsamp <- beta.sample(Y, sites=10, samples=100)
betaCsamp <- beta.sample(Z, sites=10, samples=100)
betaAdist <- betaAsamp$sampled.values
betaBdist <- betaBsamp$sampled.values
betaCdist <- betaCsamp$sampled.values
pdf("beta-SIM.pdf")
plot(density(betaAdist$beta.SIM), xlim=c(0,0.8), ylim=c(0, 19), xlab='Beta diversity', main='Beta diversity turnover', lwd=3, col='red')

#lines(density(betaAdist$beta.SNE), lty=1, lwd=2, col='red')
lines(density(betaAdist$beta.SIM), lty=2, lwd=2, col='red')
#lines(density(betaAdist$beta.SOR),  lwd=3, col='red')



#lines(density(betaBdist$beta.SNE), lty=1, lwd=2,col='green')
lines(density(betaBdist$beta.SIM), lty=1, lwd=3, col='green')


#lines(density(betaBdist$beta.SOR), col='green', lwd=3)



#lines(density(betaCdist$beta.SOR), col='blue', lwd=3)
lines(density(betaCdist$beta.SIM), lty=1, lwd=3, col='blue')
#lines(density(betaCdist$beta.SNE), col='blue', lty=1, lwd=2 )
#Add a legend
legend("bottomright", 
       legend = c("babyX", "babyY", "babyZ"), 
       col = c("red","green", "blue" ), 
       pch = c(19,19,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1, 0.1))

dev.off()



