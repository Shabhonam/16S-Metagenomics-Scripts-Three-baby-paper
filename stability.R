setwd("~/Dropbox/3_Baby_paper_shared with Shab/Revised_plots_10072018/Stability/")
#install.packages("codyn")
library(gplots)
library(ggplot)
library(codyn)
library(ggplot2)

#input creation from abundance table 
 for(baby in c("XA","XB","XC","XD","YA","YB","YC","YD","ZA","ZB","ZC","ZD")) {
   babyname <-read.csv(file=paste(baby, ".csv", sep=""))
   melteddata <- melt(babyname)
   write.csv(melteddata,file=paste(baby, ".Forstability.csv", sep=""))
   data <- read.csv(file=paste(baby, "-input.csv", sep=""))
   head(data)
   stab <- community_stability(df=data, time.var="Hours", abundance.var = "Abundance",replicate.var = "Genus")
  print(stab)


  }


data <- read.csv("YA-input.csv")
data2 <-read.csv("YB-input.csv")
#melteddata <- melt(data) #to convert the table and save it 
#head(data)
#write.csv(melteddata,"Forstability_ZC-24hrs.csv") #saving converted table
#data("knz_001d")

#head(knz_001d)
#if you want to get an overall stability then use a subplot column in excel 
stab <- community_stability(df=data, time.var="Hours", 
                            abundance.var = "Abundance",replicate.var = "Genus")
stab  <- stab[!is.infinite(stab$stability),]
stab2 <- community_stability(df=data2, time.var="Hours", 
                            abundance.var = "Abundance",replicate.var = "Genus")
stab2  <- stab2[!is.infinite(stab2$stability),]
print(stab)
# pdf("ZD-stability.pdf",6,10)
# 
# ggplot(stab, aes(x=Genus, y=stability)) + geom_point(shape=22, fill="darkred", color="darkred", size=3) +theme(axis.text.x = element_text(angle = 90, hjust = 1))
# dev.off()
#variance <-variance_ratio(df=data, time.var="Hours", 
#                         abundance.var = "Abundance", species.var ="Genus",bootnumber=1,
#                        average.replicates = F)

pdf("BabyY-AB-Stability_19102018.pdf",15,10)

ggplot(stab,aes(x=Genus,y=stability, group = 1) )+geom_point(aes(color="Breast milk",pch=17), fill="red", size=6)+
  geom_point(data=stab2,aes(color="Formula",pch=19), size=6, fill="blue" )+theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_shape_identity() +ylim(0,40)

dev.off()
