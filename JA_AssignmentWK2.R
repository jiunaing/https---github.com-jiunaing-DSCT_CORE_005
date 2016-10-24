## 35478-0001-Data
getwd()
setwd("/Users/yeapjiunaing/Documents/DSCT/WK2/Assignment")
getwd()

gcr_raw <- read.csv("GCI_dataset.csv", sep=";")
gcr <- gcr_raw[ gcr_raw$Series.code== 1.03 | gcr_raw$Series.code== 1.05 |gcr_raw$Series.code== 1.16 | 
                gcr_raw$Series.code== 3.03 | gcr_raw$Series.code== 8.06 |gcr_raw$Series.code== 4.05 | 
                gcr_raw$Series.code== 5.03 | gcr_raw$Series.code== 4.10 |gcr_raw$Series.code== 5.01 | 
                gcr_raw$Series.code== 5.02 | gcr_raw$Series.code== 0.03 |gcr_raw$Series.code== 0.01 |
                gcr_raw$Series.code== 10.01| gcr_raw$Series.code== 10.02,] 

  gcrByValue <- gcr[gcr$Attribute=="Value",]
  
  gcrByValue <- gcrByValue[ order(gcrByValue$Edition) , ]
  
  gcrByRank <- gcr[gcr$Attribute=="Rank",]

  gcrByRank <- gcrByRank[ order(gcrByRank$Edition) , ]
  

# Question 1 #######
# KHM IDN MYS PHL SGP THA VNM
# Domestic = 10.01, Foregin market size = 10.02
dosmesticMkt <-gcrByValue[gcrByValue$Series.code=="10.01", -c(2,3,4) ]

dosmesticMkt_tbl <-dosmesticMkt[,c(-1)]
dosmesticMkt_tbl <- data.matrix(dosmesticMkt_tbl)
rownames(dosmesticMkt_tbl) <-dosmesticMkt[,1]
dosmesticMkt_tbl <- as.table(dosmesticMkt_tbl)
barplot(dosmesticMkt_tbl,beside= TRUE, main="Dosmectic Market",xlab="Countries (2006-2015)", ylab = "market size index")
##barplot(aTable, main="asdf",xlab="Happiness Rating",ylab="Fr")

foreginMkt <-gcrByValue[gcrByValue$Series.code=="10.02", -c(2,3,4) ]

foreginMkt_tbl <-foreginMkt[,c(-1)]
foreginMkt_tbl <- data.matrix(foreginMkt_tbl)
rownames(foreginMkt_tbl) <-foreginMkt[,1]
foreginMkt_tbl <- as.table(foreginMkt_tbl)
barplot(foreginMkt_tbl,beside= TRUE, main="Foreign Market ", xlab="Countries (2006-2015)", ylab = "market size index")

# Question 2 #######
#4.10 Primary education enrollment, net %*
#5.01 Secondary education enrollment, gross %*
#5.02 Tertiary education enrollment, gross %*
#5.03 Quality of the education system, 1-7 (best)
#0.01 GDP (US$ billions)
#0.03 GDP per capita (US$)

qualityEducation <-gcrByValue[gcrByValue$Series.code=="5.03", -c(2,3,4) ]
qualityEducationRank<-gcrByRank[gcrByRank$Series.code=="5.03", -c(2,3,4) ]
Gdp<-gcrByValue[gcrByValue$Series.code=="0.01", -c(2,3,4) ]
GdpRank<-gcrByRank[gcrByRank$Series.code=="0.01", -c(2,3,4) ]
## Data Cleansing
Gdp[c(1,2),-c(1)]<-Gdp[c(1,2),-c(1)]/1000
gdp_tbl <- data.matrix(Gdp[,c(-1)])
rownames(gdp_tbl) <-Gdp[,1]
barplot(as.table(gdp_tbl),beside= TRUE, main="GDP",xlab="Countries (2008/2009-2013/2014)", ylab="GDP (US$ billions)")

qualityEducation_tbl <-data.matrix(qualityEducation[,c(-1)])
rownames(qualityEducation_tbl) <- qualityEducation[,c(1)]
barplot(as.table(qualityEducation_tbl),beside= TRUE, main="Education Quality",xlab="Countries (2006/2007-2014/2015)", ylab="1-7 (best)")


# Question 3 #######
#4.10 Primary education enrollment, net %*
#5.01 Secondary education enrollment, gross %*
#5.02 Tertiary education enrollment, gross %*
#4.05 HIV prevalence, % adult pop.*

educPrimEnroll <-gcrByValue[gcrByValue$Series.code=="4.1", -c(2,3,4) ]
educSecEnroll <-gcrByValue[gcrByValue$Series.code=="5.01", -c(2,3,4) ]
educTertEnroll <-gcrByValue[gcrByValue$Series.code=="5.02", -c(2,3,4) ]
HivPrevalance <-gcrByValue[gcrByValue$Series.code=="4.05", -c(2,3,4) ]


educationT_tbl <-data.matrix(educTertEnroll[,c(-1)])
rownames(educationT_tbl) <- educTertEnroll[,c(1)]
barplot(as.table(educationT_tbl),beside= TRUE, main="Tertiary education enrollment", xlab="Countries (2006/2007-2014/2015)", 
        ylab="enrollment gross %")

educationS_tbl <-data.matrix(educSecEnroll[,c(-1)])
rownames(educationS_tbl) <- educSecEnroll[,c(1)]
barplot(as.table(educationS_tbl),beside= TRUE, main="Secondary education enrollment", xlab="Countries (2006/2007-2014/2015)", 
        ylab="enrollment gross %")


HivPrevalance_tbl <-data.matrix(HivPrevalance[,c(-1)])
rownames(HivPrevalance_tbl) <- HivPrevalance[,c(1)]
barplot(as.table(HivPrevalance_tbl),beside= TRUE, main="HIV prevalence",xlab="Countries (2006/2007-2014/2015)", ylab="% adult pop")

# Question 4#############
# 1.03 Diversion of public funds, 1-7 (best)
# 1.16 Reliability of police services, 1-7 (best)
# 1.05 Irregular payments and bribes, 1-7 (best)

diversionFund  <-gcrByValue[gcrByValue$Series.code=="1.03", -c(2,3,4) ]
diversionFund <- diversionFund[-c(1,2,3,4), ]
reliablePolice <-gcrByValue[gcrByValue$Series.code=="1.16", -c(2,3,4) ]
reliablePolice <-reliablePolice[-c(1,2,3,4), ]
irregularPaym  <-gcrByValue[gcrByValue$Series.code=="1.05", -c(2,3,4) ]


library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
# 
x  <- as.numeric(substr(diversionFund$Edition,6,9))

KHMdf <- data.frame(x, diversionFund$KHM, reliablePolice$KHM, irregularPaym$KHM)
KHMdf2 <- melt(data = KHMdf, id.vars = "x")
KHMplot <-ggplot(data = KHMdf2, aes(x = x, y = value, colour = variable)) + geom_line() +
  xlab("Year") +ylab(" 1-7 (best)") +ggtitle("KHM")

IDNdf <- data.frame(x, diversionFund$IDN, reliablePolice$IDN, irregularPaym$IDN)
IDNdf2 <- melt(data = IDNdf, id.vars = "x")
IDNplot <-ggplot(data = IDNdf2, aes(x = x, y = value, colour = variable)) + geom_line() +
  xlab("Year") +ylab(" 1-7 (best)") +ggtitle("IDN")

MYSdf <- data.frame(x, diversionFund$MYS, reliablePolice$MYS, irregularPaym$MYS)
MYSdf2 <- melt(data = MYSdf, id.vars = "x")
MYSplot <-ggplot(data = MYSdf2, aes(x = x, y = value, colour = variable)) + geom_line() +
  xlab("Year") +ylab(" 1-7 (best)") +ggtitle("MYS")

PHLdf <- data.frame(x, diversionFund$PHL, reliablePolice$PHL, irregularPaym$PHL)
PHLdf2 <- melt(data = PHLdf, id.vars = "x")
PHLplot <-ggplot(data = PHLdf2, aes(x = x, y = value, colour = variable)) + geom_line() +
  xlab("Year") +ylab(" 1-7 (best)") +ggtitle("PHL")

SGPdf <- data.frame(x, diversionFund$SGP, reliablePolice$SGP, irregularPaym$SGP)
SGPdf2 <- melt(data = SGPdf, id.vars = "x")
SGPplot <-ggplot(data = SGPdf2, aes(x = x, y = value, colour = variable)) + geom_line() +
  xlab("Year") +ylab(" 1-7 (best)") +ggtitle("SGP")

THAdf <- data.frame(x, diversionFund$THA, reliablePolice$THA, irregularPaym$THA)
THAdf2 <- melt(data = THAdf, id.vars = "x")
THAplot <-ggplot(data = THAdf2, aes(x = x, y = value, colour = variable)) + geom_line() +
  xlab("Year") +ylab(" 1-7 (best)") +ggtitle("THA")

VNMdf <- data.frame(x, diversionFund$VNM, reliablePolice$VNM, irregularPaym$VNM)
VNMdf2 <- melt(data = VNMdf, id.vars = "x")
VNMplot <-ggplot(data = VNMdf2, aes(x = x, y = value, colour = variable)) + geom_line() +
  xlab("Year") +ylab(" 1-7 (best)") +ggtitle("VNM")

grid.arrange(KHMplot,IDNplot,MYSplot,PHLplot,SGPplot,THAplot,VNMplot)

# Question 5 #####
# 3.03 Inflation, annual % change*
# 8.06 Soundness of banks, 1-7 (best)
# 0.03 GDP per capita (US$)
inflation  <-gcrByValue[gcrByValue$Series.code=="3.03", -c(2,3,4) ]
inflation <- inflation[-c(1,2,9), ]
soundBank <-gcrByValue[gcrByValue$Series.code=="8.06", -c(2,3,4) ]
soundBank <- soundBank[-c(1,2,9), ]
gdpPerCap  <-gcrByValue[gcrByValue$Series.code=="0.03", -c(2,3,4) ]

xx  <- as.numeric(substr(inflation$Edition,6,9))

## Need to change to different countries data set to plot the graph at below 4 lines
yy1 <-gdpPerCap$VNM
yy2 <-soundBank$VNM
yy3 <-inflation$VNM
ccc <-"VNM"


par(mar=c(5, 12, 4, 4) + 0.1)

plot(xx, yy1, axes=F, ylim=c(0,max(yy1)), xlab="", ylab="",type="l",col="black", main="",xlim=c(2009,2014))
points(xx,yy1,pch=20,col="black")
axis(2, ylim=c(0,max(yy1)+500),col="black",lwd=2)
mtext(2,text="GDP Per Cap",line=2)

par(new=T)
plot(xx, yy2, axes=F, ylim=c(0,max(yy2)), xlab="", ylab="", 
     type="l",lty=2, main="",xlim=c(2009,2014),lwd=2)
axis(2, ylim=c(0,7),lwd=2,line=3.5)
points(xx, yy2,pch=20)
mtext(2,text="Bank Soundness",line=5.5)

par(new=T)
plot(xx, yy3, axes=F, ylim=c(0,max(yy3)), xlab="", ylab="", 
     type="l",lty=3, main="",xlim=c(2009,2014),lwd=2)
axis(2, ylim=c(0,max(yy3)),lwd=2,line=7)

points(xx, yy3,pch=20)
mtext(2,text="Inflation %",line=9)

##axis(1,pretty(range(xx),10))
axis(1,c(2009 ,2010, 2011 ,2012, 2013, 2014,2015,2016))

#mtext(ccc+ " GDP per Cap, Bank Soundness & Inflation from Year 2009-2014",side=1,col="black",line=2)
mtext(ccc,side=1,col="black",line=2)

#And then plot the legend.
##legend(x=2012,y=max(yy3)/2,legend=c("GDP","Bank","Inflation"),lty=c(1,2,3))

