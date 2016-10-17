## 35478-0001-Data
getwd()
setwd("/Users/yeapjiunaing/Documents")
getwd()

csv_data <- read.csv("35478-0001-Data.csv")
library(dplyr)

### Question 1 #############

mydata<-select(csv_data,ID,MARITAL,AGE,GENDER1,HOMPOP,HAPPY,HEALTH,SATJOB,REALINC)

mydata_NA <- mydata
mydata_NA$MARITAL[mydata_NA$MARITAL==9]
mydata_NA$AGE[mydata_NA$AGE==-1|mydata_NA$AGE==98|mydata_NA$AGE==99]
mydata_NA$GENDER1[mydata_NA$GENDER1==0|mydata_NA$GENDER1==8|mydata_NA$GENDER1==9] 
mydata_NA$HOMPOP[mydata_NA$HOMPOP==98 |mydata_NA$HOMPOP==99]
mydata_NA$HAPPY[mydata_NA$HAPPY==0|mydata_NA$HAPPY==8|mydata_NA$HAPPY==9] 
mydata_NA$HEALTH[mydata_NA$HEALTH==0|mydata_NA$HEALTH==8|mydata_NA$HEALTH==9] 
mydata_NA$SATJOB[mydata_NA$SATJOB==0|mydata_NA$SATJOB==8|mydata_NA$SATJOB==9] 
mydata_NA$REALINC[mydata_NA$REALINC==0|mydata_NA$REALINC==999998|mydata_NA$REALINC==999999] 


### Question 2 #############
mydata_NA <- mydata
mydata_NA$MARITAL[mydata_NA$MARITAL==9] <-NA
mydata_NA$AGE[mydata_NA$AGE==-1|mydata_NA$AGE==98|mydata_NA$AGE==99] <-NA
mydata_NA$GENDER1[mydata_NA$GENDER1==0|mydata_NA$GENDER1==8|mydata_NA$GENDER1==9] <-NA
mydata_NA$HOMPOP[mydata_NA$HOMPOP==98 |mydata_NA$HOMPOP==99]<-NA
mydata_NA$HAPPY[mydata_NA$HAPPY==0|mydata_NA$HAPPY==8|mydata_NA$HAPPY==9] <-NA
mydata_NA$HEALTH[mydata_NA$HEALTH==0|mydata_NA$HEALTH==8|mydata_NA$HEALTH==9] <-NA
mydata_NA$SATJOB[mydata_NA$SATJOB==0|mydata_NA$SATJOB==8|mydata_NA$SATJOB==9] <-NA
mydata_NA$REALINC[mydata_NA$REALINC==0|mydata_NA$REALINC==999998|mydata_NA$REALINC==999999] <-NA

### Question 3 #############
mydata_NA$MARITAL.f <- factor(mydata_NA$MARITAL, labels = c("Married","Widowed","Divorced","Separated","Never Married"))
mydata_NA$GENDER1.f <- factor(mydata_NA$GENDER1, labels = c("Male","Female"))
mydata_NA$HAPPY.f <- factor(mydata_NA$HAPPY, labels = c("Very happy","Pretty happy","Not too happy"))
mydata_NA$HEALTH.f <- factor(mydata_NA$HEALTH,labels = c("Excellent","Good","Fair","Poor"))
mydata_NA$SATJOB.f <- factor(mydata_NA$SATJOB,labels = c("Very Satisfied","Moderately Satisfied","A little dissatisfied","Very dissatisfied"))

### Question 4 #############
percapinc <-  mydata_NA$REALINC/mydata_NA$HOMPOP

mydata_percap <- cbind(mydata_NA,percapinc)

mydata_percap$HAPPY
mydata_percap$REALINC
mydata_percap$percapinc

### Question 5 #############
cor.test(mydata_percap$HAPPY,mydata_percap$REALINC)
cor.test(mydata_percap$HAPPY,mydata_percap$percapinc)

# Happiness value is expressed as 1,2,3 wih lowest numer represent the happiest (the most happy)
# correlation of happiness with percapinc is -0.16
# correlation of happiness with realinc is -0.10
# that indicates using percapinc yields better correlation with happiness as opposed to using realinc 
#MARITAL,AGE,GENDER1,HOMPOP,HAPPY,HEALTH,SATJOB,REALINC,percapinc
#ggplot(mydata_percap, aes(x = HAPPY)) 	+ geom_histogram(colour = "darkgreen", fill = "blue", binwidth = 1) 

summary_modal<-summary(mydata_percap)
summary_modal

### Question 6 #############

barplot(table(mydata_percap$HAPPY.f), main="Happiness Distribution by Male & Female",xlab="Happiness Rating",ylab="Frequency")
barplot(table(mydata_percap$HAPPY.f[mydata_percap$GENDER1.f=="Male"]), main="Happiness Distribution by Male",xlab="Happiness Rating",ylab="Frequency")
barplot(table(mydata_percap$HAPPY.f[mydata_percap$GENDER1.f=="Female"]), main="Happiness Distribution by Female",xlab="Happiness Rating",ylab="Frequency")


MaleHappyTable <- table(mydata_percap$HAPPY.f[mydata_percap$GENDER1.f=="Male"])
CntMalePop   <- sum( MaleHappyTable)

MaleHappyTable[1]<-MaleHappyTable[1]*100/CntMalePop
MaleHappyTable[2]<-MaleHappyTable[2]*100/CntMalePop
MaleHappyTable[3]<-MaleHappyTable[3]*100/CntMalePop

FemaleHappyTable <- table(mydata_percap$HAPPY.f[mydata_percap$GENDER1.f=="Female"])
CntFemalePop   <- sum(FemaleHappyTable)

FemaleHappyTable[1]<-FemaleHappyTable[1]*100/CntFemalePop
FemaleHappyTable[2]<-FemaleHappyTable[2]*100/CntFemalePop
FemaleHappyTable[3]<-FemaleHappyTable[3]*100/CntFemalePop


aTableCnt <-cbind( MaleHappyTable, FemaleHappyTable)

barplot(aTableCnt, main="Happiness Distribution by Male & Female",xlab="Gender",ylab="Frequency",legend= rownames(count),
        beside= TRUE)


aTableCnt <-cbind( table(mydata_percap$HAPPY.f[mydata_percap$GENDER1.f=="Male"]), 
                   table(mydata_percap$HAPPY.f[mydata_percap$GENDER1.f=="Female"]))
barplot(aTableCnt, main="Happiness Distribution by Male & Female",xlab="Gender",ylab="Frequency",legend= rownames(count),
        beside= TRUE)
## from the bar plots obviously female didn't fair better than male in the categories
## of "very happy" and "not too happy"

##Question 6 Chisq.test()###############

chisq.test(aTableCnt)

# the test gives p-value=0.4765, greater than the 0.05 significance level
# chisq test independendence of two variables, which in this independendence of gender and happiness (null hypothesis)
# p-value=0.4765 means 47% chance that null hypothesis is correct
# that is greater than 5% significant level which lead to conclusion 
# we don't reject null hypothesis that happiness is independent of gender


##Question 7###############

percapinc <-  mydata_percap$AGE

mydata_percap <- cbind(mydata_NA,percapinc)
class(mydata_percap)
mydata_percap[,"AgeCategory"] <- NA

mydata_percap$AgeCategory[mydata_percap$AGE < 35 ] <- "young"
mydata_percap$AgeCategory[mydata_percap$AGE >= 35 & mydata_percap$AGE <60 ] <- "middle age"
mydata_percap$AgeCategory[mydata_percap$AGE >=60 ] <- "old"

youngTbl  = table(mydata_percap$HAPPY.f[mydata_percap$AgeCategory=="young"])
middleTbl = table(mydata_percap$HAPPY.f[mydata_percap$AgeCategory=="middle age"])
oldTbl   =  table(mydata_percap$HAPPY.f[mydata_percap$AgeCategory=="old"])

aContingencyTbl = cbind(youngTbl,middleTbl,oldTbl)

aPropTblByCol = prop.table(aContingencyTbl,2)
#aPropTblByRow = prop.table(aContingencyTbl,1)

chisq.test(aContingencyTbl)
#chisq.test gives p-value = 0.00234, below 0.05
#we reject null hypothesis that ageCategory is independent of happiness

aPropTblByCol = prop.table(aContingencyTbl,2)
barplot(aPropTblByCol, main="Happiness Distribution by Age Group",xlab="Age Group",ylab="Ratio",legend= rownames(count),
        beside=  FALSE)
#aProbTblByCol and its barpot shows young group's "not too happy" 
#ratio was the lowest compares to middle and old age group


##Question 8###############

VeryHappy_realinc <- median(mydata_percap$REALINC[mydata_percap$HAPPY.f =="Very happy"],na.rm = TRUE)
VeryHappy_realinc
PrettyHappy_realinc<- median( mydata_percap$REALINC[mydata_percap$HAPPY.f =="Pretty happy"],na.rm = TRUE)
PrettyHappy_realinc
NotHappy_realinc<- median(mydata_percap$REALINC[mydata_percap$HAPPY.f =="Not too happy"],na.rm = TRUE)
NotHappy_realinc

# Median income of "very happy" category is 26950, 
#  higher that "Pretty happy" (22050), 
# of which also higher than "Not Happy" (13475)

##Question 9##########

# Health = {"Excellent","Good","Fair","Poor"}
ExcellentHealth_realinc <-(mydata_percap$REALINC[mydata_percap$HEALTH.f =="Excellent"])
ExcellentHealth_realinc
GoodHealth_realinc <-(mydata_percap$REALINC[mydata_percap$HEALTH.f =="Good"])
GoodHealth_realinc
FairHealth_realinc <-(mydata_percap$REALINC[mydata_percap$HEALTH.f =="Fair"])
FairHealth_realinc
PoorHealth_realinc <-(mydata_percap$REALINC[mydata_percap$HEALTH.f =="Poor"])
PoorHealth_realinc

boxplot(ExcellentHealth_realinc,GoodHealth_realinc,FairHealth_realinc,PoorHealth_realinc)
# the boxplot shows ppl with better health condition richer

##Question 10##########
# Marital  = {"Married","Widowed","Divorced","Separated","Never Married"}
MarriedHappiness  = table(mydata_percap$HAPPY.f[mydata_percap$MARITAL.f =="Married"])
WidowedHappiness  = table(mydata_percap$HAPPY.f[mydata_percap$MARITAL.f =="Widowed"])
DivorcedHappiness = table(mydata_percap$HAPPY.f[mydata_percap$MARITAL.f =="Divorced"])
SeparatedHappiness = table(mydata_percap$HAPPY.f[mydata_percap$MARITAL.f =="Separated"])
NeverMarriedHappiness = table(mydata_percap$HAPPY.f[mydata_percap$MARITAL.f =="Never Married"])

aContingencyMarryTbl = cbind(MarriedHappiness,WidowedHappiness,DivorcedHappiness,SeparatedHappiness,NeverMarriedHappiness)

chisq.test(aContingencyMarryTbl)

# the test gives p-value=2.2e-16, much lower than the 0.05 significance level
# chisq test independendence of two variables, which in this case independendence of Marital and happiness (null hypothesis)
# p-value=2.2e-16 means almost zero chance that null hypothesis is correct
# we reject the null hypothesis that happiness is independent of marital status

# marital status has effect on happiness

##Question 11##########
# Are there any household income differences btw races??

#The data set used doesn't race info for analysis