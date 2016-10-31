#
#
#
getwd()
setwd("/Users/yeapjiunaing/Documents/DSCT/WK3/assigement")
getwd()

gcr_raw <- read.csv("german_credit_data.csv", sep=";",header=T,na.strings=c(""))


gcr  <-gcr_raw
#Question 1 ####### Data Checking

sapply(gcr_raw, function(x) sum(is.na(x)))
sapply(gcr_raw, function(x) length(unique(x)))
# Found Other.installment.plans and Years.with.present.employer have values no defined in the code book
gcr$purpose.of.loan[gcr$purpose.of.loan>10|gcr$purpose.of.loan<0]
gcr$Other.installment.plans[gcr$Other.installment.plans>3|gcr$Other.installment.plans<1] <-NA
gcr$Years.with.present.employer[gcr$Years.with.present.employer>5|gcr$Years.with.present.employer<1] <-NA


library(Amelia)
missmap(gcr, main ="Missing values vs observed")

nrow(gcr)
gcr <-gcr[complete.cases(gcr),] # remove rows having NA value
nrow(gcr)

#Question 2 #######

# Split categorical value for the personal column
#contrasts(gcr$Foreign.Worker)
#contrasts(gcr$sex)

gcr$sex <- 0  # default as male
gcr$sex[gcr$Personal == 2|gcr$Personal == 5] <- 1 ## 1 as female
gcr$IsSingle <-0 # Is not single
gcr$IsSingle[gcr$Personal == 3|gcr$Personal == 5] <-1 ## is single

gcr <- subset(gcr, select=-c(Personal))
gcr$Credit.offered <- gcr$Credit.offered. 
gcr <- subset(gcr, select=-c(Credit.offered.))
gcr$Foreign.Worker[gcr$Foreign.Worker == 2] <-0
gcr$Credit.offered[gcr$Credit.offered == 2] <- 0 # To factor 0 (original raw value=2) as reject him and 1 (original raw value) as approve




#Question 3######
head(gcr)
library(ggplot2)
library(ggfortify)

##ggplot(gcr, aes(gcr$Savings.account ,gcr$Credit.asked.for..X.100.,  color = gcr$Credit.offered.)) + geom_point()
##ggplot(gcr, aes(gcr$Assets ,gcr$other.debtors.guarantors,  color = gcr$Credit.offered.)) + geom_point()


set.seed(20)
grp <- c(1:21)

gcrCluster <- kmeans(gcr[, grp], 3, nstart = 50)
autoplot(gcrCluster,gcr[, grp])

gcrCluster <- kmeans(gcr[, grp], 4, nstart = 50)
autoplot(gcrCluster,gcr[, grp])

gcrCluster <- kmeans(gcr[, grp], 5, nstart = 50)
autoplot(gcrCluster,gcr[, grp])

gcrCluster <- kmeans(gcr[, grp], 6, nstart = 50)
autoplot(gcrCluster,gcr[, grp])

gcrCluster <- kmeans(gcr[, grp], 7, nstart = 50)
autoplot(gcrCluster,gcr[, grp])

#Question 4 #####
nrow(gcr)

gcr$Foreign.Worker <- as.factor(gcr$Foreign.Worker)
gcr$sex <- as.factor(gcr$sex)
gcr$IsSingle <- as.factor(gcr$IsSingle)
gcr$Status.of.account <-as.factor(gcr$Status.of.account)
gcr$purpose.of.loan <-as.factor(gcr$purpose.of.loan)
gcr$Credit.history <-as.factor(gcr$Credit.history)
gcr$Savings.account <-as.factor(gcr$Savings.account)
##gcr$Years.with.present.employert <-as.factor(gcr$Years.with.present.employer)
gcr$other.debtors.guarantors <-as.factor(gcr$other.debtors.guarantors)
gcr$Assets <-as.factor(gcr$Assets)
gcr$Other.installment.plans  <-as.factor(gcr$Other.installment.plans )
gcr$Housing  <-as.factor(gcr$Housing )
gcr$Job.Classification   <-as.factor(gcr$Job.Classification  )
gcr$Have.telephone   <-as.factor(gcr$Have.telephone  )


#Choose training data using cross validation strategy  
# Choose a testSet by commenting out the codes
testSet <-c(1:200)
#testSet <-c(201:400)
#testSet <-c(401:600)
#testSet <-c(601:800)
#testSet <-c(801:1003)

train <- gcr[-testSet,]
test <- gcr[testSet,]

model <- glm(Credit.offered ~., family=binomial(link="logit"), data=train)
summary(model)


# odds ratios and 95% CI
exp(cbind(OR = coef(model), confint(model)))

# reviewing the model using chisq test across each predictor in sequence 
# to determine the relative importance of each predictor
# null hypothesis is that the predictors are independent -- they have no contribution to the prediction
anova(model, test="Chisq")
# it is found that Status.of.account, Loan.Duration, Credit.history,
# Savings.account,Instalment rate in % of disposable income, 
# Years.with.present.employer,other.debtors.guarantors ,Foreign.Worker
# are significance, we rejected null hypothesis that they are independent (has no contribution to the model prediction )


model1 <- glm(Credit.offered ~Status.of.account+Loan.Duration+Credit.history+Savings.account+Other.installment.plans, family=binomial(link="logit"), data=train)
summary(model1)
anova(model1, test="Chisq")

model2 <- glm(Credit.offered ~Status.of.account+Loan.Duration+Credit.history+Savings.account, family=binomial(link="logit"), data=train)
summary(model2)
anova(model2, test="Chisq")

model3 <- glm(Credit.offered ~Status.of.account+Loan.Duration+Credit.history+Savings.account+Foreign.Worker, family=binomial(link="logit"), data=train)
summary(model3)
anova(model3, test="Chisq")

model4 <- glm(Credit.offered ~Status.of.account+Loan.Duration+Credit.history, family=binomial(link="logit"), data=train)
summary(model4)
anova(model4, test="Chisq")

model5 <- glm(Credit.offered ~Loan.Duration+Credit.history+Housing+Assets, family=binomial(link="logit"), data=train)
summary(model5)
anova(model5, test="Chisq")

model6 <- glm(Credit.offered ~Status.of.account
                              +Loan.Duration
                                +Credit.history
                              +purpose.of.loan
                               +Savings.account
                           +Other.installment.plans
                            +other.debtors.guarantors
                          +Age
                          +Foreign.Worker
                            ,
              family=binomial(link="logit"), data=train)
summary(model6)
anova(model6, test="Chisq")

#QUESTION 5 - MULTICOLLINEARIITY##############

#Correlation check against the chosen predictors 
myTrain<- train[,c(1,2,3,4,6,11,13,19)]

head(train[,c(1,2,3,6,11,13,19)])
#round(cor(myTrain),2)


# Status.of.account has a weak correlation with Savings.account and Credit.history
model7 <- glm(Credit.offered ~
               # Status.of.account+
              Loan.Duration
              +Credit.history
              +Savings.account
              +Other.installment.plans
              +Assets
              +Foreign.Worker,
              family=binomial(link="logit"), data=train)
summary(model7)
model8 <- glm(Credit.offered ~
              Status.of.account+
                Loan.Duration
              +Credit.history
             # +Savings.account
              +Other.installment.plans
              +Assets
              +Foreign.Worker,
              family=binomial(link="logit"), data=train)
summary(model8)

model9 <- glm(Credit.offered ~
                Status.of.account+
                Loan.Duration
              #+Credit.history
              +Savings.account
              +Other.installment.plans
              +Assets
              +Foreign.Worker,
              family=binomial(link="logit"), data=train)
summary(model9)
model
model6
model7
model8
model9
summary(model6)
# Model 6 still has the lowest AIC amongst them


#library(usdm)
#vif((model))

#Question 7 -Assessing the predictive ability of the model###########
library(caret)
fitted.results <- predict(model,newdata=subset(test,select=c(1:22)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Credit.offered)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data=fitted.results, reference=test$Credit.offered)

fitted.results <- predict(model6,newdata=subset(test,select=c(1:22)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Credit.offered)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(data=fitted.results, reference=test$Credit.offered)

# Confusion matrix




#Question 8 ROC Plots#########

library(ROCR)
p <- predict(model, newdata=subset(test,select=c(1:22)), type="response")
pr <- prediction(p, test$Credit.offered)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

p <- predict(model6, newdata=subset(test,select=c(1:22)), type="response")
pr <- prediction(p, test$Credit.offered)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#Area under the curve When using normalized units, the area under the curve 
#(often referred to as simply the AUC, or AUROC) is equal to the probability 
#that a classifier will rank a randomly chosen positive instance higher than a randomly chosen negative one
#(assuming 'positive' ranks higher than 'negative')

#Note that it is worse to class a customer as good when they are bad, 
#than it is to class a customer as bad when they are good