#
# JA week 4 Assignment -- IMDB Movie Data
#
getwd()
setwd("/Users/yeapjiunaing/Documents/DSCT/Wk4")
getwd()

imdb_raw <- read.csv("movie_metadata.csv", sep=",",header=T,na.strings=c(""))

#1	color	                      11	actor_1_name	            21	country
#2	director_name	              12	movie_title	              22	content_rating
#3	num_critic_for_reviews	    13	num_voted_users	          23	budget
#4	duration	                  14	cast_total_facebook_likes	24	title_year
#5	director_facebook_likes	    15	actor_3_name	            25	actor_2_facebook_likes
#6	actor_3_facebook_likes	    16	facenumber_in_poster	    26	imdb_score
#7	actor_2_name	              17	plot_keywords	            27	aspect_ratio
#8	actor_1_facebook_likes	    18	movie_imdb_link	          28	movie_facebook_likes
#9	gross	                      19	num_user_for_reviews		
#10	genres	                    20	language		
#columnsChosen <-c(9,1,3,4,5,6,7,8,10,11,12,13,14,15,16,17,20,21,22,23,24,25)

library(varhandle)
#Question 2#########
imdb_raw$plot_keywords <-unfactor(imdb_raw$plot_keywords)
imdb_raw$actor_3_name <-unfactor(imdb_raw$actor_3_name)
imdb_raw$actor_2_name <-unfactor(imdb_raw$actor_2_name)
imdb_raw$actor_1_name <-unfactor(imdb_raw$actor_1_name)

# fill up those NA fields records with value when possible
imdb_raw$plot_keywords[is.na(imdb_raw$plot_keywords)]<-'-'
imdb_raw$num_user_for_reviews[is.na(imdb_raw$num_user_for_reviews)]<-1
imdb_raw$movie_facebook_likes[is.na(imdb_raw$movie_facebook_likes)]<-1
imdb_raw$actor_3_facebook_likes[is.na(imdb_raw$actor_3_facebook_likes)]<-1
imdb_raw$actor_2_facebook_likes[is.na(imdb_raw$actor_2_facebook_likes)]<-1
imdb_raw$actor_1_facebook_likes[is.na(imdb_raw$actor_1_facebook_likes)]<-1
imdb_raw$director_facebook_likes[is.na(imdb_raw$director_facebook_likes)]<-1
imdb_raw$facenumber_in_poster[is.na(imdb_raw$facenumber_in_poster)]<-1
imdb_raw$color[is.na(imdb_raw$color)]<-0
imdb_raw$actor_3_name[is.na(imdb_raw$actor_3_name)]<-'-'
imdb_raw$actor_2_name[is.na(imdb_raw$actor_2_name)]<-'-'
imdb_raw$actor_1_name[is.na(imdb_raw$actor_1_name)]<-'-'
imdb_raw$director_name[is.na(imdb_raw$director_name)]<-'-'
imdb_raw$director_facebook_likes[is.na(imdb_raw$director_facebook_likes)]<-0
imdb_raw$content_rating[is.na(imdb_raw$content_rating)]<-'Not Rated'

nrow(imdb_raw)
# Clean up those records having incomplete data
imdb_clean <-imdb_raw[complete.cases(imdb_raw),]
nrow(imdb_clean)
# Clean up those duplicate records
imdb_clean <- unique(imdb_clean)
nrow(imdb_clean)

imdb_clean$grossInMillions <-(imdb_clean$gross/1000000) ##in millions
imdb_clean$grossInMillionsInlog <-log(imdb_clean$grossInMillions) ##in millions
imdb_clean$budgetInMillions  <-(imdb_clean$budget/1000000) ##in millions
imdb_clean$profitInMillions  <-imdb_clean$grossInMillions-imdb_clean$budgetInMillions
imdb_clean$profitInMillionsInLog  <-log(imdb_clean$grossInMillions-imdb_clean$budgetInMillions)

imdb_clean$profitPercentage  <-imdb_clean$profitInMillions/imdb_clean$budgetInMillions
imdb_clean$profitPercentageInLog  <-imdb_clean$profitInMillions/imdb_clean$budgetInMillions

OverallGrossOverBudget  <-mean(imdb_clean$grossInMillions)/mean(imdb_clean$budgetInMillions)

# Calculatate Director Past Movie Avg Gross
# Exclude movie gross of a current record 
# to avoid indirect feeding of the gross amount which the regression model supposed to predict
# in SQL overall sum by direcotor/actor will first be obtained. 
# After it will be adjusted by knock-off current gross amt (after SQl)

## Get Director gross in millions

imdb_clean <-sqldf("select a.*, (b.gross) as directorTotalGross,  b.cnt as directorTotalCnt
                   from imdb_clean a,
                   (select director_name, sum(grossInMillions) as gross, count(1) as Cnt
                   from imdb_clean
                   group by director_name) b
                   where a.director_name = b.director_name 
                   ")

## Get 3 top actors gross in millions

imdb_clean <-sqldf("select a.*, 
                   (
                   select sum(b.grossInMillions) from imdb_clean b
                   where (b.actor_1_name = a.actor_1_name) or
                   (b.actor_2_name = a.actor_1_name) or
                   (b.actor_3_name = a.actor_1_name)
                   ) as actor1TotalGross,
                   (
                   select count(1) from imdb_clean b
                   where (b.actor_1_name = a.actor_1_name) or
                   (b.actor_2_name = a.actor_1_name) or
                   (b.actor_3_name = a.actor_1_name)
                   ) as actor1TotalCnt,
                   (
                   select sum(b.grossInMillions) from imdb_clean b
                   where (b.actor_1_name = a.actor_2_name) or
                   (b.actor_2_name = a.actor_2_name) or
                   (b.actor_3_name = a.actor_2_name)
                   ) as actor2TotalGross,
                   (
                   select count(1) from imdb_clean b
                   where (b.actor_1_name = a.actor_2_name) or
                   (b.actor_2_name = a.actor_2_name) or
                   (b.actor_3_name = a.actor_2_name)
                   ) as actor2TotalCnt,
                   (
                   select sum(b.grossInMillions) from imdb_clean b
                   where (b.actor_1_name = a.actor_3_name) or
                   (b.actor_2_name = a.actor_3_name) or
                   (b.actor_3_name = a.actor_3_name)
                   ) as actor3TotalGross,
                   (
                   select count(1) from imdb_clean b
                   where (b.actor_1_name = a.actor_3_name) or
                   (b.actor_2_name = a.actor_3_name) or
                   (b.actor_3_name = a.actor_3_name)
                   ) as actor3TotalCnt
                   from imdb_clean a
                   ")

mean(imdb_clean$grossInMillions)
# Adjust directorPastAvgGross as the number actually counting itself in SQL
imdb_clean$directorPastAvgGross<-(imdb_clean$directorTotalGross-(imdb_clean$grossInMillions))/(imdb_clean$directorTotalCnt-1)
#imdb_clean$directorPastAvgGross<-(imdb_clean$directorTotalGross)/(imdb_clean$directorTotalCnt)
imdb_clean$directorPastAvgGross[is.na(imdb_clean$directorPastAvgGross)]<-imdb_clean$budgetInMillions[is.na(imdb_clean$directorPastAvgGross)]

imdb_clean$actor1PastAvgGross<-(imdb_clean$actor1TotalGross-(imdb_clean$grossInMillions))/(imdb_clean$actor1TotalCnt-1)
imdb_clean$actor1PastAvgGross[is.na(imdb_clean$actor1PastAvgGross)]<-0.0
#imdb_clean$actor1PastAvgGross[is.na(imdb_clean$actor1PastAvgGross)]<-imdb_clean$budgetInMillions[is.na(imdb_clean$actor1PastAvgGross)]

imdb_clean$actor2PastAvgGross<-(imdb_clean$actor2TotalGross-(imdb_clean$grossInMillions))/(imdb_clean$actor2TotalCnt-1)
imdb_clean$actor2PastAvgGross[is.na(imdb_clean$actor2PastAvgGross)]<-0.0
#imdb_clean$actor2PastAvgGross[is.na(imdb_clean$actor2PastAvgGross)]<-imdb_clean$budgetInMillions[is.na(imdb_clean$actor2PastAvgGross)]

imdb_clean$actor3PastAvgGross<-(imdb_clean$actor3TotalGross-(imdb_clean$grossInMillions))/(imdb_clean$actor3TotalCnt-1)
imdb_clean$actor3PastAvgGross[is.na(imdb_clean$actor3PastAvgGross)]<-0.0
#imdb_clean$actor3PastAvgGross[is.na(imdb_clean$actor3PastAvgGross)]<-imdb_clean$budgetInMillions[is.na(imdb_clean$actor3PastAvgGross)]
imdb_clean$top3ActorsPastAvgGross<-imdb_clean$actor1PastAvgGross+imdb_clean$actor2PastAvgGross+imdb_clean$actor3PastAvgGross
#imdb_clean$top3ActorsPastAvgGross[(imdb_clean$top3ActorsPastAvgGross)==0]<-imdb_clean$budgetInMillions[(imdb_clean$top3ActorsPastAvgGross)==0]


imdb_clean <-sqldf("select a.*, b.cnt as languageCnt
                   from imdb_clean a,
                   (select language, count(1) as cnt
                   from imdb_clean
                   group by language) b
                   where a.language = b.language")
# Majority of the records are english by checking the languageCnt
# Create a category isEnglish to categorize as english and non-english
imdb_clean$IsEnglish<-0
imdb_clean$IsEnglish[imdb_clean$language =='English']<-1

imdb_clean <-sqldf("select a.*,b.cnt as countryCnt
                   from imdb_clean a,
                   (select country, count(1) as cnt
                   from imdb_clean
                   group by country) b
                   where a.country = b.country")

# Question 3 ####################################################

# Transform genres, countries and movie languages into dichotomous variables.
is.factor(imdb_clean$language)
is.factor(imdb_clean$genres)
is.factor(imdb_clean$country)
is.factor(imdb_clean$plot_keywords)
#contrasts(imdb_clean$genres)

# Action	Adventure	Animation
# Biography	Comedy	Crime
# Documentary	Drama	Family
# Fantasy	Film-Noir	Game-Show
# History	Horror	Music
# Musical	Mystery	Romance
# Sci-Fi	Thriller	Western
# Sport	War	Short
# News	Reality-TV	
#                                123456789012345678901234
regexes <- list(c("(Action)"    ,100000000000000000000000),
                c("(Adventure)" , 10000000000000000000000),
                c("(Animation)" ,  1000000000000000000000),
                c("(Comedy)" ,      100000000000000000000),
                c("(Crime)" ,        10000000000000000000),
                c("(Documentary)" ,   1000000000000000000),
                c("(Drama)" ,          100000000000000000),
                c("(Family)" ,          10000000000000000),
                c("(Fantasy)" ,          1000000000000000),
                c("(Film-Noir)" ,         100000000000000),
                c("(Game-Show)" ,          10000000000000),
                c("(History)" ,             1000000000000),
                c("(Horror)" ,               100000000000),
                c("(Music)" ,                 10000000000),
                c("(Musical)" ,                1000000000),
                c("(Mystery)" ,                 100000000),
                c("(Romance)" ,                  10000000),
                c("(Sci-Fi)" ,                    1000000),
                c("(Thriller)" ,                   100000), 
                c("(Western)" ,                     10000),
                c("(Sport)" ,                        1000),
                c("(War)" ,                           100),
                c("(Short)" ,                          10),                
                c("(Reality-TV)" ,                      1),
                c("(News)" ,                            0)
                )

#Create a vector, the same length as the df
#output_vector <- numeric(nrow(imdb_clean))
#output_vectorSum<- numeric(nrow(imdb_clean))
output_vector <- character(nrow(imdb_clean))
# output_vectorSum <-output_vector
# output_vectorSum <-"0000000000000000000000000"

#For each regex..
for(i in seq_along(regexes)){
#  for(i in seq_along(regexes)){  
  #Grep through d$name, and when you find matches, insert the relevant 'tag' into
  #The output vector
 # print(i )
  print(regexes[[i]][1] )
  output_vector <-"0"
  output_vector[grepl(x = imdb_clean$genres, pattern = regexes[[i]][1])] <- "1"
  output_vector[is.na(output_vector)]<-'0'
  # print(sum(as.numeric(output_vector)))
  # output_vectorSum<- paste( substring(output_vectorSum,1,i-1 ), output_vector,substring(output_vectorSum,i+1 ), sep = '')
  
  if (regexes[[i]][1] =='(Action)') {
    imdb_clean$genres_action<-output_vector
  }else if (regexes[[i]][1] =='(Adventure)') {
    imdb_clean$genres_Adventure<-output_vector
  }else if (regexes[[i]][1] =='(Animation)') {
    imdb_clean$genres_Animation<-output_vector
  }else if (regexes[[i]][1] =='(Comedy)') {
    imdb_clean$genres_Comedy<-output_vector
  }else if (regexes[[i]][1] =='(Crime)') {
    imdb_clean$genres_Crime<-output_vector
  }else if (regexes[[i]][1] =='(Drama)') {
    imdb_clean$genres_Drama<-output_vector
  }else if (regexes[[i]][1] =='(Family)') {
    imdb_clean$genres_Family<-output_vector
  }else if (regexes[[i]][1] =='(Fantasy)') {
    imdb_clean$genres_Fantasy<-output_vector
  }else if (regexes[[i]][1] =='(Horror)') {
    imdb_clean$genres_Horror<-output_vector
  }else if (regexes[[i]][1] =='(Mystery)') {
    imdb_clean$genres_Mystery<-output_vector
  }else if (regexes[[i]][1] =='(Romance)') {
    imdb_clean$genres_Romance<-output_vector
  }else if (regexes[[i]][1] =='(Sci-Fi)') {
    imdb_clean$genres_SciFi<-output_vector
  }else if (regexes[[i]][1] =='(Thriller)') {
    imdb_clean$genres_Thriller<-output_vector
  }
} 


imdb_clean$fromUSA<-0
imdb_clean$fromUSA[imdb_clean$country =='USA']<-1
imdb_clean$fromUK<-0
imdb_clean$fromUK[imdb_clean$country =='UK']<-1
imdb_clean$fromFrance<-0
imdb_clean$fromFrance[imdb_clean$country =='France']<-1
imdb_clean$fromGermany<-0
imdb_clean$fromGermany[imdb_clean$country =='Germany']<-1

imdb_profitMovie <- imdb_clean[imdb_clean$profitInMillions >=0,]


#imdb_clean$actors_Facebooklike <-imdb_clean$actor_1_facebook_likes + imdb_clean$actor_2_facebook_likes+ imdb_clean$actor_3_facebook_likes
# Question 4###############

imdb_budgetinMillions <- imdb_clean[imdb_clean$budgetInMillions >=1,]
nrow(imdb_budgetinMillions)
testSetA <- c(0:300)
testSetB <- c(200:500)
testSetC <- c(100:200, 800:900, 1500:1600)
testSetD <- c(1000:1100, 2000:2100, 3000:3100)
testSetE <- c(1500:1600, 2500:2600, 3500:3600)
#testSetC
testSet <- testSetE
imdb_training <- imdb_budgetinMillions[-testSet,]

imdb_test <- imdb_budgetinMillions[testSet,]
nrow(imdb_training)
nrow(imdb_test)

#1	color	                      11	actor_1_name	            21	country
#2	director_name	              12	movie_title	              22	content_rating
#3	num_critic_for_reviews	    13	num_voted_users	          23	budget
#4	duration	                  14	cast_total_facebook_likes	24	title_year
#5	director_facebook_likes	    15	actor_3_name	            25	actor_2_facebook_likes
#6	actor_3_facebook_likes	    16	facenumber_in_poster	    26	imdb_score
#7	actor_2_name	              17	plot_keywords	            27	aspect_ratio
#8	actor_1_facebook_likes	    18	movie_imdb_link	          28	movie_facebook_likes
#9	gross	                      19	num_user_for_reviews		
#10	genres	                    20	language	
PredictGrossModel<-NULL
PredictGrossModel <- lm((grossInMillionsInlog) ~ 
                          log(budgetInMillions) + 
                          log(directorPastAvgGross)+
                          #log(top3ActorsPastAvgGross)+
                          #title_year+
                          (cast_total_facebook_likes) + 
                          (duration) +
                          fromUSA+
                          fromUK+fromGermany+fromFrance+
                          IsEnglish +
                          +log(num_critic_for_reviews)+log(num_user_for_reviews)+
                          log(num_voted_users)+
                          # #          actors_Facebooklike+
                          # #                       genres_new+
                          genres_action+genres_Adventure+genres_Animation+genres_Comedy+
                          genres_Drama+genres_Family+genres_Horror
                        +genres_Fantasy+genres_Crime++genres_Mystery+
                          genres_Romance+genres_SciFi+genres_Thriller
                        , data = imdb_training)
summary(PredictGrossModel)


PredictProfitModel<-NULL
PredictProfitModel <- lm((profitPercentageInLog) ~ 
                        log(budgetInMillions) + 
                       log (directorPastAvgGross)+
                        #(top3ActorsPastAvgGross)+
                         #title_year+
                         (cast_total_facebook_likes) + 
                         log(duration) +
                         fromUSA+
                        #fromUK+fromGermany+fromFrance+
                         IsEnglish +
                         +log(num_critic_for_reviews)+log(num_user_for_reviews)+
                         #(num_voted_users)+
                        # #          actors_Facebooklike+
                        # #                       genres_new+
                         genres_action+genres_Adventure+genres_Animation+genres_Comedy+
                         genres_Drama+genres_Family+genres_Horror
                         # +genres_Fantasy+genres_Crime++genres_Mystery+
                         # genres_Romance+genres_SciFi+genres_Thriller
                      , data = imdb_training)
summary(PredictProfitModel)
#predictionModel <- PredictProfitModel
# predictionModel <- step(predictionModel1,direction="both")
# summary(predictionModel)

#Testing the prediction model

# Choose a model to test against test set
predictionModel <- PredictGrossModel 
predictionTesting <- predict(predictionModel, newdata = imdb_test)
head(exp(predictionTesting))
head(imdb_test$grossInMillions)
head(imdb_test$profitInMillions)

n<-nrow(imdb_test)
sumSquareError<-sum((imdb_test$grossInMillions- exp(predictionTesting))^ 2)
meanSquareError<-sumSquareError/n
rootMeanSquareError<-sqrt(meanSquareError)
print(rootMeanSquareError)
#Question 6 ##############

confint(predictionModel, 'duration', level=0.95)

#Question 8###################


# Perform a correlation check on the variables
colnames(imdb_clean)
#(budgetInMillions) + (directorPastAvgGross)+(cast_total_facebook_likes) + 
#duration + fromUSA  +(num_critic_for_reviews)+(num_user_for_reviews)+(num_voted_users)+
imdb_clean_subset<- imdb_clean[,c(31,44, 14,4,  3,19,13)]
round(cor(imdb_clean_subset),2)


#                 num_user_for_reviews and num_voted_users        having 0.78 correlation
#  follows by btw num_voted_users      and num_critic_for_reviews having 0.60 correlation
#  follows by btw num_user_for_reviews and num_critic_for_reviews having 0.57 correlation
#  Propose to drop num_voted_users as predictor

# Question 9 ##############
# #1467,285 users voted for the movie
# Action Adventure genre
# Originates from USA
# Movie duration is 109 minutes
# 750 reviews from critics
# 980 reviews from users 

Qns9predictionModel <-NULL
Qns9predictionModel <- lm((grossInMillionsInlog) ~ 
                          log(budgetInMillions) + 
                          log(directorPastAvgGross)+
                          #log(top3ActorsPastAvgGross)+
                          #title_year+
                          (cast_total_facebook_likes) + 
                          (duration) +
                          fromUSA+
                          fromUK+fromGermany+fromFrance+
                          IsEnglish +
                          +log(num_critic_for_reviews)+log(num_user_for_reviews)+
                          log(num_voted_users)+
                          actor_fa
                          # #                       genres_new+
                          genres_action+genres_Adventure+genres_Animation+genres_Comedy+
                          genres_Drama+genres_Family+genres_Horror
                        +genres_Fantasy+genres_Crime++genres_Mystery+
                          genres_Romance+genres_SciFi+genres_Thriller
                        , data = imdb_training)
summary(Qns9predictionModel)


Qn9TestModel <-NULL
Qn9TestModel$duration <- 109
Qn9TestModel$genres_action <-"1"
Qn9TestModel$genres_Adventure <-"1"
Qn9TestModel$genres_Animation <-"0"
Qn9TestModel$genres_Comedy <-"0"
Qn9TestModel$genres_Crime <-"0"
Qn9TestModel$genres_Drama <-"0"
Qn9TestModel$genres_Family <-"0"
Qn9TestModel$genres_Fantasy <-"0"
Qn9TestModel$genres_Horror <-"0"
Qn9TestModel$genres_Mystery <-"0"
Qn9TestModel$genres_Romance <-"0"
Qn9TestModel$genres_SciFi <-"0"
Qn9TestModel$genres_Thriller <-"0"
Qn9TestModel$fromUSA<-1
Qn9TestModel$fromUK<-0
Qn9TestModel$fromGermany<-0
Qn9TestModel$fromFrance<-0

Qn9TestModel$IsEnglish<-1
Qn9TestModel$num_critic_for_reviews <- 750
Qn9TestModel$num_user_for_reviews <- 980
Qn9TestModel$num_voted_users <- 1467285
# Derive the values for budgetInMillions, cast_total_facebook_likes and directorPastAvgGross
# by taking an average from the training data 
# Rather taking a whole population mean, choose the sample data more closer to given attributes

avgValues <-sqldf("select avg(budgetInMillions) as avgBudget,
                          avg(cast_total_facebook_likes) as avgCastTotalFacebookLikes,
                          avg(directorPastAvgGross) as avgdirectorPastAvgGross,
                          count(1)
                   from imdb_training
                    where fromUSA= 1 and genres_action ='1'
                    and genres_Adventure ='1'
                    and duration >80 
                    and num_voted_users >100000
                  ")

Qn9TestModel$budgetInMillions <-  avgValues$avgBudget
Qn9TestModel$cast_total_facebook_likes <- avgValues$avgCastTotalFacebookLikes
Qn9TestModel$directorPastAvgGross <-avgValues$avgdirectorPastAvgGross

Qns9predictionTesting <- predict(Qns9predictionModel, newdata = Qn9TestModel)
print(exp(Qns9predictionTesting))

#Build another model without budgetInMillions,directorPastAvgGross and cast_total_facebook_likes to predict
Qns9predictionModelSimplify<- NULL
Qns9predictionModelSimplify <- lm((grossInMillionsInlog) ~ 
                            #log(budgetInMillions) + 
                            #log(directorPastAvgGross)+
                            #log(top3ActorsPastAvgGross)+
                            #title_year+
                            #(cast_total_facebook_likes) + 
                            (duration) +
                            fromUSA+
                            fromUK+fromGermany+fromFrance+
                            IsEnglish +
                            +log(num_critic_for_reviews)+log(num_user_for_reviews)+
                            log(num_voted_users)+
                            # #          actor_2_facebook_likes+
                            # #                       genres_new+
                            genres_action+genres_Adventure+genres_Animation+genres_Comedy+
                            genres_Drama+genres_Family+genres_Horror
                          +genres_Fantasy+genres_Crime++genres_Mystery+
                            genres_Romance+genres_SciFi+genres_Thriller
                          , data = imdb_training)
summary(Qns9predictionModelSimplify)
Qns9predictionAnotherTesting <- predict(Qns9predictionModelSimplify, newdata = Qn9TestModel)
print(exp(Qns9predictionAnotherTesting))

# Question 10  ##############

# 2,689,764 users voted for the movie
# 50% increase in movie production budget as compared to the prequel
# Action Adventure genre
# Originates from USA
# 60% more likes on the movie's official Facebook page
# 30% more likes on Actor 1's Facebook page
# 18% more likes on Actor 2's Facebook page
# Movie duration is 125 minutes
# 913 reviews from critics
# 5,060 reviews from users

Qn10TestModel <-Qn9TestModel
Qn10TestModel$num_voted_users <- 2689764
Qn10TestModel$budgetInMillions <- Qn9TestModel$budgetInMillions*1.5 # having more budget
Qn10TestModel$duration <- 125
Qn10TestModel$genres_action <-"1"
Qn10TestModel$genres_Adventure <-"1"
Qn10TestModel$genres_Animation <-"0"
Qn10TestModel$genres_Comedy <-"0"
Qn10TestModel$genres_Crime <-"0"
Qn10TestModel$genres_Drama <-"0"
Qn10TestModel$genres_Family <-"0"
Qn10TestModel$genres_Fantasy <-"0"
Qn10TestModel$genres_Horror <-"0"
Qn10TestModel$genres_Mystery <-"0"
Qn10TestModel$genres_Romance <-"0"
Qn10TestModel$genres_SciFi <-"0"
Qn10TestModel$genres_Thriller <-"0"
Qn10TestModel$fromUSA<-1
Qn10TestModel$fromUK<-0
Qn10TestModel$fromGermany<-0
Qn10TestModel$fromFrance<-0

Qn10TestModel$IsEnglish<-1
Qn10TestModel$num_critic_for_reviews <- 913
Qn10TestModel$num_user_for_reviews <- 5063

Qn10predictionTesting <- predict(Qns9predictionModel, newdata = Qn10TestModel)
print(exp(Qn10predictionTesting))

confint(Qns9predictionModel, 'log(budgetInMillions)', level=0.95)
confint(Qns9predictionModel, 'log(num_voted_users)', level=0.95)
