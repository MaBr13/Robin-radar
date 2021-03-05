#Multinomial regression
#M. Bradaric
#11/01/2021

library(dplyr)

#data prep
birds1 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/spring_2019_br.csv",sep=";")
weather1 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/spring_2019_wr.csv",sep=";")
birds2 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/autumn_2019_br.csv",sep=";")
weather2 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/autumn_2019_wr.csv",sep=";")
birds3 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/spring_2020_br.csv",sep=";")
weather3 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/spring_2020_wr.csv",sep=";")
birds4 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/autumn_2020_br.csv",sep=";")
weather4 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/autumn_2020_wr.csv",sep=";")
rownames(weather1) <- NULL
rownames(weather2) <- NULL
rownames(weather3) <- NULL
rownames(weather4) <- NULL

birdsS <- rbind(birds1,birds3)
birdsA <- rbind(birds2,birds4)
weatherS <- rbind(weather1,weather3)
weatherA <- rbind(weather2,weather4)

Spring <- left_join(birdsS,weatherS,by=c("time","altitude_layer"),keep=FALSE)
Autumn <- left_join(birdsA,weatherA,by=c('time','altitude_layer'))
#check the numbers and proportions of birds at different altitudes per night
#SPRING
Spring$height <- with(Spring,ifelse(Spring$altitude_layer<=300 ,"low","high"))
Spring$height = factor(Spring$height, levels=c("high","low"), labels=c("high","low"))
Spring$night <- as.POSIXct(Spring$night.x,tz="UTC")

lowS <- subset(Spring,height=='low')
highS <- subset(Spring,height=='high')

Spring1 <- lowS %>%
  group_by(night.x) %>%
  summarise(cnt_l=sum(count,na.rm=TRUE),wind_ass_l=mean(wind_ass),temp_l=mean(temp),rh_l=mean(r),
            crwc_l=mean(crwc),cc_l=mean(cc))

Spring2 <- highS %>%
  group_by(night.x) %>%
  summarise(cnt_h=sum(count,na.rm=TRUE),wind_ass_h=mean(wind_ass),temp_h=mean(temp),rh_h=mean(r),
            crwc_h=mean(crwc),cc_h=mean(cc))

mydataS <- left_join(Spring1,Spring2,by="night.x")
mydataS$total <- rowSums(mydataS[,c(2,8)])
mydataS$cat <- with(mydataS,ifelse(mydataS$cnt_l>mydataS$cnt_h,1,ifelse(mydataS$cnt_l==mydataS$cnt_h,0,2)))
mydataS$perc_low <- (mydataS$cnt_l*100)/mydataS$total 
mydataS$perc_high <- (mydataS$cnt_h*100)/mydataS$total
mydataS$alt_cat <- with(mydataS,ifelse(mydataS$total<100,0,ifelse(mydataS$perc_low-mydataS$perc_high>=-5 & mydataS$perc_low-mydataS$perc_high<=0,0,
                                                                  ifelse(mydataS$perc_low-mydataS$perc_high<=5 & mydataS$perc_low-mydataS$perc_high>=0,0,
                                                                         ifelse(mydataS$perc_low-mydataS$perc_high < -5,2,1)))))
mydataS <- subset(mydataS, total!=0)
mydataS$alt_cat <- as.factor(mydataS$alt_cat)

#AUTUMN
Autumn$height <- with(Autumn,ifelse(Autumn$altitude_layer<=100 ,"low","high"))
Autumn$height = factor(Autumn$height, levels=c("high","low"), labels=c("high","low"))
Autumn$night <- as.POSIXct(Autumn$night.x,tz="UTC")

lowA <- subset(Autumn,height=='low')
highA <- subset(Autumn,height=='high')

Autumn1 <- lowA %>%
  group_by(night.x) %>%
  summarise(cnt_l=sum(count,na.rm=TRUE),wind_ass_l=mean(wind_ass),temp_l=mean(temp),rh_l=mean(r),
  crwc_l=mean(crwc),cc_l=mean(cc))

Autumn2 <- highA %>%
  group_by(night.x) %>%
  summarise(cnt_h=sum(count,na.rm=TRUE),wind_ass_h=mean(wind_ass),temp_h=mean(temp),rh_h=mean(r),
  crwc_h=mean(crwc),cc_h=mean(cc))

mydataA <- left_join(Autumn1,Autumn2,by="night.x")
mydataA$total <- rowSums(mydataA[,c(2,8)])
mydataA$cat <- with(mydataA,ifelse(mydataA$cnt_l>mydataA$cnt_h,1,ifelse(mydataA$cnt_l==mydataA$cnt_h,0,2)))
mydataA$perc_low <- (mydataA$cnt_l*100)/mydataA$total 
mydataA$perc_high <- (mydataA$cnt_h*100)/mydataA$total
mydataA$alt_cat <- with(mydataA,ifelse(mydataA$total<100,0,ifelse(mydataA$perc_low-mydataA$perc_high>=-5 & mydataA$perc_low-mydataA$perc_high<=0,0,
                                                                  ifelse(mydataA$perc_low-mydataA$perc_high<=5 & mydataA$perc_low-mydataA$perc_high>=0,0,
                                                                         ifelse(mydataA$perc_low-mydataA$perc_high < -5,2,1)))))
mydataA <- subset(mydataA, total!=0)
mydataA$alt_cat <- as.factor(mydataA$alt_cat)
# Loading the dplyr package
library(dplyr)

# Using sample_frac to create 70 - 30 slipt into test and train
trainS <- sample_frac(mydataS, 0.7)
sample_idS <- as.numeric(rownames(trainS)) # rownames() returns character so as.numeric
testS <- mydataS[-sample_idS,]

# Setting the basline 
trainS$alt_cat <- relevel(trainS$alt_cat, ref = "0")


# Loading the nnet package
require(nnet)
# Training the multinomial model
multinom.fit <- multinom(alt_cat ~ wind_ass_l + wind_ass_h, data = trainS)

# Checking the model
summary(multinom.fit)

## extracting coefficients from the model and exponentiate
exp(coef(multinom.fit))



head(probability.table <- fitted(multinom.fit))

# Predicting the values for train dataset
trainS$precticed <- predict(multinom.fit, newdata = trainS, "class")

# Building classification table
ctable <- table(trainS$alt_cat, trainS$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

# Predicting the values for train dataset
testS$precticed <- predict(multinom.fit, newdata = testS, "class")

# Building classification table
ctable <- table(testS$alt_cat, testS$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)
