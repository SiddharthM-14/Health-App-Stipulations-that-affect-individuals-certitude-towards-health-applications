rm(list=ls())
library(rio)
library(moments)
library(missForest)
library(Hmisc)
library(mice)
library(VIM)
library(rms)
library(AER)
library(car)
library(carData)
library(caret)
library(caTools)
library(corrplot)
library(DescTools)
library(dplyr)
library(ggplot2)
library(lattice)
library(lme4)
library(lmtest)
library(MASS)
library(PerformanceAnalytics)
library(readxl)
library(rio)
library(ROCR)
library(sandwich)
library(stargazer)
library(survival)
library(tidyverse)
getwd()
setwd("C:/Users/gaura/Downloads/SDM Fall 21/Project")
#setwd("C:/Users/hital/SDM/SDM/Group Project")
df=import("SDM_GP_Medical_Data_updated.xlsx")

hist(df$health.wellness.apps)

#####Feature Engineering
#converting pounds to kg
hist(df$Weight)  
df$Weight=(df$Weight/2.20462262)
hist(df$Weight)
#converting height from inch to meter
hist(df$Height)
df$Height=(df$Height*0.0254)
hist(df$Height)
table(dfb$Smoke)
table(dfb$SmokeNow)
#calculating BMI
df$BMI=((df$Weight)/(df$Height)**2)
table(df$BMI)
hist(df$BMI)
df$BMI_CDC=ifelse(df$BMI<=18.5,"Under_Weight",
                  ifelse((df$BMI>=18.6 & df$BMI<=24.9),"Healthy",
                         ifelse((df$BMI>=25 & df$BMI<=29.9),"Overweight",
                                ifelse((df$BMI>=30& df$BMI<=35),"Class I Obese",
                                       ifelse((df$BMI>=35.1& df$BMI<=40),"Class II Obese",
                                              ifelse(df$BMI>40.1,"Class III Obese","Healthy"))))))
df$BMI_CDC=factor(df$BMI_CDC)
df$BMI_CDC=relevel(df$BMI_CDC,"Healthy")
table(df$BMI_CDC)
#Categorical variable - Lifestyle
hist(df$TimesModerateExercise)
hist(df$TimesStrengthTraining)
df$Excerciseinweek=round(((df$TimesModerateExercise+df$TimesStrengthTraining)/2),digits=0)
hist(df$Excerciseinweek)

df$Lifestyle=ifelse(df$Excerciseinweek>=5,"Active",
                    ifelse((df$Excerciseinweek>=2 & df$Excerciseinweek<=4),"Moderate",
                           ifelse(df$Excerciseinweek<=1,"Sedentary","Moderate")))
table(df$Lifestyle)
df$Lifestyle=factor(df$Lifestyle)
df$Lifestyle=relevel(df$Lifestyle,"Active")
hist(df$HowLongModerateExerciseMinutes)
hist(df$Age)
table(df$BMI_CDC)
table(df$health.wellness.apps)
unique(df)
hist(df$Used.Health.Wellness.Apps)
table(df$Used.Health.Wellness.Apps)
table(df$Wearable.Dev.Track.Health)
##Alcohol intake
hist(df$DrinksPerDay)
table(df$BirthGender)
df$AlcoholIntake=ifelse(df$DrinksPerDay<=1 & (df$BirthGender == 2|df$BirthGender == 0) | df$DrinksPerDay<=2 && df$BirthGender == 1,"Low",
                        ifelse((df$DrinksPerDay>=2 & df$DrinksPerDay<=3 & (df$BirthGender == 2|df$BirthGender == 0)  | df$DrinksPerDay>=3 & df$DrinksPerDay<=4 && df$BirthGender == 1),"Moderate",
                               ifelse(df$DrinksPerDay>=4 & (df$BirthGender == 2|df$BirthGender == 0)  | df$DrinksPerDay>=5 && df$BirthGender == 1,"High","Non Drinker")))
df$Gender=ifelse(df$BirthGender==2,"Female","Male")
table(df$Gender)
df$Gender=factor(df$Gender)
table(df$AlcoholIntake)
df$AlcoholIntake=factor(df$AlcoholIntake)
df$AlcoholIntake=relevel(df$AlcoholIntake,"Non Drinker")
df$BirthGender=factor(df$BirthGender)

##General Health
table(df$GeneralHealth)
df$Health_Cat = ifelse(df$GeneralHealth==1,"Healthy",
                       ifelse(df$GeneralHealth==2,"Healthy",
                              ifelse(df$GeneralHealth==3, "Fair",
                                     ifelse(df$GeneralHealth==4,"Fair",
                                            ifelse(df$GeneralHealth==5,"Poor","Healthy")))))
table(df$Health_Cat)
df$Health_Cat=factor(df$Health_Cat)
df$Health_Cat=relevel(df$Health_Cat,"Healthy")

##Medical Conditions
table(df$MedConditions_Cat)
df$MedConditions_Cat = ifelse(df$MedConditions_Cat==1,"Diabetes",
                              ifelse(df$MedConditions_Cat==2, "BP",
                                     ifelse(df$MedConditions_Cat==3,"Heart Condition",
                                            ifelse(df$MedConditions_Cat==4,"Lung Disease",
                                                   ifelse(df$MedConditions_Cat==5,"Anxiety","None")))))
table(df$MedConditions_Cat)
df$MedConditions_Cat=factor(df$MedConditions_Cat)
df$MedConditions_Cat=relevel(df$MedConditions_Cat,"None")
#Occupation
hist(df$Occupation) ##Missing data is high so we cannot use this

#Pandemic
table(df$Pandemic)
df$Pandemic = ifelse(df$Pandemic==1, "Yes", "No")
df$Pandemic=factor(df$Pandemic)
df$Pandemic=relevel(df$Pandemic,"No")
table(df$Pandemic)
#Race
table(df$Race)
df$Race = ifelse(df$Race==9,"No Race",
                 ifelse(df$Race==11,"White",
                        ifelse(df$Race==12,"African American",
                               ifelse(df$Race==14,"Asian",
                                      ifelse(df$Race==16,"Asian",
                                             ifelse(df$Race==31,"Asian",
                                                    ifelse(df$Race==32,"Asian",
                                                           ifelse(df$Race==33,"Asian",
                                                                  ifelse(df$Race==37,"Asian",
                                                                         ifelse(df$Race==54,"Asian","No Race"))))))))))
table(df$Race)

df$Race=factor(df$Race)
df$Race=relevel(df$Race,"No Race")
#########
####Model Building
dfb=subset(df,df$Used.Health.Wellness.Apps==1 | df$Used.Health.Wellness.Apps==2)
dfb$Used.Health.Wellness.Apps=ifelse(dfb$Used.Health.Wellness.Apps==2,0,1)
attach(dfb)
df$Used.Health.Wellness.Apps=factor(df$Used.Health.Wellness.Apps)
df$Used.Health.Wellness.Apps=relevel(df$Used.Health.Wellness.Apps,"0")
table(df$Used.Health.Wellness.Apps)
table(df$health.wellness.apps)
df$health.wellness.apps=factor(df$health.wellness.apps)
table(df$have.device)

##Have a Device
dfb$have.device=ifelse(dfb$have.device==1,"Tablet/Computer only",
                       ifelse(dfb$have.device==2, "Smartphone",
                              ifelse(dfb$have.device==5, "Multiple Devices",
                                     ifelse(dfb$have.device==0,"None","None"))))
table(dfb$have.device)
dfb$have.device=factor(dfb$have.device)
dfb$have.device=relevel(dfb$have.device,"Multiple Devices")

hist(dfb$Willing.Share.Data)

#Health Insurance
hist(dfb$Health.Insurance)
table(dfb$Health.Insurance)
dfb$Health.Insurance=ifelse(dfb$Health.Insurance==1,"Yes","No")
dfb$Health.Insurance=factor(dfb$Health.Insurance)

table(dfb$NotAccessed_ConcernedPrivacy)

#Caregiving
table(dfb$Caregiving)
dfb$Caregiving=ifelse(dfb$Caregiving==6,"No","Yes")
dfb$Caregiving=factor(dfb$Caregiving)

#Smoker
table(dfb$Smoke)
table(SmokeNow)
dfb$Smoker=ifelse(dfb$SmokeNow==1,"Chain Smokers",
                  ifelse(dfb$SmokeNow==2,"Occasional",
                         ifelse(dfb$SmokeNow==9,"Undisclosed","Non Smoker")))
dfb$Smoker=factor(dfb$Smoker)
dfb$Smoker=relevel(dfb$Smoker,"Non Smoker")
dfb$Smoke=NULL
dfb$SmokeNow=NULL
#Has a Partner
table(dfb$MaritalStatus)
dfb$HasPartner=ifelse(dfb$MaritalStatus<=2,"Yes","No")
dfb$HasPartner=factor(dfb$HasPartner)
dfb$MaritalStatus=NULL

#Income Class
table(dfb$IncomeRanges)
dfb$IncomeRanges=ifelse(dfb$IncomeRanges<=5,"Lower",
                        ifelse(dfb$IncomeRanges==6 | dfb$IncomeFeelings==7,"Middle",
                               ifelse(dfb$IncomeRanges==8 | dfb$IncomeFeelings==9,"Upper","Lower")))
table(dfb$IncomeRanges)
dfb$IncomeRanges=factor(dfb$IncomeRanges)
dfb$IncomeRanges=relevel(dfb$IncomeRanges,"Lower")

#table(dfb$often.use.of.internet)
##dfb$have_Device=ifelse(df$have.device==4,0,
## ifelse(df$have.device==3,0,
## ifelse(df$have.device==0,0,1)))
#dftest=subset(df,df$Used.Health.Wellness.Apps==1 | df$Used.Health.Wellness.Apps==0)

m1=glm(Used.Health.Wellness.Apps~have.device+Health.Insurance
       +Caregiving+BMI_CDC+Smoker+Age+Gender
       +HasPartner+Race+IncomeRanges+MedConditions_Cat+Health_Cat+Lifestyle
       +Pandemic+AlcoholIntake,data = dfb,family = binomial(link = "logit"))
table(dfb$Used.Health.Wellness.Apps)
hist(dfb$Used.Health.Wellness.Apps)
m2=glm(Used.Health.Wellness.Apps~have.device+Health.Insurance
       +Caregiving+BMI_CDC+Smoker+Age+Gender
       +HasPartner+Race+IncomeRanges+MedConditions_Cat+Health_Cat+Lifestyle
       +Pandemic+AlcoholIntake,data = dfb,family = binomial(link = "probit"))
summary(m2)
table(dfb$Used.Health.Wellness.Apps)
hist(dfb$Used.Health.Wellness.Apps)
# m1=glm(Used.Health.Wellness.Apps~health.wellness.apps+have.device+
#          Willing.Share.Data+Health.Insurance
#        +NotAccessed_ConcernedPrivacy+Smoke+Age+BMI+BirthGender
#        +MaritalStatus+Race+IncomeRanges+MedConditions_Cat+often.use.of.internet+Health_Cat
#        +Pandemic+AlcoholIntake,
#        data = df,family = binomial(link = "logit"))
summary(m1)
library(stargazer)
stargazer(m1,m2,single.row = TRUE,type = "text")
cbind( Coef = coef(m1), 
       Odd = exp(coef(m1)), 
       Prb = exp(coef(m1))/(1+exp(coef(m1))) ,
       Dst = abs(0.5 - exp(coef(m1))/(1+exp(coef(m1)))))
library(plotly)
g <- ggplot(dfb, aes(as.factor(Used.Health.Wellness.Apps)))
p <- g + geom_bar()+scale_fill_manual(values = c("0" = "red",
                                                 "1" = "orange"
))
ggplotly(p)
ggplot(data = dfb, aes(x = AlcoholIntake, fill = AlcoholIntake)) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("High Risk Drinking" = "red",
                               "Moderate Risk Drinking" = "orange",
                               "Low Risk Drinking" = "gold",
                               "No Risk Drinking" = "Green" ))
set.seed(1111)
dfb_sample = sample.split(dfb, SplitRatio = .75)
dfb_train = subset(dfb, dfb_sample == TRUE)
dfb_test = subset(dfb, dfb_sample == FALSE)
dfb_train$Used.Health.Wellness.Apps=as.factor(dfb_train$Used.Health.Wellness.Apps)
library(caret)
library(ROCR)
train_m1=glm(Used.Health.Wellness.Apps~have.device+Health.Insurance
             +Caregiving+BMI_CDC+Smoker+Age+Gender
             +HasPartner+Race+IncomeRanges+MedConditions_Cat+Health_Cat+Lifestyle
             +Pandemic+AlcoholIntake,data = dfb_train,family = binomial(link = "logit"))
dfb_test$Used.Health.Wellness.Apps=factor(dfb_test$Used.Health.Wellness.Apps)
dfb


pPred <- predict(train_m1, newdata=dfb_train, type="response")
pPred <- ifelse(pPred > 0.5, 1, 0)
pPred=as.factor(pPred)
dfb_train$Used.Health.Wellness.Apps=as.factor(dfb_train$Used.Health.Wellness.Apps)
length(pPred)
length(dfb_train$Used.Health.Wellness.Apps)
pCM <- table(dfb_train$Used.Health.Wellness.Apps, pPred)  
pPred <- pCM[2,2]/(pCM[2,2]+pCM[1,2])
pRec <- pCM[2,2]/(pCM[2,2]+pCM[2,1])
pF1 <- 2 * ((pRec * pPred) / (pRec + pPred))
dfb_train$Used.Health.Wellness.Apps=as.factor(dfb_train$Used.Health.Wellness.Apps)
#dfp_test$churn=factor(dfp_test$churn)


# Data Visualizations
dfb$UsedHealth.app=ifelse(dfb$Used.Health.Wellness.Apps==1,"Yes","No")
ggplot(data=dfb, aes(UsedHealth.app)) +
  geom_bar(
    col="black",
    aes(fill=..count..)) +
  scale_fill_gradient("Count", low="red", high="green")

ggplot(data = dfb, aes(x = UsedHealth.app, fill = UsedHealth.app)) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("Yes" = "Green",
                               "No" = "red"))
table(dfb$UsedHealth.app)

ggplot(data = dfb, aes(x = have.device, fill = UsedHealth.app)) +
  geom_bar(position = "fill") + ylab("proportion") +
  stat_count(geom = "text",
             aes(label = scale(PercentRank()),
                 position=position_fill(), colour="white"))

ggplot(data = dfb, aes(x = AlcoholIntake, fill = UsedHealth.app)) +
  geom_bar(position = "dodge")+scale_fill_manual(values = c("Yes" = "Green",
                                                            "No" = "red"))
dfb$BMI
hist(df$Age)

ggplot(df, aes(Age)) +
  geom_histogram()

qplot(df$Age,
      geom="histogram",
      binwidth = 5,
      main = "Histogram for Age",
      xlab = "Age",
      fill=I("blue"),
      col=I("red"),
      alpha=I(.2),
)
ggplot(data=df, aes(Age)) +
  geom_histogram(breaks=seq(10, 110, by=5),
                 col="black",
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low="green", high="red")

qplot(df$Age,
      geom="histogram",
      binwidth = 5,
      main = "Histogram for Age",
      xlab = "Age")

table(dfb$have.device)