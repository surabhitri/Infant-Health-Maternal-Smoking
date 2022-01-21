###########################################################################
###########################################################################
################## Maternal Smoking and Gestational Age ###################
###########################################################################
###########################################################################

###### Clear environment and load libraries
rm(list = ls())
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)
library(dplyr)
library(rms) #for VIF
library(MASS)

###### Load the data
smoking <- read.csv("smoking.csv")
smoking <- smoking[c(-2,-4)]

#making the response variable
smoking$premature[smoking$gestation < 270] <- 1
smoking$premature[smoking$gestation >= 270] <- 0

#Collapsing race and education categories for easier analysis
smoking$med[smoking$med == 7] <- 6
smoking$mrace[smoking$mrace == 1 | smoking$mrace == 2 | smoking$mrace == 3 | smoking$mrace == 4 | smoking$mrace == 5] <- 0

#converting vars from num to factor
smoking[,'mrace']<-factor(smoking[,'mrace'])
smoking[,'med']<-factor(smoking[,'med'])
smoking[,'inc']<-factor(smoking[,'inc'])
smoking[,'smoke']<-factor(smoking[,'smoke'])
smoking[,'premature']<-factor(smoking[,'premature'])
smoking <- smoking[c(-2)]

#looking at the data
summary(smoking)
describe(smoking)
table(smoking$premature)
#the data may not as balanced in the response variable as we would want it to be

###### Exploratory data analysis

#EDA for continuous variables
# parity vs premature
ggplot(smoking,aes(x=premature, y=parity, fill=premature)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Parity vs Premature",
       x="Had premature baby?",y="Parity") + 
  theme_classic() + theme(legend.position="none")
#no difference in median

# mage vs premature
ggplot(smoking,aes(x=premature, y=mage, fill=premature)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="mage vs Premature",
       x="Had premature baby?",y="Mother's age") + 
  theme_classic() + theme(legend.position="none") 
#can see some difference in median - might be a variable of interest

# mht vs premature
ggplot(smoking,aes(x=premature, y=mht, fill=premature)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="mht vs Premature",
       x="Had premature baby?",y="Mother's height") + 
  theme_classic() + theme(legend.position="none") 
#no difference in median

# mpregwt vs premature
ggplot(smoking,aes(x=premature, y=mpregwt, fill=premature)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="mpregwt vs Premature",
       x="Had premature baby?",y="Mother's pre-pregnancy weight") + 
  theme_classic() + theme(legend.position="none")
#can see some difference in median - var might be of interest

#EDA for categorical variables
# premature vs mrace
apply(table(smoking[,c("premature","mrace")])/sum(table(smoking[,c("premature","mrace")])),
      2,function(x) x/sum(x)) 
#the chances of having a premature baby when you're white are considerably lower than when you're black 
# - don't want to comment on other races as the data is not sufficient for other races
#chi-square test
chisq.test(table(smoking[,c("premature","mrace")]))
#it's significant

# premature vs med
apply(table(smoking[,c("premature","med")])/sum(table(smoking[,c("premature","med")])),
      2,function(x) x/sum(x)) 
#can see difference in probabilities - is lower for people with higher levels of education 
#but also confusing because for example mothers who are college grads have a 16% probability of having a premature baby
#as compared to 23% for high school grads + trade school (level 3)
#chi-square test
chisq.test(table(smoking[,c("premature","med")]))
#it's significant

# premature vs inc
apply(table(smoking[,c("premature","inc")])/sum(table(smoking[,c("premature","inc")])),
      2,function(x) x/sum(x)) 
#the probabilities are almost the same ~18-19% except for level 1 (26%) income mothers and level 8 (6%)
#chi-square test
chisq.test(table(smoking[,c("premature","inc")]))
#it's not significant

# premature vs smoke
apply(table(smoking[,c("premature","smoke")])/sum(table(smoking[,c("premature","smoke")])),
      2,function(x) x/sum(x)) 
#can see a significant difference in probabilities :D
#chi-square test
chisq.test(table(smoking[,c("premature","smoke")]))
#it's not significant


#Let's check binned probabilities of continuous variables with premature
###Checking if we need any transformations
#premature vs parity
smoking$prem_inter <- as.numeric(smoking$premature)
smoking$prem_inter[smoking$premature == '1'] <- 1
smoking$prem_inter[smoking$premature == '0'] <- 0
par(mfrow=c(1,1)) 
binnedplot(y=smoking$prem_inter,smoking$parity,xlab="Parity",ylim=c(0,1),col.pts="navy",
           ylab ="Had premature baby?",main="Binned Parity and Premature cases",
           col.int="white") 
#not enough data

#premature vs mage
par(mfrow=c(1,1)) 
binnedplot(y=smoking$prem_inter,smoking$mage,xlab="mage",ylim=c(0,1),col.pts="navy",
           ylab ="Had premature baby?",main="Binned Mother's age and Premature cases",
           col.int="white") 
#doesn't look like there's a pattern

#premature vs mht
par(mfrow=c(1,1)) 
binnedplot(y=smoking$prem_inter,smoking$mht,xlab="mht",ylim=c(0,1),col.pts="navy",
           ylab ="Had premature baby?",main="Binned Mother's height and Premature cases",
           col.int="white") 
#not enough data

#premature vs mpregwt
par(mfrow=c(1,1)) 
binnedplot(y=smoking$prem_inter,smoking$mpregwt,xlab="mpregwt",ylim=c(0,1),col.pts="navy",
           ylab ="Had premature baby?",main="Binned Mother's pre-pregnancy weight and Premature cases",
           col.int="white") 
#decreasing increasing trend - we'll look at binned residuals later 

###### Model fitting
model1 <- glm(premature ~ parity + mrace + mage + med + mht + mpregwt + inc + smoke, data = smoking, family = binomial)
summary(model1)

#model assessment

#save the raw residuals
rawresid1 <- residuals(model1,"resp")

##binned residual plots for continuous variables
binnedplot(x=fitted(model1),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#no points outside the 95% standard error lines so that's good. But the model is not very random so let's go ahead and see what is happening
#with the continuous variables

#mage
binnedplot(x=smoking$mage,y=rawresid1,xlab="Mother's age",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks good

#mht
binnedplot(x=smoking$mht,y=rawresid1,xlab="Mother's height",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#might be a trend there - will need a transformation maybe
#categories - 60-63, 63-66
#let's look at average residuals by mht using the tapply command
plot(0:17,tapply(rawresid1, smoking$mht, mean),col='blue4',pch=10)
table(smoking[,c("premature","mht")])

#mpregwt
binnedplot(x=smoking$mpregwt,y=rawresid1,xlab="Mother's pre-pregnancy weight",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks good

#parity
binnedplot(x=smoking$parity,y=rawresid1,xlab="Parity",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#not enough data
table(smoking[,c("premature","parity")])
#maybe convert into categories - 0, 1-3,4

##tables for factor variables
#smoke
tapply(rawresid1, smoking$smoke, mean) 

#mrace
tapply(rawresid1, smoking$mrace, mean) 

###### Model validation
#let's do the confusion matrix with .5 threshold
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model1) >= 0.5, "1","0")),
                            as.factor(smoking$premature),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate
#we're not able to identify premature birth in mothers but we are identifying non premature births well - maybe because we don't have enough data
#for premature births?

#first, let's repeat with the marginal percentage in the data
mean(smoking$prem_inter)
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model1) >= mean(smoking$prem_inter), "1","0")),
                            as.factor(smoking$premature),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
#this looks more balanced 

#look at ROC curve
roc(smoking$premature,fitted(model1),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
#AUC = 66.7%

#check multicollinearity
vif(model1)
#med might be correlated with one of the predictors - removing med is making smoke significant at the 0.05 level hurray - or we can include
#interactions between med

#### Model transformations
#let's convert mht to two categories - DID NOT WORK
smoking$mht_new <- rep(0,nrow(smoking))
smoking$mht_new[smoking$mht > 63] <- 1
table(smoking$mht,smoking$mht_new)

model2 <- glm(premature ~ parity + mrace + mage + med + mht_new + mpregwt + inc + smoke, data = smoking, family = binomial)
summary(model2)

rawresid2 <- residuals(model2,"resp")

##binned residual plots for continuous variables
binnedplot(x=fitted(model2),y=rawresid2,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#feels worse than model1 - it has few outliers and isn't random

#roc
roc(smoking$premature,fitted(model2),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
#AUC = 66.8% - very very small improvement

#let's convert mht to 5 categories
smoking$mht_new <- rep(0,nrow(smoking))
smoking$mht_new[smoking$mht <= 61] <- 1
smoking$mht_new[smoking$mht > 61 & smoking$mht <= 63] <- 2
smoking$mht_new[smoking$mht > 63 & smoking$mht <= 65] <- 3
smoking$mht_new[smoking$mht > 65 & smoking$mht <= 67] <- 4
smoking$mht_new[smoking$mht > 67] <- 5
smoking[,'mht_new']<-factor(smoking[,'mht_new'])

table(smoking$mht,smoking$mht_new)

model2 <- glm(premature ~ parity + mrace + mage + med + mht_new + mpregwt + inc + smoke, data = smoking, family = binomial)
summary(model2)

rawresid2 <- residuals(model2,"resp")

#model2 with normal mht
model2_mht <- glm(premature ~ parity + mrace + mage + med + mht + mpregwt + inc + smoke, data = smoking, family = binomial)
summary(model2_mht)

rawresid2 <- residuals(model2,"resp")



##binned residual plots for continuous variables
binnedplot(x=fitted(model2),y=rawresid2,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks good

#mage
binnedplot(x=smoking$mage,y=rawresid2,xlab="Mother's age",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks good

#mpregwt
binnedplot(x=smoking$mpregwt,y=rawresid2,xlab="Mother's pre-pregnancy weight",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks good

#parity
binnedplot(x=smoking$parity,y=rawresid2,xlab="Parity",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#not enough data


#roc
roc(smoking$premature,fitted(model2_mht),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
#AUC = 67.5% - small improvement

#let's do the same for parity - DID NOT WORK
smoking$p_new <- rep(0,nrow(smoking))
smoking$p_new[smoking$parity <= 1] <- 1
smoking$p_new[smoking$parity > 1 & smoking$parity <= 3] <- 2
smoking$p_new[smoking$parity > 3 & smoking$parity <= 4] <- 3
smoking$p_new[smoking$parity > 4 & smoking$parity <= 6] <- 4
smoking$p_new[smoking$parity>6] <- 5
smoking[,'p_new']<-factor(smoking[,'p_new'])

model3 <- glm(premature ~ p_new + mrace + mage + med + mht_new + mpregwt + inc + smoke, data = smoking, family = binomial)
summary(model3)

rawresid3 <- residuals(model3,"resp")

##binned residual plots for continuous variables
binnedplot(x=fitted(model3),y=rawresid3,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks good

#roc
roc(smoking$premature,fitted(model2),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
#AUC = 67.5% - almost the same but makes the binned residuals overall plot look worse so will stick to model2

####Interactions
#Now let's see interactions with the smoke variable
# parity vs premature by smoke
ggplot(smoking,aes(x=premature, y=parity, fill=premature)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Parity vs premature baby, by smoke",
       x="Had premature baby?",y="Parity") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ smoke)
#median is different

# mage vs premature by smoke
ggplot(smoking,aes(x=premature, y=mage, fill=premature)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Mother's age vs premature baby, by smoke",
       x="Had premature baby?",y="Mother's age") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ smoke)
#some interaction might be there

# mht vs premature by smoke
ggplot(smoking,aes(x=premature, y=mht, fill=premature)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Mother's height vs premature baby, by smoke",
       x="Had premature baby?",y="Mother's height") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ smoke)
#some interaction might be there

# mpregwt vs premature by smoke
ggplot(smoking,aes(x=premature, y=mpregwt, fill=premature)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Mother's pre-pregnancy weight vs premature baby, by smoke",
       x="Had premature baby?",y="Mother's pre-pregnancy weight") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ smoke)
#some interaction might be there

# mrace vs premature by smoke - we want to check cos asked in question
# med vs premature by inc - we want to check cos intuitively this seems like interaction and vif for med was high

#mrace and smoke
model_inter1 <- glm(premature ~ inc + med  + smoke * mrace + mht + parity + mpregwt + mage, data = smoking, family = binomial)
summary(model_inter1)
anova(model_inter1, model2_mht, test= "Chisq")
#interaction between mrace and smoke is not significant

#med and inc - WARNING
model_inter1 <- glm(premature ~ inc * med  + smoke + mrace + mht + parity + mpregwt + mage, data = smoking, family = binomial)
summary(model_inter1)
anova(model_inter1, model2_mht, test= "Chisq")

#parity and smoke
model_inter1 <- glm(premature ~ inc + med  + smoke * parity + mht + mrace + mpregwt + mage, data = smoking, family = binomial)
summary(model_inter1)
anova(model_inter1, model2_mht, test= "Chisq")
#interaction between parity and smoke is not significant

#mht_new and smoke
model_inter1 <- glm(premature ~ inc + med  + smoke * mht + parity + mrace + mpregwt + mage, data = smoking, family = binomial)
summary(model_inter1)
anova(model_inter1, model2_mht, test= "Chisq")
#interaction between mht_new and smoke is significant at 0.1

#mage and smoke
model_inter1 <- glm(premature ~ inc + med  + smoke * mage + parity + mrace + mpregwt + mht, data = smoking, family = binomial)
summary(model_inter1)
anova(model_inter1, model2_mht, test= "Chisq")
#interaction between mage and smoke is not significant

#mpregwt and smoke
model_inter1 <- glm(premature ~ inc + med  + smoke * mpregwt + parity + mrace + mage + mht, data = smoking, family = binomial)
summary(model_inter1)
anova(model_inter1, model2_mht, test= "Chisq")
#interaction between mpregwt and smoke is not significant

#inc and smoke
model_inter1 <- glm(premature ~ mpregwt + med  + smoke * inc + parity + mrace + mage + mht, data = smoking, family = binomial)
summary(model_inter1)
anova(model_inter1, model2_mht, test= "Chisq")
#interaction between inc and smoke is not significant

#med and smoke
model_inter_final <- glm(premature ~ mpregwt + inc  + smoke * med + parity + mrace + mage + mht, data = smoking, family = binomial)
summary(model_inter_final)
anova(model_inter_final, model2_mht, test= "Chisq")
vif(model_inter_final)
#interaction between med and smoke is significant
#after including this interaction the vif is also in control for all variables so this is our final model for now

#med * smoke and med * inc - WARNING
model_inter1 <- glm(premature ~ mpregwt + med * (inc  + smoke) + parity + mrace + mage + mht_new, data = smoking, family = binomial)
summary(model_inter1)
anova(model_inter1, model2, test= "Chisq")
#interaction between med and smoke is significant

#med * smoke and mht * smoke
model_inter1 <- glm(premature ~ mpregwt + smoke * (med  + mht) + parity + mrace + mage + inc, data = smoking, family = binomial)
summary(model_inter1)
anova(model_inter1, model2_mht, test= "Chisq")
#interaction is significant

#but now let's check that given the interaction between med and smoke is the interaction between mht and smoke significant?
model2_check <- glm(premature ~ mpregwt + smoke * med  + mht + parity + mrace + mage + inc, data = smoking, family = binomial)
model_inter2 <- glm(premature ~ mpregwt + smoke * (med  + mht) + parity + mrace + mage + inc, data = smoking, family = binomial)
summary(model_inter1)
anova(model_inter2, model2_check, test= "Chisq")
#interaction is significant at 0.1 so we will not include it

#let's use the stepwise function to do model selection (using BIC)
n <- nrow(smoking)
null_model <- glm(premature ~ smoke + mrace,data=smoking,family=binomial)
model_step <- step(null_model,scope=formula(model_inter_final),direction="both",
     trace=0,k = log(n))
model_step$call

#let's use the stepwise function to do model selection (using AIC)
n <- nrow(smoking)
null_model <- glm(premature ~ smoke + mrace,data=smoking,family=binomial)
model_step_AIC <- step(null_model,scope=formula(model_inter_final),direction="both",
                   trace=0)
                   #,k = log(n))
model_step_AIC$call
#glm(formula = premature ~ smoke + mrace + med + mpregwt + smoke:med, 
#family = binomial, data = smoking)

model_final <- glm(formula = premature ~ smoke + mrace + med + mpregwt + smoke:med, 
                   family = binomial, data = smoking)
summary(model_final)

#model assessment for final model
#save the raw residuals
rawresid_final <- residuals(model_final,"resp")

##binned residual plots for continuous variables
binnedplot(x=fitted(model_final),y=rawresid_final,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


###### Model validation
#let's do the confusion matrix with .5 threshold
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model_final) >= 0.5, "1","0")),
                            as.factor(smoking$premature),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate
#we're not able to identify premature birth in mothers but we are identifying non premature births well - maybe because we don't have enough data
#for premature births?

#first, let's repeat with the marginal percentage in the data
mean(smoking$prem_inter)
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model_final) >= mean(smoking$prem_inter), "1","0")),
                            as.factor(smoking$premature),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
#this looks more balanced 

#look at ROC curve
roc(smoking$premature,fitted(model_final),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
#AUC = 67.4%

#why interaction b/w these vars is giving a warning
table(smoking[,c("med","inc")])









