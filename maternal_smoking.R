###########################################################################
###########################################################################
################## Maternal Smoking and Birth Weights #####################
###########################################################################
###########################################################################

###### Clear environment and load libraries
rm(list = ls())
library(knitr)
library(ggplot2)
#library(kableExtra)
#library(lattice)
library(dplyr)
library(rms) #for VIF
library(MASS)
library(jtools)


#Loading Data
smoking <- read.csv("smoking.csv")
babies <- read.csv("babiesdata.csv")
summary(smoking)
str(smoking)

#subsetting the data and removing unwanted vars
smoking <- smoking[c(-2,-3)]
summary(smoking)
describe(smoking)

#Collapsing race and education categories for easier analysis
smoking$med[smoking$med == 7] <- 6
smoking$mrace[smoking$mrace == 1 | smoking$mrace == 2 | smoking$mrace == 3 | smoking$mrace == 4 | smoking$mrace == 5] <- 0

#converting vars from num to factor
smoking[,'mrace']<-factor(smoking[,'mrace'])
smoking[,'med']<-factor(smoking[,'med'])
smoking[,'inc']<-factor(smoking[,'inc'])
smoking[,'smoke']<-factor(smoking[,'smoke'])

####### EDA
#Checking if the distribution of the response variable is normal
hist(smoking$bwt.oz,xlab="Baby Weight (Ounces)",main="Distribution of Weight of Babies",col=rainbow(10))

#Exploring the relationship b/w weight and predictors
#mage
ggplot(smoking,aes(x=mage, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Weight vs Mother's Age",x="Mother's Age",y="Weight")
#not linear - maybe we can try converting mage to categorical 
#you can do the median thing

#mpregwt
ggplot(smoking,aes(x=mpregwt, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm", col="red3") + theme_classic() +
  labs(title="Weight vs Mother's Weight",x="Mother's Weight",y="Weight")
#somehwat weakly linear
#you can do the median thing

#parity
ggplot(smoking,aes(x=parity, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm", col="red3") + theme_classic() +
  labs(title="Weight vs Parity",x="Parity",y="Weight")
#not linear

#mht
ggplot(smoking,aes(x=mht, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm", col="red3") + theme_classic() +
  labs(title="Weight vs Mother's Height",x="Mother's Height",y="Weight")
#not linear
#you can do the median thing

#inc
ggplot(smoking,aes(x=inc, y=bwt.oz, fill=inc)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Weight vs Family Income",x="Family Income",y="Weight") + 
  theme_classic() + theme(legend.position="none")
#median is somewhat similar

#mrace
ggplot(smoking,aes(x=mrace, y=bwt.oz, fill=mrace)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Red") +
  labs(title="Weight vs Mother's Race",x="Mother's Race",y="Weight") + 
  theme_classic() + theme(legend.position="none")
#median is different

#med
ggplot(smoking,aes(x=med, y=bwt.oz, fill=med)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Weight vs Mother's Educ",x="Mother's Educ",y="Weight") + 
  theme_classic() + theme(legend.position="none")
#median is similar

#smoke
ggplot(smoking,aes(x=smoke, y=bwt.oz, fill=smoke)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Weight vs Smoke",x="Smoke",y="Weight") + 
  theme_classic() + theme(legend.position="none")
#median is different

####### Let's explore interactions
#Weight with mage by smoke
ggplot(smoking,aes(x=mage, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Weight vs Mother's Age by Smoke",x="Mother's Age",y="Weight") +
  facet_wrap( ~ smoke,ncol=4)
#no interaction

#Weight with parity by smoke
ggplot(smoking,aes(x=parity, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Weight vs Parity by Smoke",x="Parity",y="Weight") +
  facet_wrap( ~ smoke,ncol=4)
#no interaction

#Weight with mht by smoke
ggplot(smoking,aes(x=mht, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Weight vs Mother's Height by Smoke",x="Mother's Height",y="Weight") +
  facet_wrap( ~ smoke,ncol=4)
#no interaction

#Weight with mpregwt by smoke
ggplot(smoking,aes(x=mpregwt, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Weight vs Mother's Weight by Smoke",x="Mother's Weight",y="Weight") +
  facet_wrap( ~ smoke,ncol=4)
#no interaction

####### Now interaction b/w categorical variables
#Weight with mrace by smoke
ggplot(smoking,aes(x=mrace, y=bwt.oz, fill=mrace)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Weight vs Race by Smoke",x="Race",y="Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke,ncol=4)
#there might be some interaction 

#Weight with med by smoke
ggplot(smoking,aes(x=med, y=bwt.oz, fill=med)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Weight vs Mother's Educ by Smoke",x="Mother's Educ",y="Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke,ncol=4)
#trend is mostly the same with some differences

#Weight with inc by smoke
ggplot(smoking,aes(x=inc, y=bwt.oz, fill=inc)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Weight vs Family's Income by Smoke",x="Family's Income",y="Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke,ncol=4)
#trend is mostly the same with some differences

#TAKEAWAYS:
#We see no evidence of non-normality
#We also notice that linearity is a problem with all the continuous vars as they behave like categorical vars
#We see that the median for race and smoke is different suggesting they might be significant variables
#We might also consider interactions between `Race` and `Smoke`

###### Modeling and Model Assessment
#Based on our EDA we will include all the variables and the interaction b/w mrace and smoke

#First, a MLR model on weight with only main effects
Model1 <- lm(bwt.oz~parity+mrace+mage+med+mht+mpregwt+inc+smoke,data=smoking)
summary(Model1)

#Let's do some model assessment with this before adding the interaction term
#Linearity 
ggplot(smoking,aes(x=mpregwt, y=Model1$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() 
#Linearity is clearly not being satisfied for parity, mage, mht because they're behaving like categorical variables
#mpregwt is somewhat satisfying the linearity assumption

#Normality
plot(Model1,which=2,col=c("blue4"))
#normality is being reasonably satisfied aligned with what we saw during EDA

#variance and independence
plot(Model1,which=1,col=c("blue4"))
#both assumptions are being reasonably satisfied

#cooks distance, leverage, standardized residuals
plot(Model1,which=5,col=c("blue4"))
#no outliers, leverage or influential points

#Lets fit the model to all the vars + interaction b/w smoke and race
Model1_inter1 <- lm(bwt.oz~parity+mage+med+mht+mpregwt+inc+smoke*mrace,data=smoking)
summary(Model1_inter1)

#F-test to understand if our interaction is significant
anova(Model1,Model1_inter1)
#interaction is not significant

#check multicollinearity with vif
vif(Model1)
#med has very high vif which is problematic

#Let's remove med and see if it improves our model
Model2 <- lm(bwt.oz~parity+mrace+mage+mht+mpregwt+inc+smoke,data=smoking)
summary(Model2)
#doesn't improve the model

#Let's check for multicollinearity between med and inc as they intuitively seem to be correlated
Model1_inter2 <- lm(bwt.oz~parity+mage+med*inc+mht+mpregwt+smoke+mrace,data=smoking)
summary(Model1_inter2)

#F-test to understand if our interaction is significant
anova(Model1,Model1_inter2)
#interaction is significant and we might want to remove one of these variables

#stepwise for all other interactions
NullModel <- lm(bwt.oz~smoke+mrace,data=smoking)
FullModel <- lm(bwt.oz~smoke*mrace + smoke*med + smoke*inc + 
                  med*inc + parity + mage + mht + mpregwt,
                data=smoking)
n <- nrow(smoking)
Model_step <- step(NullModel, scope = formula(FullModel),direction="both",trace=0, k = log(n))
Model_step$call
#lm(formula = bwt.oz ~ smoke + mrace + mpregwt + mht, data = smoking)

#making the final model
final_model <- lm(bwt.oz~smoke + mrace + mpregwt + mht, data = smoking)
summary(final_model)

#let's check model assumptions of the final model
#Normality
plot(final_model,which=2,col=c("blue4"))
#normality is being reasonably satisfied aligned with what we saw during EDA

#variance and independence
plot(final_model,which=1,col=c("blue4"))
#both assumptions are being reasonably satisfied

#cooks distance, leverage, standardized residuals
plot(final_model,which=5,col=c("blue4"))
#few outliers - how do we extract outliers from this graph?

#confidence intervals
confint(final_model,level = 0.95)

#centering the vars to interpret the intercept
summ(final_model, center = TRUE)

#plot for interaction between income and education
ggplot(smoking,aes(x=med, y=bwt.oz, fill=med)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Set1") +
  labs(title="Weight vs Mother's Educ by Income",x="Mother's Educ",y="Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ inc,ncol=4)

#image 1 (histogram of mrace)
counts <- table(smoking$smoke, smoking$mrace)
barplot(counts, main="Distribution of Mothers' Race by Smoking Status",
        xlab="Race", col=c("lightblue","yellow"),
        legend = rownames(counts))



















