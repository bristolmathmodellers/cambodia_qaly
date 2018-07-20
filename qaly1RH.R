setwd("C:/Users/Owner/Documents/QALY")
load("qalyall.RData")
library(dplyr)
mydata <- qalyall %>% filter(!is.na(model) & !is.na(stage) & !is.na(cured))

# subset the data to exclude individuals with "NA" for model, stage, or cured.

mydata$pat_id <- as.factor(mydata$pat_id) #changing data types
mydata$stage <- as.factor(mydata$stage)
mydata$dem_age <- as.numeric(mydata$dem_age)
mydata$dem_sex <- as.factor(mydata$dem_sex)
mydata$treated <- as.factor(mydata$treated)
mydata$started_treatment <- as.factor(mydata$started_treatment)
mydata$cured <- as.factor(mydata$cured)
mydata$model <- as.factor(mydata$model)
mydata$point <- as.factor(mydata$point)
hist(mydata$dem_age) #slight older skew
attach(mydata)

#ANOVA
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
ggboxplot(mydata, x = "point", y = "thai_utility", color = "model")
ggline(mydata, x = "point", y = "thai_utility", color = "model")
means<- round(tapply(mydata$thai_utility, mydata$point, mean))
means
install.packages("gplots")
library(gplots)
plotmeans(thai_utility~point, digits=2, ccol=red, mean.labels=T, main="EQ5D means by treatment stage")
boxplot(thai_utility ~ point, main="EQ5d by treatment stage (mean is blue dot)", xlab="Treatment stage", ylab="EQ5D", col=rainbow(7))
points(means, col="blue", pch=18)

install.packages(car)
library(car)
av1 <- aov(thai_utility ~ point, data = mydata)
summary(av1) #Does utility depend on point?
tuk <- TukeyHSD(av1)
tuk # There is a significant difference between all groups
plot(tuk) #Significant differences do not cross the zero value

#ANCOVA
av2 <- aov(thai_utility ~ point + model, data = mydata)
summary(av2) # Does utility depend on point and model?
Anova(av2, type = "III", singular.ok = TRUE)
TukeyHSD(av2, which = "point")
pairwise.t.test(mydata$thai_utility, mydata$point,
                p.adjust.method = "BH")
plot(av2, 1)
plot(av2, 2)
hist(residuals(av2)) #Not normal, right skew

av3 <- aov(thai_utility ~ point + model + stage + dem_age + dem_sex, data = mydata)
summary(av3)
summary.aov(av3)
Anova(av3, type = "III", singular.ok = TRUE)
TukeyHSD(av3, which = "point")
hist(residuals(av3)) #Not normal, right skew

#BETA REGRESSION - needs to be between 0 and 1
mydata$betas <- mydata$thai_utility - 0.001
mydata$betas[mydata$betas <=0] <-NA
library(betareg)
trainingIndex <- c(1:(nrow(mydata)-1))
trainingData <- mydata[trainingIndex, ]
testData <- mydata[-trainingIndex, ]
betaMod <- betareg(betas ~ point + stage, data = trainingData)
summary (betaMod) # model summary
predict (betaMod, testData) # predict on test data

#LINEAR MODEL
library(lme4)
lm1 <- lmer(thai_utility ~ point + model + (1|pat_id) + (1|stage) + (1|dem_age) + (1|dem_sex), data = mydata, REML=FALSE)
summary(lm1)
anova(lm1)
fit <- lm(thai_utility ~ 1, data = mydata)
summary(fit)
logLik(lm1)
logLik(fit)
lm2 <- glm(thai_utility ~ point + model + (1|pat_id) + (1|stage) + (1|dem_age) + (1|dem_sex), data = mydata, REML=FALSE)

#MULTILEVEL MODEL
lmer0 <- lmer(indo_utility~point+(1|pat_id), data=mydata, REML=FALSE) #Basic model, poor fit
lmer1 <- lmer(indo_utility~point+relevel(stage, ref="F0")+(1|pat_id))
lmer2 <- lmer(indo_utility~point+relevel(stage, ref="F0")+cured*point+(1|pat_id))
lmer3 <- lmer(indo_utility~point+relevel(stage, ref="F0")+dem_sex+dem_age+point*cured+(1|pat_id), data=mydata, REML=FALSE)
lmer4 <- lmer(indo_utility~point+relevel(stage, ref="F0")+point*stage+model+dem_sex+dem_age+dem_age*stage+point*cured+(1|pat_id), data=mydata, REML=FALSE)
lmerfixd <- lmer(indo_utility~point+relevel(stage, ref="F0")+dem_age+dem_sex+cured+model+(1|pat_id))
# Changing the base level for 'stage' to F0
# what about if we ignore age and/or sex?

#drop1 on the most complex model
drop1(lmer4)
library(lmerTest)
drop1(lmer4, ddf="Kenward-Roger")
drop1(lmerfixd) #Model with fixed effects only
# which model is preferred by AIC? - Everything but point*stage and cured (see drop1 outputs for each model) 
# p-values for each 
library(pbkrtest)
coef4 <- data.frame(coef(summary(lmer4))) #Obtaining p values by using the normal approximation
coef4$p.z <- 2 * (1 - pnorm(abs(coef4$t.value)))
df.KR4 <- get_ddf_Lb(lmer4, fixef(lmer4)) #Getting the Kenward Roger-approximated df
coef4$p.KR <- 2 * (1 - pt(abs(coef4$t.value), df.KR4)) #Getting p-values from the t-distribution using the t-values and approximated df
coef4

#Final reduced model
lmerfin <- lmer(indo_utility~point+relevel(stage, ref="F0")+model+dem_sex+dem_age+dem_age*stage+(1|pat_id), data=mydata, REML=FALSE)
summary(lmerfin)
coeffin <- data.frame(coef(summary(lmerfin))) #Obtaining p values by using the normal approximation
coeffin$p.z <- 2 * (1 - pnorm(abs(coeffin$t.value)))
df.KRfin <- get_ddf_Lb(lmerfin, fixef(lmerfin)) #Getting the Kenward Roger-approximated df
coeffin$p.KR <- 2 * (1 - pt(abs(coeffin$t.value), df.KRfin)) #Getting p-values from the t-distribution using the t-values and approximated df
coeffin

#Other p values
coef1 <- data.frame(coef(summary(lmer1))) #Obtaining p values by using the normal approximation
coef1$p.z <- 2 * (1 - pnorm(abs(coef1$t.value)))
coef1
install.packages("pbkrtest")
library(pbkrtest)
df.KR1 <- get_ddf_Lb(lmer1, fixef(lmer1)) #Getting the Kenward Roger-approximated df
coef1$p.KR <- 2 * (1 - pt(abs(coef1$t.value), df.KR1)) #Getting p-values from the t-distribution using the t-values and approximated df
coef1
lmer4 <- lmer(indo_utility~point+relevel(stage, ref="F0")+point*stage+model+dem_sex+dem_age+dem_age*stage+point*cured+(1|pat_id), data=mydata, REML=FALSE)
coef2 <- data.frame(coef(summary(lmer2))) #Obtaining p values by using the normal approximation
coef2$p.z <- 2 * (1 - pnorm(abs(coef2$t.value)))
coef2
df.KR2 <- get_ddf_Lb(lmer2, fixef(lmer2)) #Getting the Kenward Roger-approximated df
coef2$p.KR <- 2 * (1 - pt(abs(coef2$t.value), df.KR2)) #Getting p-values from the t-distribution using the t-values and approximated df
coef2
qqnorm(residuals(lmer2))
qqline(residuals(lmer2))
nullmer <- lmer(thai_utility ~ (1 | pat_id), data=mydata, REML=FALSE)
anova(nullmer, lmer2)
drop1(lmer2)


#Does F stage grouping matter?
br1 <- betareg(betas ~ point + stage, data=mydata)
mydata$new <- as.factor(mydata$stage)
levels(mydata$new) <- c("post","mild", "mild", "moderate", "moderate", "cirrhosis", "post")
br2 <- betareg(betas ~ point + new, data = mydata)
mydata$new2 <- as.factor(mydata$stage)
levels(mydata$new2) <- c("postC","preC", "preC", "preC", "preC", "postC", "postC")
br3 <- betareg(betas ~ point + new2, data = mydata)
summary(br1)
AIC(br1)
summary(br2)
AIC(br2)
summary(br3)
AIC(br3) 
library(lmtest)
lrtest(br1, br2, br3)

#SUMMARY

#All models apart from those with point*stage and cured removed gave higher AICs
#(see drop1 outputs for each model),
#meaning that retaining these two variables would not improve the predictive or
#explanatory power of the model.

#Indo utility was significantly higher during treatment compared to
#before treatment (0.03 unit increase), and utility was higher still
#post-treatment (0.05 unit increase).
#Sex 1 had significantly higher utility than sex 0 (0.02 units higher).
#There were no other significant influences on utility.

###
# Get coefficient estimates to use in Markov model
mydata$combo1 <- as.factor(paste(mydata$stage,mydata$point,sep="_"))
lmer5 <- lmer(indo_utility~relevel(combo1, ref="F0_initial")+(1|pat_id), data=mydata, REML=FALSE)
coef5 <- data.frame(coef(summary(lmer5))) #Obtaining p values by using the normal approximation
coef5$p.z <- 2 * (1 - pnorm(abs(coef5$t.value)))
library(pbkrtest)
df.KR5 <- get_ddf_Lb(lmer5, fixef(lmer5)) #Getting the Kenward Roger-approximated df
coef5$p.KR <- 2 * (1 - pt(abs(coef5$t.value), df.KR5)) #Getting p-values from the t-distribution using the t-values and approximated df
coef5

# Getting SE and CIs
library(lsmeans)
lsmeans(lmer5, test.effs=NULL, method.grad='simple', specs='combo1')

#### look at point/cure instead of point*cure
 mydata$pointcured <- mydata$point
 mydata$pointcured[mydata$cured==T & mydata$point=="SVR"] <- "SVR-cured"
 mydata$pointcured[mydata$cured==F & mydata$point=="SVR"] <- "SVR-failed"
 mydata$pointcured[mydata$cured==T & mydata$point=="treatment"] <- "treatment-cured"
 mydata$pointcured[mydata$cured==F & mydata$point=="treatment"] <- "treatment-failed"
# ERROR - "invalid factor level, NA generated"

  # first make a model with this instead of the point*cure interaction and see if any significant effect
 lmeralt <- lmer(indo_utility~point+relevel(stage, ref="F0")+point*stage+model+dem_sex+dem_age+dem_age*stage+pointcured+(1|pat_id), data=mydata, REML=FALSE)
 
 ## are the pKR values adjusted for multiple testing?
 # ANSWER - I would say yes (KR df are calculated as the first part of the process - therefore is this adjustment?)
 # but I am not sure if what I've done is sufficient, would like a second opinion.
 # See https://seriousstats.wordpress.com/tag/kenward-roger-approximation/


