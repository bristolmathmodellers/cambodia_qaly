setwd("C:/Users/Owner/Documents/QALY")
load("qalyall.RData")
mydata <- qalyall
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
install.packages(gplots)
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
lmer(thai_utility~point+(1|pat_id), data=mydata, REML=FALSE) #Basic model, poor fit
lmer1 <- lmer(thai_utility~point+stage+dem_sex+dem_age+cured+(1|pat_id), data=mydata, REML=FALSE)
coef1 <- data.frame(coef(summary(lmer1))) #Obtaining p values by using the normal approximation
coef1$p.z <- 2 * (1 - pnorm(abs(coef1$t.value)))
coef1
install.packages("pbkrtest")
library(pbkrtest)
df.KR1 <- get_ddf_Lb(lmer1, fixef(lmer1)) #Getting the Kenward Roger-approximated df
coef1$p.KR <- 2 * (1 - pt(abs(coef1$t.value), df.KR1)) #Getting p-values from the t-distribution using the t-values and approximated df
coef1
lmer2 <- lmer(thai_utility~point+stage+point*stage+model+dem_sex+dem_age+dem_age*stage+cured+(1|pat_id), data=mydata, REML=FALSE)
coef2 <- data.frame(coef(summary(lmer2))) #Obtaining p values by using the normal approximation
coef2$p.z <- 2 * (1 - pnorm(abs(coef2$t.value)))
coef2
df.KR2 <- get_ddf_Lb(lmer2, fixef(lmer2)) #Getting the Kenward Roger-approximated df
coef2$p.KR <- 2 * (1 - pt(abs(coef2$t.value), df.KR2)) #Getting p-values from the t-distribution using the t-values and approximated df
coef2
qqnorm(residuals(lmer2))
qqline(residuals(lmer2))
nullmer <- thai_utility ~ (1 | pat_id)
anova(nullmer, lmer2) # Can't compare models, not sure why

#Does F stage grouping matter?
br1 <- betareg(betas ~ point + stage, data=mydata)
mydata$new <- as.factor(mydata$stage)
levels(mydata$new) <- c("mild", "mild", "moderate", "moderate", "cirrhosis", "post", "post")
br2 <- betareg(betas ~ point + new, data = mydata)
mydata$new2 <- as.factor(mydata$stage)
levels(mydata$new2) <- c("preC", "preC", "preC", "postC", "postC", "postC")
br3 <- betareg(betas ~ point + new2, data = mydata)
summary(br1)
summary(br2)
summary(br3)
library(lmtest)
lrtest(br1, br2, br3)
