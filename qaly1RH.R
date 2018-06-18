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
library(betareg)
trainingIndex <- c(1:(nrow(mydata)-1))
trainingData <- mydata[trainingIndex, ]
testData <- mydata[-trainingIndex, ]
betaMod <- betareg(thai_utility ~ point + model, data = trainingData)

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
