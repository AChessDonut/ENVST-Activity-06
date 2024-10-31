library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
# read in greenhouse gas data from reservoirs
ghg <- read.csv("Deemer_GHG_Data.csv")

# log transform methane fluxes
ghg$log.ch4 <- log(ghg$ch4+1)

ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

unique(ghg$Region)

##Binary variables##
# binary variable for boreal region
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)

# binary variable for tropical region
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)

# binary variable for alpine region
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)

# binary variable for known hydropower
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)

# multiple regression
# creates a model object
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg) 
#uses the data argument to specify dataframe

summary(mod.full)

#Checking assumptionss
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

# qq plot - normality
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

# shapiro-wilks test -
shapiro.test(res.full)

#residuals
plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

# isolate continuous model variables into data frame - multicolinearity:

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

#Model selection
# run stepwise
full.step <- ols_step_forward_aic(mod.full)
# view table
full.step 

# check full model
full.step$model

# plot AIC over time
plot(full.step )

##Predictions
# prediction with interval for predicting a point
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")

# look at prediction with 95% confidence interval of the mean

predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")
