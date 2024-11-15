library(dplyr)
library(ggplot2)
library(olsrr)
library(lubridate)
library(forecast)
library(PerformanceAnalytics)
#Chapter 8 Homework and Prompts
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

#Problem 1:The authors of the reservoir greenhouse gas study recommend using the following transformation for CO2 data: 1 / (CO2 + 1000)
#Use the transformation and design a regression analysis to present to water managers 
#about the impact of reservoir characteristics on carbon dioxide fluxes. In designing your regression, you should consider the environmental conditions 
#that impact carbon dioxide fluxes, the availability of data, and the assumptions of ordinary least squares regression. 
#Create a regression table including a R2 and the sample size with paragraph summary of the findings that can be presented to water managers.
# multiple regression
# creates a model object
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg) 
summary(mod.full)

#Checking assumptions
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

# qq plot normality
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



#Chapter 9 - (Both) Homework and (In-Class Prompts)
ETdat <- read.csv("ETdata.csv")

unique(ETdat$crop)

# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+ geom_line()+ labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition of subset t's: y = Trend + (seasonality * error)
plot(almond_dec)

acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)

pacf.plot <- pacf(na.omit(almond_ts))
#ARIMA = Autorregressive Integrated Moving Average, building coefficients into the AR model.
#This specifies the function's simplest form and returns standard errors for 
#coefficients and provide model fit statistics, cutting NAs 
almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1
#Uses a higher order AR for correlation
model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4
#looking at fitted values to see matched observations, calculates fitted values 
#by subtracting residuals from data
# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

#Forecasts data and gives uncertainty interval. Looks at AR4 model forecast:
newAlmond <- forecast(model4)
newAlmond

#Formats data and set up a data frame to plot it.
#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)
# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#Problem 2: Decompose the evapotranspiration time series for almonds, pistachios, fallow/idle fields, corn, and table grapes. 
#Evaluate differences in the observations, trends, and seasonality of the data between the different crops. 
#Write a summary of your evaluation for a water manager that is interested in examining how irrigation can affect evapotranspiration. 
#The manager also wants to understand what crops have the greatest water consumption, the timing of high water consumption, and if there are changes over time. 
#Include plots of your decomposition.
pistachio <- ETdat %>% # ET data
  filter(crop == "Pistachios") %>% # only use pistachio fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(pistachio, aes(x=ymd(date),y=ET.in))+
  geom_point()+ geom_line()+ labs(x="year", y="Monthy evapotranspiration (in)")

# pistachio ET time series
pistachio_ts <- ts(pistachio$ET.in, 
                start = c(2016,1),
                frequency= 12) 
pistachio_dec <- decompose(pistachio_ts)
plot(pistachio_dec)
#-------------------------------------------------------------------------------
fallow_idle_lands <- ETdat %>% # ET data
  filter(crop == "Fallow/Idle Cropland") %>% # only use fallow/idle croplands 
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(fallow_idle_lands, aes(x=ymd(date),y=ET.in))+
  geom_point()+ geom_line()+ labs(x="year", y="Monthy evapotranspiration (in)")

# fallow/idle croplands ET time series
fallow_idle_lands_ts <- ts(fallow_idle_lands$ET.in, 
                start = c(2016,1),
                frequency= 12) 
fallow_idle_lands_dec <- decompose(fallow_idle_lands_ts)
plot(fallow_idle_lands_dec)

#-------------------------------------------------------------------------------
corn <- ETdat %>% # ET data
  filter(crop == "Corn") %>% # only use corn fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(corn, aes(x=ymd(date),y=ET.in))+
  geom_point()+ geom_line()+ labs(x="year", y="Monthy evapotranspiration (in)")

# corn ET time series
corn_ts <- ts(corn$ET.in, 
                   start = c(2016,1),
                   frequency= 12) 
corn_dec <- decompose(corn_ts)
plot(corn_dec)

#-------------------------------------------------------------------------------
table_grapes <- ETdat %>% # ET data
  filter(crop == "Grapes (Table/Raisin)") %>% # only use grapes fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(table_grapes, aes(x=ymd(date),y=ET.in))+
  geom_point()+ geom_line()+ labs(x="year", y="Monthy evapotranspiration (in)")

# corn ET time series
table_grapes_ts <- ts(table_grapes$ET.in, 
                   start = c(2016,1),
                   frequency= 12) 
table_grapes_dec <- decompose(table_grapes_ts)
plot(table_grapes_dec)

#Problem 3:Design an autoregressive model for pistachios and fallow/idle fields. Forecast future evapotranspiration 
#for each field so that water managers can include estimates in their planning. Make a plot that includes historical and forecasted evapotranspiration for the crops 
#to present to the water manager. Include a brief explanation of your autoregressive models.

#pistachio:
#----------
pistachio_y <- na.omit(pistachio_ts)
pistachio_model <- arima(pistachio_y, order = c(1,0,0)) 
pistachio_model

pistachio_model_2 <- arima(pistachio_y, order = c(4,0,0)) 
pistachio_model_2

# calculate fit
pistachio_fit1 <- pistachio_y - residuals(pistachio_model) 
pistachio_fit2 <- pistachio_y - residuals(pistachio_model_2)
#plot data
plot(pistachio_y)
# plot fit
points(pistachio_fit1, type = "l", col = "darkgreen", lty = 2, lwd=2)
points(pistachio_fit2, type = "l", col = "magenta4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "darkgreen","magenta4"),
       bty="n")

newPistachio <- forecast(pistachio_model_2)
newPistachio

newPistachioF <- data.frame(newPistachio)

years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistachioF$dateF <- ymd(paste(years,"/",month,"/",1))

ggplot() + geom_line(data = pistachio, aes(x = ymd(date), y = ET.in)) +
xlim(ymd(pistachio$date[1]),newPistachioF$dateF[24]) +  
geom_line(data = newPistachioF, aes(x = dateF, y = Point.Forecast), col="brown3") + 
geom_ribbon(data=newPistachioF, aes(x=dateF,ymin=Lo.95, ymax=Hi.95), 
fill=rgb(0.5,0.5,0.5,0.5)) + theme_classic()+ labs(x="year", y="Evapotranspiration (in)")

#fallow/idle fields
#------------------
fallow_idle_lands_y <- na.omit(fallow_idle_lands_ts)
fallow_idle_lands_model <- arima(fallow_idle_lands_y, order = c(1,0,0)) 
fallow_idle_lands_model

fallow_idle_lands_model_2 <- arima(fallow_idle_lands_y, order = c(4,0,0)) 
fallow_idle_lands_model_2

# calculate fit
fallow_idle_lands_fit1 <- fallow_idle_lands_y - residuals(fallow_idle_lands_model) 
fallow_idle_lands_fit2 <- fallow_idle_lands_y - residuals(fallow_idle_lands_model_2)
#plot data
plot(fallow_idle_lands_y)
# plot fit
points(fallow_idle_lands_fit1, type = "l", col = "orange3", lty = 2, lwd=2)
points(fallow_idle_lands_fit2, type = "l", col = "turquoise3", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "darkgreen","magenta4"),
       bty="n")

new_Fallow_Idle_Lands <- forecast(fallow_idle_lands_model_2)
new_Fallow_Idle_Lands

new_Fallow_Idle_Lands_F <- data.frame(new_Fallow_Idle_Lands)

years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
new_Fallow_Idle_Lands_F$dateF <- ymd(paste(years,"/",month,"/",1))

ggplot() + geom_line(data = fallow_idle_lands, aes(x = ymd(date), y = ET.in)) +
  xlim(ymd(fallow_idle_lands$date[1]),new_Fallow_Idle_Lands_F$dateF[24]) +  
  geom_line(data = new_Fallow_Idle_Lands_F, aes(x = dateF, y = Point.Forecast), col="darkorange") + 
  geom_ribbon(data=new_Fallow_Idle_Lands_F, aes(x=dateF,ymin=Lo.95, ymax=Hi.95), 
              fill=rgb(0.5,0.5,0.5,0.5)) + theme_classic()+ labs(x="year", y="Evapotranspiration (in)")


