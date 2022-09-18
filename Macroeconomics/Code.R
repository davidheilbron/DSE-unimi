library(dplyr)
library(tidyverse)
library(readxl)
library(stats)
library(ggplot2)
library(tseries)
library(TSstudio)
library(robustbase)
library(fpp2)
library(aTSA)
library(vars)
library(lmtest)
library(forecast)
library(urca)

setwd("C:/Users/Helmuth/OneDrive/Studies/Graduate/University of Milan/M.Sc. Data Science & Economics/1st Year/Advanced Macroeconomics/Project/Scripts")

colombia<-read_excel("Database.xlsx", col_names=T) #read database

#Set time series variables
nominal_r<-ts(colombia$nominal_i, start=c(2010, 1), end=c(2021, 4), frequency=4) #Nominal interest rate
pi<-ts(colombia$inflation, start=c(2010, 1), end=c(2021, 4), frequency=4) #Inflation
real_gdp<-ts(colombia$real_gdp, start=c(2010, 1), end=c(2021, 4), frequency=4) #Real GDP
ln_real_gdp<-log(real_gdp) #Log of Real GDP

#Compute GDP gap: estimate trend using OLS and use residuals as GDP gap
trend<-lm(ln_real_gdp~c(1:48))

gdp_gap<-ts(trend$residuals*100,
            start=start(real_gdp), 
            end=end(real_gdp), 
            frequency=frequency(real_gdp)) #GDP gap

autoplot(gdp_gap)

#STRATEGY
#First, we'll forecast inflation using 4 different models:
#1) Random Walk (RW)
#2) ARMA(1,1)
#3) ARIMA(2,1,0)
#4) ARIMA chosen by R (autoarima)
#5) VAR using inflation and GDP output
#6) VAR using inflation, interest rate and GDP output

#For models 1) to 5) the forecast will be of 4 periods ahead (full 2022)
#For model 6), forecast will be for Q1 2022.

#Then we'll estimate Taylor Rule coefficients and the nominal interest rate 
#for 2022:1 given this model (assumption=Central Bank implements interest rate equal to Taylor Rule)

#We'll iterate this process for the whole year, until we have inflation and interest rate for 2022:4

#Lastly, we'll compare the forecasts. Specially 5) and 6), to determine how much inflation reacts to interest rate
#and to determine if there is a possibility that inflation is pushed down to the 3% target

########################################################################3
#INFLATION DESCRIPTION

autoplot(pi)+ggtitle("Inflation rate - quarterly average, Colombia")+labs(x="Time", y="Inflation")
ggAcf(pi)+ggtitle("ACF of inflation") #Exponential decay, suggest an AR component
ggPacf(pi)+ggtitle("PACF of inflation") #Strong 1 and 2 lag. Suggest an AR component up to 2 lags

#Tests for stationarity - ADF and PP

adf.test(pi) #Sample is not stationary - Case 2 (drift and no trend)
pp.test(pi) #Sample is not stationary - Case 2 (drift and no trend)

#First differences to check if process is I(1)
dpi<-diff(pi)
adf.test(dpi) #Series is now stationary in first differences - Case 1 (no drift no trend)
pp.test(dpi) #Series is now stationary in first differences - Case 1 (no drift no trend)



##########################
#Model 1) Random Walk (RW)
##########################

#Fit Model
f_rw<-arima(pi, order=c(0,1,0))
checkresiduals(f_rw) #Persistence in the first lag. LB test confirms that errors are autocorrelated

#Out of sample forecast
forecast_1<-forecast::forecast(pi, model=f_rw, h=4)
plot_forecast(forecast_1)
summary(forecast_1)



#####################
#Model 2) ARMA (1,1)
#####################

#Fit model
f_arma11<-arima(pi, order=c(1,0,1))
autoplot(f_arma11) #Check roots. All inside the unit circle
checkresiduals(f_arma11) #No significant lags, but LB test rejects H0 as in model no.1

#Out of sample forecast
forecast_2<-forecast::forecast(pi, model=f_arma11, h=4)
plot_forecast(forecast_2)
summary(forecast_2)



###############################
#Model 3) SARIMA (2,1,0)(2,0,0)
###############################

#Fit model
f_sarima210<-forecast::Arima(pi, order=c(2,1,0), seasonal=c(2,0,0), include.constant=T)
autoplot(f_sarima210) #AR roots inside unit circle
checkresiduals(f_sarima210) #Ljung Box test - H0: the model does not exhibit a lack of fit - Not rejected

#Out of sample forecast
forecast_3<-forecast::forecast(pi, model=f_sarima210, h=4)
plot_forecast(forecast_3)
summary(forecast_3)



###########################
#Model 4) ARIMA chosen by R
###########################

#Fit model
f_auto<-auto.arima(pi, seasonal=T, allowdrift = T, approximation=F, stepwise=F, trace=T)
autoplot(f_auto) #AR roots inside unit circle
checkresiduals(f_auto) #Ljung Box test - H0: the model does not exhibit a lack of fit - Not rejected

#Out of sample forecast
forecast_4<-forecast::forecast(pi, model=f_auto, h=4)
plot_forecast(forecast_4)
summary(forecast_4)



######################################
#Model 5) VAR(2) (inflation, GDP gap)
######################################

#Lag selection
VAR_5<-na.omit(cbind(pi, gdp_gap))
VARselect(VAR_5, lag.max=10)
#4 lags as suggested by SC Criterion. This is an upper bound, we can still use less lags and reduce the variance

#Fit model
Model_5<-vars::VAR(VAR_5, p=2, type="const", season=4, exogen=NULL)
summary(Model_5) #all roots lie inside the unit circle, model is stable
acf(residuals(Model_5))
serial.test(Model_5, lags.pt=12, type="PT.asymptotic") #H0: there is no serial autocorrelation in the errors. We fail to reject HO

#Granger Causality tests
causality(Model_5, cause="gdp_gap")$Granger #H0: GDP gap do not Granger-cause inflation, we do not reject H0
causality(Model_5, cause="pi")$Granger #H0 Inflation do not Granger-cause gdp_gap, we do not reject H0

#Impulse Response Functions
pi_irf<-irf(Model_5, impulse="gdp_gap", response="pi", n.ahead=20, boot=T)
plot(pi_irf)

#Out of sample forecast
fcast_5<-predict(Model_5, n.ahead=4, ci=0.95)
forecast_5<-fcast_5[["fcst"]][["pi"]][,1]#2022 inflation forecast



#################################################
#Model 6) VAR(2) (inflation, GDP gap, interest rate)
#################################################

#Lag selection
VAR_6<-na.omit(cbind(pi, gdp_gap, nominal_r))
VARselect(VAR_6, lag.max=10)
#10 lags as suggested by SC Criterion. These are way too many lags, we'll stick to 2 lags again

#Fit model
Model_6<-vars::VAR(VAR_6, p=2, type="const", season=4, exogen=NULL)
summary(Model_6) #all roots lie inside the unit circle, model is stable
acf(residuals(Model_6)) #still some autocorrelation between inflation and the interest rate
serial.test(Model_6, lags.pt=12, type="PT.asymptotic") #H0: there is no serial autocorrelation in the errors. We fail to reject HO

#Granger Causality tests
causality(Model_6, cause="gdp_gap")$Granger #H0: GDP gap do not Granger-cause inflation, we do not reject H0
causality(Model_6, cause="pi")$Granger #H0 Inflation do not Granger-cause gdp_gap, we do not reject H0

#Impulse Response Functions
plot(irf(Model_6, impulse="gdp_gap", response="pi", n.ahead=20, boot=T))
plot(irf(Model_6, impulse="nominal_r", response="pi", n.ahead=20, boot=T))

#Out of sample forecast
fcast_6<-predict(Model_6, n.ahead=1, ci=0.95)
fcast6_Q1<-fcast_6[["fcst"]][["pi"]][,1] #2022:1 inflation forecast





#######################
#TAYLOR RULE ESTIMATION
#######################

#Now, we'll estimate the Taylor Rule coefficients for Colombia 2010 - 2021
taylor_col<-lm(nominal_r~pi+gdp_gap) #Backward Looking Taylor Rule using OLS
summary(taylor_col) #Intercept R* and inflation are significant. GDP gap is not significant
#Inflation coefficient below 1 suggests a not-stabilizing reaction of Central Bank to inflation deviations

#Let's store the coefficients to use them later
R<-taylor_col[["coefficients"]][["(Intercept)"]] #Intercept
delta<-taylor_col[["coefficients"]][["pi"]] #Inflation gap coefficient
gamma<-taylor_col[["coefficients"]][["gdp_gap"]] #GDP gap coefficient

Trule_col=R+delta*pi+gamma*gdp_gap #Backward Looking Taylor Rule for the Colombian case

TRplot<-cbind(Trule_col, nominal_r)

autoplot(TRplot)+ggtitle("Colombia's nominal interest rate and Taylor Rule")+
  labs(x="Time",
       y="(%)")

#NOTE: here, we computed the backward looking Taylor Rule that will be used to estimate
#forward looking interest rates. This is not the proper procedure, as the true coefficients
#should have been computed using inflation expectations at t periods before.



######################
#Expected GDP GAP 2022
######################

#To compute a Forward Looking Taylor Rule, we must compute first the expected GDP gap

#Let's compute GDP gap series including 2022 growth rates
#According to BBVA research, real GDP will grow 4.8, 7,8, 2,7 and 1,2, respectively at each quarter of 2022
gdp_growth_2022 <-c(4.8, 7.8, 2.7, 1.2)

#Estimate real GDP 2022
real_gdp_2022<-ts((1+(gdp_growth_2022/100))*real_gdp[45:48], 
                  start=c(2022,1), 
                  end=c(2022,4), 
                  frequency=4)

#Overwrite real GDP series
real_gdp<-ts(c(real_gdp, real_gdp_2022), 
             start=start(real_gdp), 
             end=end(real_gdp_2022), 
             frequency=frequency(real_gdp))

ln_real_gdp<-log(real_gdp) #re take logs
trend<-lm(ln_real_gdp~c(1:52)) #re compute trend
gdp_gap<-ts(trend$residuals*100,
            start=start(real_gdp), 
            end=end(real_gdp), 
            frequency=frequency(real_gdp)) #re compute GDP gap including 2022 growth forecast

plot(gdp_gap)
E_gdp_gap<-gdp_gap[49:52]


#Now, we can proceed to compute the inflation gap for each estimated model
pi_target<-ts(c(3, 3, 3, 3),
              start=c(2022, 1), 
              end=c(2022, 4),
              frequency=4) #Assumption: Inflation target remains the same through 2022 at 3%


forward_taylor_1=R+gamma*E_gdp_gap+delta*forecast_1$mean #Forward Looking Taylor Rule for forecast 1)
forward_taylor_2=R+gamma*E_gdp_gap+delta*forecast_2$mean #Forward Looking Taylor Rule for forecast 2)
forward_taylor_3=R+gamma*E_gdp_gap+delta*forecast_3$mean #Forward Looking Taylor Rule for forecast 3)
forward_taylor_4=R+gamma*E_gdp_gap+delta*forecast_4$mean #Forward Looking Taylor Rule for forecast 4)
forward_taylor_5=R+gamma*E_gdp_gap+delta*forecast_5 #Forward Looking Taylor Rule for forecast 5)



#For forecast 6), we'll estimate the Taylor Rule for 2022:1, then incorporate its result as the nominal_r
#of 2022 Q1 and forecast inflation 2022_2, then compute Taylor Rule Q2 2022 and so on...

forward_taylor_6_Q1=R+gamma*E_gdp_gap[1]+delta*fcast6_Q1 #TR for Q1_2022 is 5.8 according to Model 6)

#Inflation forecast 2022:2
pi_TR<-ts(c(pi, fcast6_Q1), start=start(pi), end=c(2022, 1), frequency=frequency(pi))
i_TR<-ts(c(nominal_r, forward_taylor_6_Q1), start=start(nominal_r), end=c(2022, 1), frequency=frequency(nominal_r))
gdp_gap_TR<-ts(gdp_gap[1:49], start=start(gdp_gap), end=c(2022, 1), frequency=frequency(gdp_gap))

VAR_TR<-na.omit(cbind(pi_TR, gdp_gap_TR, i_TR))
Model_6_TR<-vars::VAR(VAR_TR, p=2, type="const", season=4, exogen=NULL)

forecast_6_Q2<-predict(Model_6_TR, n.ahead=1, ci=0.95)
fcast6_Q2<-forecast_6_Q2[["fcst"]][["pi_TR"]][,1] #Q2 2022 inflation forecast: 6.31

#Nominal interest rate 2022:2
forward_taylor_6_Q2=R+gamma*E_gdp_gap[2]+delta*fcast6_Q2 #TR for Q2_2022 is 5.9 according to Model 6)



#Inflation forecast 2022:3
pi_TR_2<-ts(c(pi_TR, fcast6_Q2), start=start(pi), end=c(2022, 2), frequency=frequency(pi))
i_TR_2<-ts(c(i_TR, forward_taylor_6_Q2), start=start(nominal_r), end=c(2022, 2), frequency=frequency(nominal_r))
gdp_gap_TR<-ts(gdp_gap[1:50], start=start(gdp_gap), end=c(2022, 2), frequency=frequency(gdp_gap))

VAR_TR<-na.omit(cbind(pi_TR_2, gdp_gap_TR, i_TR_2))
Model_6_TR_2<-vars::VAR(VAR_TR, p=2, type="const", season=4, exogen=NULL)

forecast_6_Q3<-predict(Model_6_TR_2, n.ahead=1, ci=0.95)
fcast6_Q3<-forecast_6_Q3[["fcst"]][["pi_TR_2"]][,1] #Q2 2022 inflation forecast: 5.99


#Nominal interest rate 2022:3
forward_taylor_6_Q3=R+gamma*E_gdp_gap[3]+delta*fcast6_Q3 #TR for Q2_2022 is 5.78 according to Model 6)



#Inflation forecast 2022:4
pi_TR_3<-ts(c(pi_TR_2, fcast6_Q3), start=start(pi), end=c(2022, 3), frequency=frequency(pi))
i_TR_3<-ts(c(i_TR_2, forward_taylor_6_Q3), start=start(nominal_r), end=c(2022, 3), frequency=frequency(nominal_r))
gdp_gap_TR<-ts(gdp_gap[1:51], start=start(gdp_gap), end=c(2022, 3), frequency=frequency(gdp_gap))

VAR_TR<-na.omit(cbind(pi_TR_3, gdp_gap_TR, i_TR_3))
Model_6_TR_3<-vars::VAR(VAR_TR, p=2, type="const", season=4, exogen=NULL)

forecast_6_Q4<-predict(Model_6_TR_3, n.ahead=1, ci=0.95)
fcast6_Q4<-forecast_6_Q4[["fcst"]][["pi_TR_3"]][,1] #Q2 2022 inflation forecast: 5.54

#Nominal interest rate 2022:3
forward_taylor_6_Q4=R+gamma*E_gdp_gap[4]+delta*fcast6_Q4 #TR for Q2_2022 is 5.58 according to Model 6)


#Store results
forecast_6<-c(fcast6_Q1, fcast6_Q2, fcast6_Q3, fcast6_Q4) #Group forecasts of model 6) into fcast6 object
forward_taylor_6<-ts(c(forward_taylor_6_Q1, forward_taylor_6_Q2, forward_taylor_6_Q3, forward_taylor_6_Q4),
                     start=c(2022,1),
                     end=c(2022,4),
                     frequency=4)


cbind(forecast_6, forward_taylor_6)

#Model 6) takes into account the nominal interest rate, and it forecasts inflation to be higher
#than the other models. But also, as it endogenizes the monetary policy rate, inflation
#is clearly pushed downwards after the Central Bank starts to increase it. 
#Central Bank increase of the nominal interest rate is not very aggresive, given by the coefficient
#on the Taylor Rule below 1, but it is somehow effective.

#It is not probable that the interest rate is set that high during the first semester of 2022
#as of 29 March 2022, the nominal interest rate is at 4%. Next decision will be taken on 31st March 2022
#Inflation in January was at 6.94% and 8.01% in February 2022.




################
#COMPARE RESULTS
################

#Compare forecasts
final_forecasts<-cbind(forecast_1$mean, forecast_2$mean, forecast_3$mean, forecast_4$mean, forecast_5, forecast_6)
round(final_forecasts, digits=2)

autoplot(final_forecasts)


#Compare Taylor Rule estimations
final_taylor_rules<-cbind(
  forward_taylor_1, 
  forward_taylor_2, 
  forward_taylor_3, 
  forward_taylor_4, 
  forward_taylor_5,
  forward_taylor_6)

round(final_taylor_rules, digits=2)
autoplot(final_taylor_rules)


#Inflation forecast, GDP gap and nominal interest rate for each model
round(cbind(forecast_1$mean, gdp_gap[49:52], forward_taylor_1), digits=2) #Model 1)
round(cbind(forecast_2$mean, gdp_gap[49:52], forward_taylor_2), digits=2) #Model 2)
round(cbind(forecast_3$mean, gdp_gap[49:52], forward_taylor_3), digits=2) #Model 3)
round(cbind(forecast_4$mean, gdp_gap[49:52], forward_taylor_4), digits=2) #Model 4)
round(cbind(forecast_5, gdp_gap[49:52], forward_taylor_5), digits=2) #Model 5)
round(cbind(forecast_6, gdp_gap[49:52], forward_taylor_6), digits=2) #Model 6)

#Finally, we can see that inflation does not seem to get any close to the 3% target during 2022

