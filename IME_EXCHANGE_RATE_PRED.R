#Packages needed
install.packages("forecast")
install.packages("lubridate")
library(forecast)
library(lubridate)

#Time series 
ts_ex<-ts(IME_DATA_FINAL$USDEUR, start=c(year(min(IME_DATA_FINAL$date)), quarter(min(IME_DATA_FINAL$date))), frequency=4)
ts_ecb<-ts(IME_DATA_FINAL$ECB, start=c(year(min(IME_DATA_FINAL$date)), quarter(min(IME_DATA_FINAL$date))), frequency=4)
ts_fed<-ts(IME_DATA_FINAL$FED, start=c(year(min(IME_DATA_FINAL$date)), quarter(min(IME_DATA_FINAL$date))), frequency=4)

#Model structure 
ts_final<-cbind(ts_fed,ts_ecb)
model5<-Arima(ts_ex,xreg=ts_final,order=c(2,1,0))

#Prediction plugins
future_data<-cbind(rep(0.05,2),rep(0.0375,2))
future_data<-cbind(c(0.053,0.05,0.05,0.0475),c(0.0375,0.035,0.0325,0.0325))

#Forecasts
forecast_results <- forecast(model5, xreg = future_data)
print(forecast_results)
plot(forecast_results)

#Monte Carlo Simulation
n_simulations <- 1000
sim_results <- numeric(n_simulations)
last_ecb_rate <- 0.04
sd_ecb_rate <- 0.015
last_fed_rate <- 0.053
sd_fed_rate <- 0.02
forecast_horizon<-4
for (i in 1:n_simulations) {
  simulated_fed_rates <- rnorm(forecast_horizon, mean = last_fed_rate, sd = sd_fed_rate)
  simulated_ecb_rates <- rnorm(forecast_horizon, mean = last_fed_rate, sd = sd_ecb_rate)
  ts_final <- forecast(model5, xreg = cbind(simulated_ecb_rates, simulated_fed_rates), h = forecast_horizon)
  sim_results[i] <- any(ts_final$mean <= 1)
  }
probability_of_parity <- mean(sim_results)
print(paste("Probability of reaching parity in the next", forecast_horizon ,"quarters is ", probability_of_parity*100,"%")) 

