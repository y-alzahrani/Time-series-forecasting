## ----setup, include=FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)




## ----warning=FALSE---------------------------------------------------------------------------
data(AirPassengers) # Loads the data

# Puts the data in a dataframe with two columns; ds for time and y for the number of passengers
AirPassengers.df = data.frame(ds=zoo::as.yearmon(time(AirPassengers)), y=AirPassengers)

# Initial values for the parameters a and b are specified and the function iterates through values of a and b
# until it converges on the values that provide the best fit.
exponential_model = nls(AirPassengers ~ a * exp(b * time(AirPassengers)), data = AirPassengers.df,
                        start=list(a = 100, b = 0))

plot(AirPassengers, main="Monthly totals of international airline passengers from 1949 to 1960", xlab = "Year", ylab ="Number of passengers (in thousands)"); points(time(AirPassengers), fitted(exponential_model), type="l", col="red"); legend("topleft", legend="Fitted exponential model", col="red", lty=1)


## ----message=FALSE---------------------------------------------------------------------------
AirPassengers.df = data.frame(ds=zoo::as.yearmon(time(AirPassengers)), y=AirPassengers)

seasonality_OFF_model = prophet::prophet(AirPassengers.df, yearly.seasonality=FALSE)

future_dates = prophet::make_future_dataframe(seasonality_OFF_model, periods=60, freq="month")

forecast = predict(seasonality_OFF_model, future_dates)

plot(seasonality_OFF_model, forecast, main="Prophet forecast with disabled seasonality", xlabel="Year", ylabel="Number of passengers (in thousands)", xaxt="n") + ggplot2::scale_y_continuous(breaks = seq(0, 1000, by=100))


seasonality_ON_model = prophet::prophet(AirPassengers.df, yearly.seasonality=TRUE)

future_dates = prophet::make_future_dataframe(seasonality_ON_model, periods=60, freq="month")

forecast = predict(seasonality_ON_model, future_dates)

plot(seasonality_ON_model, forecast, main="Prophet forecast with enabled seasonality", xlabel="Year", ylabel="Number of passengers (in thousands)", xaxt="n") + ggplot2::scale_y_continuous(breaks = seq(0, 1000, by=100))


## ----message=FALSE---------------------------------------------------------------------------
multiplicative_model = prophet::prophet(AirPassengers.df, yearly.seasonality=TRUE, seasonality.mode = "multiplicative")

future_dates = prophet::make_future_dataframe(multiplicative_model, periods=60, freq="month")

forecast = predict(multiplicative_model, future_dates)

plot(multiplicative_model, forecast, main="Prophet forecast with multiplicative seasonality", xlabel="Year", ylabel="Number of passengers (in thousands)", xaxt="n") + ggplot2::scale_y_continuous(breaks = seq(0, 1000, by=100))


## ----message=FALSE---------------------------------------------------------------------------
# Creates a sequence of dates at 1 year intervals within the specified time periods.
summer_holiday_dates = seq(as.Date("1949-07-01"), as.Date("1960-08-31"), by="year")
winter_holiday_dates = seq(as.Date("1949-12-01"), as.Date("1961-01-31"), by="year")

# A dataframe of the sequences of dates is created because the Prophet model can only work with dataframes.
holidays_df = data.frame(holiday=c(rep("Summer Holiday", length(summer_holiday_dates)),
                                   rep("Winter Holiday", length(winter_holiday_dates))),
                         ds=c(summer_holiday_dates, winter_holiday_dates))

model = prophet::prophet(AirPassengers.df, yearly.seasonality=TRUE, seasonality.mode = "multiplicative", holidays = holidays_df)

future_dates = prophet::make_future_dataframe(model, periods=60, freq="month")

forecast = predict(model, future_dates)

plot(model, forecast, main="Prophet forecast with holiday dates included in the model", xlabel="Year",
     ylabel="Number of passengers (in thousands)", xaxt="n") + ggplot2::scale_y_continuous(breaks = seq(0, 1000, by=100))


## ----message=FALSE---------------------------------------------------------------------------
AirPassengers.df = data.frame(ds=zoo::as.Date(time(AirPassengers)), y=AirPassengers)

# Creates a new dataframe which contains data just from the last 5 years only.
AirPassengers_df_filtered = AirPassengers.df[AirPassengers.df$ds >= as.Date("1955-01-01"),]

model = prophet::prophet(AirPassengers_df_filtered, yearly.seasonality=TRUE, seasonality.mode = "multiplicative")

future_dates = prophet::make_future_dataframe(model, periods=60, freq="month")

forecast = predict(model, future_dates)

plot(model, forecast, main="Prophet forecast using data from the last 5 years", xlabel="Year", ylabel="Number of passengers (in thousands)", xaxt="n") + ggplot2::scale_y_continuous(breaks=seq(0, 1000, by=100)) + ggplot2::scale_x_datetime(date_breaks="1 year", date_labels="%Y")


## --------------------------------------------------------------------------------------------
data("AirPassengers")
plot(decompose(AirPassengers))


## --------------------------------------------------------------------------------------------
Holt_Winters_model = HoltWinters(AirPassengers, seasonal="multiplicative")

Holt_Winters_forecast = predict(Holt_Winters_model, 60)

plot(Holt_Winters_model, Holt_Winters_forecast, main="Holt-Winters forecasting", xlab="Year", ylab="Number of passengers (in thousands)"); legend("topleft", legend=c("Holt-Winters forecast", "Data"), col=c("red", "black"), lty=1)


## --------------------------------------------------------------------------------------------
data("AirPassengers")
plot(decompose(log(AirPassengers)))


## --------------------------------------------------------------------------------------------
Holt_Winters_log_model = HoltWinters(log(AirPassengers))

Holt_Winters_log_forecast = predict(Holt_Winters_log_model, 60)

linear_model = lm(log(AirPassengers) ~ time(AirPassengers))

plot(Holt_Winters_log_model, Holt_Winters_log_forecast, main="Holt-Winters forecasting using the logarithm", xlab="Year", ylab="log of no. of passengers"); points(time(AirPassengers), fitted(linear_model), type="l", col="blue"); legend("topleft", legend=c("Holt-Winters forecast", "Fitted linear model", "Data"), col=c("red", "blue", "black"), lty=1)

