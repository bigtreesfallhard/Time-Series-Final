library(lubridate)
library(tseries)
library(forecast)
#library(DataCombine)
library(bsts)
library(xts) #do not bring in dplyr last

setwd("~/Google Drive/My Drive/School/UChicago/Spring 2023/Time Series/Final")
getwd()

df <- read.csv('online_sales.csv')

# lookthrough
summary(df) # 9586 rows
#Date, Spend, Sales, Advertised.SKU.Sales, Other.SKU.sales


length(df) # 15 columns

#impressions is going to be the biggest impact to sales
# question is how can we bring efficiency into play
# clicks could also be affected since this is online retail
# Does volume matter? that seems like a time series in of itself
#then we need to talk profit margin
#can time series be used 


head(df$Date) #Oct 17, 2021
head(df$Spend) #$0.00
head(df$Sales) #$0.00
head(df$Advertised.SKU.Sales) #$0.00
head(df$Other.SKU.sales) #$0.00

df$Date <- as.Date(df$Date, format = '%b %d, %Y')
head(df$Date) # date

# remove dollar sign and convert to numeric
df$Spend <- as.numeric(gsub(",", "", gsub("\\$", "", df$Spend))) # doesnt catch the big values in the 1000s, need to fix, this is fixed
head(df$Spend)

# remove dollar sign and convert to numeric
df$Sales <- as.numeric(gsub(",", "", gsub("\\$", "", df$Sales))) # doesnt catch the big values in the 1000s, need to fix, this is fixed
head(df$Sales)

# remove dollar sign and convert to numeric
df$Advertised.SKU.Sales <- as.numeric(gsub(",", "", gsub("\\$", "", df$Advertised.SKU.Sales))) # doesnt catch the big values in the 1000s, need to fix, this is fixed
head(df$Advertised.SKU.Sales)

# remove dollar sign and convert to numeric
df$Other.SKU.sales <- as.numeric(gsub(",", "", gsub("\\$", "", df$Other.SKU.sales))) # doesnt catch the big values in the 1000s, need to fix, this is fixed
head(df$Other.SKU.sales)



# EDA on time series
# predict Sales


summary(df$Date) # 36 nas, it will need to drop. Min 10/17/2021 - 1/11/2022

df_omit <- na.omit(df)

summary(df_omit$Date) #10/17/2021 - 1/11/2022

#df_omit[,6:15]
#df_omit[,1]

df_omit1 <- aggregate(df_omit[,6:15], by = list(Day = df_omit$Date), FUN = sum)
summary(df_omit1)


# Create a sequence of dates
#dates <- seq(as.Date("2021-10-17"), as.Date("2022-01-11"), by = "week")

# Convert the time series object to an xts object
sales_xts <- xts(df_omit1[,-1], order.by = df_omit1$Day)
sales_xts

#daily_sales_data <- apply.daily(sales_xts, FUN = sum)

plot(sales_xts$Impressions) # 87

#weekly_sales_xts <- apply.weekly(sales_xts, FUN = sum)
#weekly_sales_xts

#plot(weekly_sales_xts) # 14


plot(sales_xts$Sales) # mean seems time invariant, not sure about seasonality, no trend, could be cyclical(weekend sales) looks pretty random, there is a big spike and a big drop but that can be black friday
acf(sales_xts$Sales) # very close to stationary
pacf(sales_xts$Sales) # very close to stationary


adf.test(sales_xts$Sales) # reject null, this is stationary

kpss.test(sales_xts$Sales) # reject null, this is stationary


plot(sales_xts$Impressions) # this looks stationary to me
acf(sales_xts$Impressions) # very close to stationary, maybe needs differencing
pacf(sales_xts$Impressions) # very close to stationary


adf.test(sales_xts$Impressions) # reject null, this is stationary, but just barely

kpss.test(sales_xts$Impressions) # reject null, this is stationary


#stl(sales_xts$Sales)
sales_arima <- auto.arima(sales_xts$Sales)
summary(sales_arima) #(2,0,0)

summary(auto.arima(sales_train$Impressions)) #(1,0,0)

#test train split 80/20
train_start_date <- as.Date("2021-10-17")
train_end_date <- as.Date("2021-12-23")
test_start_date <- as.Date("2021-12-24")
test_end_date <- as.Date("2022-01-11")


# Split into train and test sets
sales_train <- sales_xts[train_start_date <= index(sales_xts) & index(sales_xts) <= train_end_date]
sales_test <- sales_xts[test_start_date <= index(sales_xts) & index(sales_xts) <= test_end_date]

length(sales_train$Sales) # 68
length(sales_test$Sales) # 19

sales_train_arima <- auto.arima(sales_train$Sales)
summary(sales_train_arima) #(1,0,0)

#predict/forecast
sales_arima_forecast <- forecast(sales_train_arima, h = 19)
print(sales_arima_forecast) # 
plot(sales_arima_forecast)# little better than naive, makes sense

# model metrics
sales_arima_predicted <- sales_arima_forecast$mean
sales_arima_predicted <- as.numeric(sales_arima_predicted)



sales_arima_rmse <- sqrt(mean((sales_arima_predicted - sales_test$Sales)^2))
sales_arima_mape <- mean(abs((sales_test$Sales - sales_arima_predicted) / sales_test$Sales)) * 100
print(paste("RMSE: ", sales_arima_rmse))
print(paste("MAPE: ", sales_arima_mape, "%"))

#residual analysis
acf(sales_arima_forecast$residuals, na.action=na.pass) # not bad
pacf(sales_arima_forecast$residuals, na.action=na.pass) # shows this is close
Box.test(sales_arima_forecast$residuals, type = "Ljung-Box") #fail to reject null, residuals are independent
hist(sales_arima_forecast$residuals) # pretty normal, all in all solid model


# cant do seasonality
#HW1 <- HoltWinters(sales_train$Sales)
# Custom HoltWinters fitting
#HW2 <- HoltWinters(sales_train$Sales, alpha=0.2, beta=0.1, gamma=0.1)
#Visually evaluate the fits
#plot(dfts, ylab="Sales")
#lines(HW1$fitted[,1], lty=2, col="blue")
#lines(HW2$fitted[,1], lty=2, col="red")



sales_train_arima_dyanmic <- Arima(sales_train$Sales, order = c(1,0,0), xreg= sales_train$Impressions)
summary(sales_train_arima_dyanmic) #(1,0,0)


# predict/forecast
forecast_result_dynamic <- forecast(sales_train_arima_dyanmic, h = 19, xreg= sales_test$Impressions)
print(forecast_result_dynamic)
plot(forecast_result_dynamic) # much better with external regressor

forecast_result_dynamic_predicted <- as.numeric(forecast_result_dynamic$mean)


# model metrics
sales_dy_rmse <- sqrt(mean((forecast_result_dynamic_predicted - sales_test$Sales)^2))
sales_dy_mape <- mean(abs((sales_test$Sales - forecast_result_dynamic_predicted) / sales_test$Sales)) * 100
print(paste("RMSE: ", sales_dy_rmse))
print(paste("MAPE: ", sales_dy_mape, "%"))

#residual analysis
acf(forecast_result_dynamic$residuals) # not bad
pacf(forecast_result_dynamic$residuals) # stationary here
Box.test(forecast_result_dynamic$residuals, type = "Ljung-Box") #fail to reject null, residuals are independent
hist(forecast_result_dynamic$residuals) # not 100% normal, has some outliers but good enough


# Define the maximum lag to test
max_lag <- 30

# Initialize variables to store the best model and the smallest error
best_model_dy<- NULL
smallest_error <- Inf
impr_lag <- NULL


# Loop over possible lags
for (lag1 in 1:max_lag) {
  fit2_train1 <- sales_train
  # Create lagged predictors
  fit2_train1$Impressions <- lag(fit2_train1$Impressions, lag1)

      
  # Drop NA values that were created by lagging
  fit2_train1 <- na.omit(fit2_train1)
      
  # Create a state specification with a dynamic regression
  ss <- list()
  ss <- AddDynamicRegression(ss, Sales ~ Impressions, data=fit2_train1)
      
  # Fit the model
  bsts.model <- bsts(Sales ~ Impressions, state.specification = ss, data = fit2_train1, niter = 500, seed=1748)
      
  # Predict on the training set itself (to get the error for the current lags)
  prediction <- predict(bsts.model, newdata = fit2_train1)
      
  # Compute the mean squared error of the predictions
  error <- mean((prediction$mean - fit2_train1$Sales)^2)
      
  # If this model has the smallest error so far, update the best model and smallest error
  if (error < smallest_error) {
    best_model_dy <- bsts.model
    smallest_error <- error
    impr_lag <- lag1
  }
}

impr_lag # 1


# The best_model variable now holds the model with the optimal lags
# Predict on the testing set
prediction_test_sales <- predict(best_model_dy, horizon=nrow(sales_test), burn = SuggestBurn(0.1, best_model_dy), newdata = sales_test)

# Plot
plot(prediction_test_sales)

plot(best_model_dy, "coefficients")

forecast_result_bsts_predicted <- prediction_test_sales$mean 
bsts_residual <- forecast_result_bsts_predicted - sales_test$Sales

# model metrics
sales_bsts_rmse <- sqrt(mean((forecast_result_bsts_predicted - sales_test$Sales)^2))
sales_bsts_mape <- mean(abs((sales_test$Sales - forecast_result_bsts_predicted) / sales_test$Sales)) * 100
print(paste("RMSE: ", sales_bsts_rmse))
print(paste("MAPE: ", sales_bsts_mape, "%"))

#residual analysis
acf(bsts_residual) # not bad
pacf(bsts_residual) # stationary here
Box.test(bsts_residual, type = "Ljung-Box") #fail to reject null, residuals are independent
hist(bsts_residual) # very good


#plot(sales_test$Sales)
#points(sales_arima_forecast, col="red", pch="*")
#lines(sales_arima_forecast, col="red",lty=2)
