# --- Loading necessary libraries ---
library(fpp3)
library(egg)

# --- Reading in data, formatting date column appropriately ---
w <- read.csv("Data/Weather_Imputed.csv")
w$date <- as.Date(w$date)

# --- Summarising maximum air temperature and precipitation amount by monthly means ---
mean_maxtp <- w %>% group_by(year(date), month(date)) %>%
  summarise(maxtp = mean(maxtp))
mean_rain <- w %>% group_by(year(date), month(date)) %>%
  summarise(rain = mean(rain))

# --- Creating time-series and tsibbles ---
maxtp_ts <- ts(mean_maxtp$maxtp, frequency = 12, start=c(1942,1), end=c(2026,2))
maxtp <- as_tsibble(maxtp_ts)
names(maxtp) <- c("Month", "MaxTemp")

rain_ts <- ts(mean_rain$rain, frequency = 12, start=c(1942,1), end=c(2026,2))
rain <- as_tsibble(rain_ts)
names(rain) <- c("Month", "Rain")



# --- Stretched datasets for cross-validation ---

# 1-month step
stretch_1 <- maxtp %>% stretch_tsibble(.init = 72, .step = 1) %>% 
  relocate(.id) %>% filter(.id != max(.id))

# 6-month step
stretch_6 <- maxtp %>% stretch_tsibble(.init = 72, .step = 6) %>% 
  relocate(.id) %>% filter(.id != max(.id))

# 12-month (1 year) step, for 1-year forecast
stretch_12 <-  maxtp %>% stretch_tsibble(.init = 72, .step = 12) %>% 
  relocate(.id) %>% filter(.id != max(.id))

# 1-year step, for 2-year forecast
stretch_12_2 <- stretch_12 %>% filter(.id != max(.id))

# 1-year step, for 3-year forecast
stretch_12_3 <- stretch_12_2 %>% filter(.id != max(.id))



# --- Training accuracy ETS (baseline additive model) ---
training_ets <- maxtp %>% 
  model(ETS(MaxTemp ~ error("A") + trend("A") + season("A"))) %>% accuracy()


# --- ETS stretched - 1 month, 6 month, 12 months, 2 years, 3 years ---
s1_ets <- stretch_1 %>% 
  model(ETS(MaxTemp ~ error("A") + trend("A") + season("A"))) %>% 
  forecast(h=1) %>% accuracy(maxtp)

s6_ets <- stretch_6 %>%
  model(ETS(MaxTemp ~ error("A") + trend("A") + season("A"))) %>% 
  forecast(h=6) %>% accuracy(maxtp)

s12_ets <- stretch_12 %>% 
  model(ETS(MaxTemp ~ error("A") + trend("A") + season("A"))) %>% 
  forecast(h=12) %>% accuracy(maxtp)

s12_ets2 <- stretch_12_2 %>% 
  model(ETS(MaxTemp ~ error("A") + trend("A") + season("A"))) %>% 
  forecast(h=24) %>% accuracy(maxtp)

s12_ets3 <- stretch_12_3 %>% 
  model(ETS(MaxTemp ~ error("A") + trend("A") + season("A"))) %>% 
  forecast(h=36) %>% accuracy(maxtp)


# --- Function for obtaining the results of cross-validation ---
results <- function(train, cv){
  res <- data.frame(rbind(dplyr::select(train, "RMSE", "MAE", "MAPE"), 
                          dplyr::select(cv, "RMSE", "MAE", "MAPE")))
  
  res <- round(res, digits = 3)
  
  colnames(res) <- c("RMSE", "MAE", "MAPE")
  rownames(res) <- c("Training", "CV step = 1", "CV step = 6", 
                     "CV step = 12 (1 year forecast)", 
                     "CV step = 12 (2 year forecast)",
                     "CV step = 12 (3 year forecast)")
  
  return(res)
}

# --- ETS cross-validation results ---
cv_results_ets <- rbind(s1_ets, s6_ets, s12_ets, s12_ets2, s12_ets3)
ets_models <- results(training_ets, cv_results_ets)
ets_models

# write.csv(ets_models, "TS_Results/ets_cv_1.csv", row.names = TRUE)





# --- Baseline SARIMA model --- 
training_arima <- maxtp %>% model(ARIMA(MaxTemp, stepwise = F, 
                                        approximation = F))
training_arima %>% report()
# Obtained model - ARIMA(0,0,3)(2,1,0)[12] 

train_arima <- training_arima %>% accuracy()

# --- SARIMA stretched - 1 month, 6 month, 12 months, 2 years, 3 years ---
s1_arima <- stretch_1 %>% 
  model(ARIMA(MaxTemp ~ pdq(0,0,3) + PDQ(2,1,0))) %>% 
  forecast(h=1) %>% accuracy(maxtp)

s6_arima <- stretch_6 %>% 
  model(ARIMA(MaxTemp ~ pdq(0,0,3) + PDQ(2,1,0))) %>% 
  forecast(h=6) %>% accuracy(maxtp)

s12_arima <- stretch_12 %>% 
  model(ARIMA(MaxTemp ~ pdq(0,0,3) + PDQ(2,1,0))) %>% 
  forecast(h=12) %>% accuracy(maxtp)

s12_arima2 <- stretch_12_2 %>% 
  model(ARIMA(MaxTemp ~ pdq(0,0,3) + PDQ(2,1,0))) %>% 
  forecast(h=24) %>% accuracy(maxtp)

s12_arima3 <- stretch_12_3 %>% 
  model(ARIMA(MaxTemp ~ pdq(0,0,3) + PDQ(2,1,0))) %>% 
  forecast(h=36) %>% accuracy(maxtp)

# --- SARIMA cross-validation results ---
cv_results_arima <- rbind(s1_arima, s6_arima, s12_arima, s12_arima2, s12_arima3)
arima_models <- results(train_arima, cv_results_arima)
arima_models

# write.csv(arima_models, "TS_Results/arima_cv.csv", row.names = TRUE)



# --- Regression with SARIMA errors: MaxTemp ~ Rain  ---

# Tsibble of both maximum air temperature and precipitation amount monthly means
temp_rain <- maxtp %>% mutate(Rain = rain$Rain)

# Baseline linear model with SARIMA errors
arima_lm <- temp_rain %>% model(ARIMA(MaxTemp ~ Rain, stepwise = F,
                                     approximation = F))
arima_lm %>% report()
# Obtained model - LM w/ ARIMA(1,0,0)(2,1,0)[12] errors 

# Training accuracy, whole dataset
arima_lm %>% accuracy()

# Training on 2012-2022 data, testing on 2023-2025
temp_rain_train <- temp_rain %>% filter(year(Month) > 2011 & year(Month) < 2023)
temp_rain_test <- temp_rain %>% filter(year(Month) > 2022 & year(Month) < 2026)

# Training the model on 10 years of data
sarima_lm <- temp_rain_train %>% 
  model(ARIMA(MaxTemp ~ Rain + pdq(1,0,0) + PDQ(2,1,0)))
lm_arima_train_acc <- sarima_lm %>% accuracy() %>% dplyr::select(RMSE, MAE, MAPE)

# Testing on the left out 3 years
lm_arima_test_acc <- forecast(sarima_lm, temp_rain_test) %>% 
  accuracy(temp_rain_test) %>%  dplyr::select(RMSE, MAE, MAPE)

lm_arima_results <- data.frame(rbind(lm_arima_train_acc, lm_arima_test_acc))
lm_arima_results <- round(lm_arima_results, digits = 3)
rownames(lm_arima_results) <- c("Training", "Test")
lm_arima_results

# write.csv(lm_arima_results, "TS_Results/lm_arima.csv", row.names = TRUE)



# --- Baseline performance of the ETS model ---

# Splitting time-series into training and testing
maxtp_train <- maxtp %>% filter(year(Month) > 2011 & year(Month) < 2023)
maxtp_test <- maxtp %>% filter(year(Month) > 2022 & year(Month) < 2026)

# Training the additive ETS model on 10 years of data
ets_aaa <- maxtp_train  %>% 
  model(ETS(MaxTemp ~ error("A") + trend("A") + season("A")))

train_acc_ets <- ets_aaa %>% accuracy() %>% dplyr::select(RMSE, MAE, MAPE)

# Forecasting for 3 years, comparing to test set
test_acc_ets <- ets_aaa %>%  forecast(h = "3 years") %>% accuracy(maxtp_test) %>% 
  dplyr::select(RMSE, MAE, MAPE)

# ETS baseline results
ets_res <-  data.frame(rbind(train_acc_ets, test_acc_ets))
ets_res <- round(ets_res, digits = 3)
rownames(ets_res) <- c("Training", "Test")

# write.csv(ets_res, "TS_Results/ets.csv", row.names = F)


# ETS forecast plot
ets_forecast <- ets_aaa  %>%  forecast(h = "3 years") %>% 
  autoplot(maxtp_train, level = 95) +
  labs(y="Maximum Temperature", title="Monthly Mean Maximum Temperature") +
  autolayer(maxtp_test, colour = "black")

# ggsave("Plots/ETSforecast.png", plot = ets_forecast, width = 10, height = 6, dpi = 300)


# --- Baseline performance of the SARIMA model ---

# Training the SARIMA model on 10 years of data
sarima <- maxtp_train %>% model(ARIMA(MaxTemp ~ pdq(0,0,3) + PDQ(2,1,0))) 
train_acc_sarima <- sarima %>% accuracy() %>% dplyr::select(RMSE, MAE, MAPE)

# Forecasting for 3 years, comparing to test set
test_acc_sarima <- sarima %>%  forecast(h = "3 years") %>% 
  accuracy(maxtp_test) %>% dplyr::select(RMSE, MAE, MAPE)

# SARIMA baseline results
sarima_res <-  data.frame(rbind(train_acc_sarima, test_acc_sarima))
sarima_res <- round(sarima_res, digits = 3)
rownames(sarima_res) <- c("Training", "Test")
sarima_res

# write.csv(sarima_res, "TS_Results/sarima.csv", row.names = F)
