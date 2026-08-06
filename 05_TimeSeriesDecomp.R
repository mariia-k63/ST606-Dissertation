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


# --- STL Decomposition ---
maxtp_decomp <- maxtp %>% model(STL(MaxTemp ~ trend(window = 11) + season(window = "periodic"),
                                    robust = TRUE)) %>% components() %>% autoplot()
# ggsave("Plots/maxtp_decomp_STL.png", plot = maxtp_decomp, width = 10, height = 6, dpi = 300)

rain_decomp <- rain %>% model(STL(Rain ~ trend(window = 11) + season(window = "periodic"),
                                  robust = T)) %>% components() %>% autoplot()
# ggsave("Plots/rain_decomp_STL.png", plot = rain_decomp, width = 10, height = 6, dpi = 300)

