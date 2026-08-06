# --- Loading necessary libraries ---
library(tidyverse)

# --- Reading in the data, formatting date column appropriately ---
weather <- read_csv("Data/DailyDataFeb26.csv")
weather$date <- as.Date(weather$date, format = "%d/%m/%Y")
head(weather)

# --- Inspecting for missing data ---
colSums(is.na(weather))

# --- Subsetting to predictors of interest ---
w <- weather %>% select(date, maxtp, mintp, gmin, rain, cbl, wdsp, sun, evap)

# --- Adding year and decade columns ---
w <- w %>% mutate(year = year(date)) %>% relocate(year, .before = 2)
w <- w %>% mutate(decade = year - year %% 10) %>% relocate(decade, .before = 3)
head(w)

# --- Adding a 2-decade breaks column ---
w <- w %>% mutate(period = cut(w$year, breaks = seq(1929, 2029, 20), dig.lab=10,
                               labels = c("1940s","1950-60s", "1970-80s", 
                                          "1990-00s", "2010-20s"))) %>% 
  relocate(period, .before = 4)
levels(w$period)



# --- Imputing gmin (Grass Minimum Temperature) with monthly mean ---
w %>% filter(is.na(gmin)==TRUE)

g <- w %>% filter(year(date)==2010 & month(date)==12)

gmin_m <- round(mean(g$gmin, na.rm=TRUE), digits = 2)

idx <- which(is.na(w$gmin)==TRUE)

for (i in idx){
  w[idx,]$gmin <- gmin_m
}

# Overall mean pre-imputation
mean(weather$gmin, na.rm=TRUE)

# Overall mean post-imputation
mean(w$gmin)



# --- Imputing evap (Evaporation) ---
w %>% filter(is.na(evap)==TRUE)

w %>% filter(year(date)==2010 & month(date)==6)
w %>% filter(year(date)==2013 & month(date)==7)  %>% print(n=33)

idx_e <- which(is.na(w$evap)==TRUE)

for (i in idx_e){
  w[i,]$evap <- (w[i-1,]$evap+w[i+1,]$evap)/2
}

# Overall mean pre-imputation
mean(weather$evap, na.rm=TRUE)

# Overall mean post-imputation
mean(w$evap)



# --- Confirming no missing values are left ---
colSums(is.na(w)==TRUE)

# --- Saving the imputed dataset ---
# write.csv(w, "Data/Weather_Imputed.csv", row.names = FALSE)


