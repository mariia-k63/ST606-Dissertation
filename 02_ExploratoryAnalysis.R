# --- Loading necessary libraries ---
library(tidyverse)
library(GGally)

# --- Reading in data, formatting columns appropriately ---
w <- read_csv("Data/Weather_Imputed.csv")
w$year <- as.integer(w$year)
w$decade <- factor(w$decade)
w$period <- factor(w$period)

# --- Inspecting data, confirming all missing values were imputed ---
glimpse(w)
colSums(is.na(w))


# --- Scatterplot matrix ---
pairs_plot <- ggpairs(w, columns = 5:12) 

# ggsave("Plots/Pairs.png", plot = pairs_plot, width = 10, height = 6, dpi = 300)


# --- Minimum vs maximum air temperatures scatterplot ---
temp <- w %>% ggplot(aes(x=mintp, y=maxtp, colour = decade)) + 
  geom_point(size = 1, alpha = 0.5) + 
  scale_color_discrete(name = "Decade") +
  labs(title = "Maximum vs minimum air temperature")

# ggsave("Plots/MinMaxTemp.png", plot = temp, width = 6, height = 5, dpi = 300)


# --- Sunshine duration vs precipitation scatterplot ---
sun_rain <- w %>% ggplot(aes(x=rain,y=sun, colour = decade)) + 
  geom_point(size = 1, alpha = 0.5) + 
  scale_color_discrete(name = "Decade") +
  labs(title = "Sunshine duration vs precipitation amount")

# ggsave("Plots/SunRain.png", plot = sun_rain, width = 6, height = 5, dpi = 300)

