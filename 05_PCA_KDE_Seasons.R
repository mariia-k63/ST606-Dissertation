# --- Loading necessary libraries ---
library(tidyverse)
library(egg) 
library(MASS)
library(rts) 


# --- Reading in data, performing PCA & formatting columns appropriately ---
w <- read_csv("Data/Weather_Imputed.csv")


# --- Adding season column ---
w$season <- c("")

winter <- c(1,2,12)
spring <- c(3,4,5)
summer <- c(6,7,8)
autumn <- c(9,10,11)

idx_winter <- which(month(w$date) %in% winter)
idx_spring <- which(month(w$date) %in% spring)
idx_summer <- which(month(w$date) %in% summer)
idx_autumn <- which(month(w$date) %in% autumn)

for (i in 1:length(w$date)){
  if (month(w[i,]$date) %in% winter){
    w[i,]$season <- "Winter"
  }
  else if (month(w[i,]$date) %in% spring) {
    w[i,]$season <- "Spring" 
  }
  else if (month(w[i,]$date) %in% summer) {
    w[i,]$season <- "Summer" 
  }
  else {
    w[i,]$season <- "Autumn" 
  }
}

w <- w %>% relocate(season, .before = 5)


# --- Principal components analysis ---
pca_result <- prcomp(w[,6:13], scale. = TRUE)
pr_comps <- data.frame(pca_result$x[,1:2])

# Creating a new dataframe with PCA results and 2-decade periods
pr_comps$decade <- w$decade
pr_comps$season <- factor(w$season, levels = c("Winter", "Spring", 
                                               "Summer", "Autumn"))
pr_comps$period <- factor(w$period)
pr_comps_2dec <- filter(pr_comps, decade>=1970)

pr_comps$decade <- factor(w$decade)
pr_comps_2dec$decade <- factor(pr_comps_2dec$decade)


# --- Function for plotting KDE results at different quantiles ---

# By Alan Cameron on Stack Overflow 
# available at: https://stackoverflow.com/questions/75598144/interpretation-of-2d-density-estimate-charts

density_quantiles <- function(x, y, quantiles) {
  dens <- MASS::kde2d(x, y, n = 25)
  df   <- cbind(expand.grid(x = dens$x, y = dens$y), z = c(dens$z))
  r    <- terra::rast(df)
  ind  <- sapply(seq_along(x), function(i) cellFromXY(r, cbind(x[i], y[i])))
  ind  <- ind[order(-r[ind][[1]])]
  vals <- r[ind][[1]]
  ret  <- approx(seq_along(ind)/length(ind), vals, xout = quantiles)$y
  replace(ret, is.na(ret), max(r[]))
}


# --- Plotting KDE results at different quantiles in 2-decade breaks, faceting by season ---
q25 <- ggplot(pr_comps_2dec, aes(x=PC1, y=PC2)) +
    geom_point(alpha=0.3,
               colour = "grey", size = 0.5) +
    geom_density2d(
      aes(colour = period),
      n=25,
      alpha=0.9,
      contour_var = "density",
      linewidth = 0.4,
      breaks = density_quantiles(pr_comps_2dec$PC1, pr_comps_2dec$PC2, c(0, 0.25))) +
    labs(title = "25th Quantile") + 
    xlim(-4.5, 4.5) + ylim(-4.5, 4.5) + facet_wrap(vars(season), nrow=4) +
    theme(legend.position = "none", axis.title.x=element_blank())

q50 <- ggplot(pr_comps_2dec, aes(x=PC1, y=PC2)) +
  geom_point(alpha=0.3,
             colour = "grey", size = 0.5) +
  geom_density2d(
    aes(colour = period),
    n=25,
    alpha=0.9,
    contour_var = "density",
    linewidth = 0.4,
    breaks = density_quantiles(pr_comps_2dec$PC1, pr_comps_2dec$PC2, c(0, 0.5))) +
  labs(title = "50th Quantile") + 
  xlim(-4.5, 4.5) + ylim(-4.5, 4.5) + facet_wrap(vars(season), nrow=4) +
  theme(legend.position = "none", axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

q75 <- ggplot(pr_comps_2dec, aes(x=PC1, y=PC2)) +
  geom_point(alpha=0.3,
             colour = "grey", size = 0.5) +
  geom_density2d(
    aes(colour = period),
    n=25,
    alpha=0.9,
    contour_var = "density",
    linewidth = 0.4,
    breaks = density_quantiles(pr_comps_2dec$PC1, pr_comps_2dec$PC2, c(0, 0.75))) +
  labs(title = "75th Quantile") + 
  xlim(-4.5, 4.5) + ylim(-4.5, 4.5) + facet_wrap(vars(season), nrow=4) +
  theme(axis.title=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


# --- Final plot ---
kde_plot_quart <- ggarrange(q25, q50, q75, ncol=3)
# ggsave("Plots/KDE_Season_Quartiles.jpg", plot = kde_plot_quart, dpi = 300, height = 8, width = 8)




