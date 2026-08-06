# --- Loading necessary libraries ---
library(tidyverse)
library(egg) 
library(MASS)
library(rts) 

# --- Reading in data, performing PCA & formatting columns appropriately ---
w <- read_csv("Data/Weather_Imputed.csv")

pca_result <- prcomp(w[,5:12], scale. = TRUE)
pr_comps <- data.frame(pca_result$x[,1:2])

pr_comps$decade <- w$decade
pr_comps$period <- factor(w$period)
pr_comps_2dec <- filter(pr_comps, decade>=1970)
pr_comps$decade <- factor(w$decade)

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

# --- Heatmap of KDE results at 25th, 75th, 90th and 99th quantiles ---
quant_heatmap <- c(0, 0.25, 0.5, 0.75, 0.9, 0.99)
heatmap_KDE <- ggplot(pr_comps, aes(x=PC1, y=PC2)) +
  geom_point(alpha=0.3,
             colour = "grey", size = 0.5) +
  geom_density2d_filled(
    n=25,
    alpha=0.7,
    aes(fill = after_stat(level)),
    contour_var = "density",
    breaks = density_quantiles(pr_comps$PC1, pr_comps$PC2, quant_heatmap)) +
  coord_equal() +
  scale_fill_viridis_d('Quantiles', labels = scales::percent(quant_heatmap[-1]),
                       direction = -1)

# ggsave("Plots/KDE_Heatmap.jpg", plot = heatmap_KDE, dpi = 300)


# --- Plotting KDE results at different quantiles in 2-decade breaks ---
quant_plot <- function(q, lims=FALSE){
  if (lims==TRUE) {
    ggplot(pr_comps_2dec, aes(x=PC1, y=PC2)) +
      geom_point(alpha=0.3,
                 colour = "grey", size = 0.5) +
      geom_density2d(
        aes(colour = period),
        n=25,
        alpha=0.9,
        contour_var = "density",
        linewidth = 0.4,
        breaks = density_quantiles(pr_comps_2dec$PC1, pr_comps_2dec$PC2, c(0, q/100))) +
      coord_equal() +
      labs(title = paste(as.character(q),"th Quantile", sep="")) + 
      xlim(-3.5, 3.5) + ylim(-3.5,3.5)
  }
  else {
    ggplot(pr_comps_2dec, aes(x=PC1, y=PC2)) +
      geom_point(alpha=0.3,
                 colour = "grey", size = 0.5) +
      geom_density2d(
        aes(colour = period),
        n=25,
        alpha=0.9,
        contour_var = "density",
        linewidth = 0.4,
        breaks = density_quantiles(pr_comps_2dec$PC1, pr_comps_2dec$PC2, c(0, q/100))) +
      coord_equal() +
      labs(title = paste(as.character(q),"th Quantile", sep=""))
  }
}


# Quartiles plot
quart <- c(25,50,75)
for (i in 1:length(quart)){
  name <- paste("q", quart[i], sep = "")
  assign(name, quant_plot(quart[i], lims=TRUE))
}

kde_plot_quart <- ggarrange(q25, q50, q75,nrow=1)
# ggsave("Plots/KDE_Quartiles.jpg", plot = kde_plot_quart, dpi = 300, width = 12)



# Higher quantiles plot
quantiles <- c(80,90,99)
for (i in 1:length(quart)){
  name <- paste("q", quantiles[i], sep = "")
  assign(name, quant_plot(quantiles[i]))
}
kde_plot_hq <-ggarrange(q80, q90, q99,nrow=1)
# ggsave("Plots/KDE_HighQuantiles.jpg", plot = kde_plot_hq, dpi = 300, width = 12)

