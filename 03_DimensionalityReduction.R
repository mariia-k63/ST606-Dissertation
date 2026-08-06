# --- Loading necessary libraries ---
library(tidyverse)
library(ggbiplot)
library(Rtsne)
library(umap)
library(egg)
set.seed(123)


# --- Reading in data, formatting columns appropriately ---
w <- read_csv("Data/Weather_Imputed.csv")
w$year <- as.integer(w$year)
w$decade <- factor(w$decade)
w$period <- factor(w$period)


# --- Principal components analysis ---
pca_result <- prcomp(w[,5:12], scale. = TRUE)
summary(pca_result)

# Biplots with loading vectors & ellipses
biplot_a <- ggbiplot(pca_result, groups = w$decade, 
                     alpha = 0.5,
                     varname.size = 2,
                     varname.adjust = 1.5,
                     varname.color = "#009",
                     point.size = 0.5) + xlim(-5,5)

biplot_b <- ggbiplot(pca_result, groups = w$decade, ellipse = TRUE, 
                     alpha = 0.05,
                     ellipse.alpha = 0,
                     ellipse.linewidth = 0.3,
                     varname.size = 2,
                     varname.adjust = 1.5,
                     varname.color = "#009",
                     point.size = 0.5) + xlim(-5,5)

pca_plots <- ggarrange(biplot_a, biplot_b, nrow=1)
# ggsave("Plots/PCA_plots.jpg", plot = pca_plots, width = 10, height = 6, dpi = 300)


# Close-up of the biplot
pca_closer <- ggbiplot(pca_result, groups = w$decade, ellipse = TRUE, 
                       alpha = 0.1,
                       ellipse.alpha = 0,
                       ellipse.linewidth = 0.4,
                       varname.size = 4,
                       varname.adjust = 1.5,
                       varname.color = "#009",
                       point.size = 0.5) + xlim(-2.5,2.5) + ylim(-2.5,2.5)

# ggsave("Plots/PCA_closer.png", plot = pca_closer, width = 6, height = 6, dpi = 300)



# --- t-SNE ---
# 500 neighbours & 1000 iterations (default iterations number, scaled data)
tsne_results <- Rtsne(w[, 5:12], pca_scale = TRUE, perplexity = 500, max_iter = 1000)

# Storing results in a dataframe, creating a scatterplot
tsne_df <- data.frame(X = tsne_results$Y[, 1],
                      Y = tsne_results$Y[, 2],
                      Decade = w$decade)

tsne_plot <- tsne_df %>% ggplot(aes(x=X, y=Y, colour = Decade)) + 
  geom_point(alpha = 0.4) + labs(title = "t-SNE") 

# ggsave("Plots/tSNE_plot.png", plot = tsne_plot, width = 7.5, height = 6, dpi = 300)



# --- UMAP ---
# 50 neighbours & 200 iterations (default iterations number, scaled data)
umap_results <- umap(scale(w[,5:12]), n_neighbors=50)

# Storing results in a dataframe, creating a scatterplot
umap_df <- data.frame(x = umap_results$layout[,1],
                      y = umap_results$layout[,2],
                      Decade = w$decade)

umap_plot <- umap_df %>% ggplot(aes(x=x, y=y, colour = Decade)) + 
  geom_point(alpha = 0.5) + labs(title = "UMAP") 

# ggsave("Plots/UMAP_plot.png", plot = umap_plot, width = 7.5, height = 6, dpi = 300)


tsne_umap <- ggarrange(tsne_plot, umap_plot, nrow=1)

# ggsave("Plots/tsne_umap.jpg", plot = tsne_umap, width = 15, height = 6, dpi = 300)

