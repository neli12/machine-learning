## Load libraries
library(dplyr)
library(kableExtra)
library(PerformanceAnalytics)
library(corrplot)
library(ggplot2)
library(reshape2)
library(psych)
library(tibble)
library(plotly)
library(factoextra)
library(ggrepel)
library(raster)
library(tmap)
library(rgdal)

## Set working directory and list files
setwd("C:/Users/neliq/Documents/MBA_monografia/tables")
list.files()

## Load file
soil <- read.csv('solos_pca.csv', sep = ";")
soil %>%
  kable() %>%
  kable_styling(bootstrap_options = 'striped',
                 full_width = T, 
                 font_size = 12)

## Exclude standard deviation coluns
soil_mean <- soil[,c(1:3, 5, 7, 9, 11, 13)]
soil_mean %>%
  kable() %>%
  kable_styling(bootstrap_options = 'striped',
                full_width = T, 
                font_size = 12)

## Chart correlation between variables
chart.Correlation(soil_mean[, 3:8], histogram = TRUE, pch = "+")
corrplot.mixed(cor(soil_mean[,3:8]), lower = 'number', upper = 'circle')

## Save the correlation matrix
corr_soil <- cor(soil_mean[,3:8])

## Bartlett's test
cortest.bartlett(R = corr_soil)

## Running PCA
# First scale and rearrange colums to rownames, exclude IBGE codes (CD_MUN)
soil_scaled <- soil_mean %>% 
  select(-CD_MUN) %>% 
  column_to_rownames("NM_MUN") %>% 
  scale() %>% 
  data.frame()

# Run PCA
soil_pca <- prcomp(soil_scaled)
summary(soil_pca)   # Explained variance in the second row


# Summary - Reorder important variables 
summary_pca <- data.frame(eigenvalues = soil_pca$sdev ^ 2,
                          variance_proportion = summary(soil_pca)$importance[2,],
                          cumulative_proportion = summary(soil_pca)$importance[3,])

summary_pca

# Visualizing the weight of each variable on the explained variance. But first, reshape the dataframe
ggplotly(
  data.frame(soil_pca$rotation) %>%
    mutate(var = names(soil_mean[3:8])) %>%
    melt(id.vars = "var") %>%
    mutate(var = factor(var)) %>%
    ggplot(aes(x = var, y = value, fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~variable) +
    labs(x = NULL, y = NULL, fill = "Soil variables:") +
    scale_fill_brewer() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
)

# Plot the scree plot - To see the variance explained by each PC
ggplotly(
  fviz_eig(X = soil_pca,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "orange",
           linecolor = "darkgoldenrod4")
)

## Extracting factor loadings
k <- sum((soil_pca$sdev ^ 2) > 1)  # Select those that have loadings higher than 1
factor_loadings <- soil_pca$rotation[, 1:k] %*% diag(soil_pca$sdev[1:k])
colnames(factor_loadings) <- c("F1", 'F2')

# Visualyzing communalities
data.frame(rowSums(factor_loadings ^ 2)) %>%
  rename(comunalidades = 1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Plotting factor loadings
data.frame(factor_loadings) %>%
  ggplot(aes(x = F1, y = F2)) +
  geom_point(color = "darkred", size = 3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_text_repel(label = row.names(factor_loadings)) +
  labs(x = "F1",
       y = "F2") +
  theme_bw()

## Extracting factorial scores
factor_scores <- t(soil_pca$rotation)/soil_pca$sdev 
colnames(factor_scores) <- colnames(soil_scaled)

factor_scores

## Building rankings
# Select just the PC1
score_PC1 <- factor_scores[1,]
score_PC1

# Apply to each county
county_F1 <- t(apply(soil_scaled, 1, function(x) x * score_PC1))
county_F1 <- data.frame(county_F1) %>%
               mutate(F1 = rowSums(.) * -1)

# Transfer to the original column
soil_mean["F1"] <- county_F1$F1
soil_mean

# Create the ranking -- Multiply F1 by the explained variance
soil_mean <- soil_mean %>%
               mutate(ranking = F1 * 
                        summary_pca$variance_proportion[1])

# Visualyzing the final ranking
soil_mean %>%
  arrange(desc(ranking)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

## Load shapefile data containing SP counties and visualyzing
setwd("C:/Users/neliq/Documents/MBA_monografia")
list.files()

SP_counties <- readOGR("SP_Municipios_2021.shp", use_iconv=TRUE, encoding="UTF-8")
SP_counties@data$CD_MUN <- as.numeric(SP_counties@data$CD_MUN)
tm_shape(SP_counties) + 
  tm_borders()

## Merge data
counties_data <- merge(SP_counties,
                         soil_mean,
                         by.x = "CD_MUN",
                         by.y = "CD_MUN")

## Plotting rankings
tmap_mode("view")
tm_shape(counties_data) +
  tm_fill("ranking", midpoint = 0, palette = "RdBu", 
          style = "quantile", n = 10, legend.show = T) +
  tm_borders(alpha = 0.8) ##+ 
  ##tm_text("NM_MUN.x", size = 0.5)
