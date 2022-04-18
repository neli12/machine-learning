## Load libraries
library(raster)
library(caret)
library(ggplot2)

## Set working directory , list files and load dataset
setwd("C:/Users/FREY/Documents")
list.files()

dados  <- read.csv("new_dataset.csv")
SYSI <- stack("SYSI.tif")
names(SYSI) <- c("B1", "B2", "B3", "B4", "B5", "B6")

## Or load dataset from github
githubURL <- "https://github.com/neli12/machine-learning-R/raw/main/new_dataset.RData"
load(url(githubURL))


# Select randomly 30% of the dataset and split in training and validation
set.seed(45)
val_rows <- sample(nrow(dados), 116)

dat_train <- dados[-val_rows,]
dat_test <- dados[val_rows,]


## Split Xs variables from the target variable
covariates_train <- dat_train[, -1:-2]
covariates_test <- dat_test[, -1:-2]

#### Boostrapping

## Set the number of repetitions
k <- 50

# Create an empty matrix to store the predictions of the validation set for each k
preds <- matrix(nrow = nrow(dat_test), ncol = k)

# Create an empty RasterStack to store rasters of each k
raster_preds <- stack()

# Run the boostrapping

for(i in 1:k) {
  
  library(caret)
  
  # Generate a bootstrap resample of dataset rows
  cal_rows <- sample(nrow(dat_train), nrow(dat_train), replace = TRUE)
  
  # Set the grid search -- You can change this or set by default
  grid <- expand.grid(committees = 1, neighbors = 0)
  
  # Fit a Cubist Argila
  fit_model <- train(x = covariates_train[cal_rows, ],
                           y = dat_train$Clay.gkg[cal_rows],
                           method = "cubist",
                           trControl = trainControl(method = "none"),
                           tuneGrid = grid)
  
  # Predict onto validation samples
  preds[, i] <- predict(fit_model, newdata = covariates_test)
  
  # Predict onto raster
  raster_preds <- stack(raster_preds,
                          raster::predict(SYSI, fit_model))
}


summary(fit_model$finalModel)

# Compute mean of bootstrap repetitions
preds_mean <- raster::calc(raster_preds, mean)
plot(z_mean, main = "Bootstrapped Cubist Clay content")

# Compute prediction interval limits (5 and 95%)
preds_interval <- raster::calc(raster_preds, 
                           function(x) {
                             quantile(x, probs = c(0.05, 0.95), na.rm = TRUE)
                           })
names(preds_interval) <- c("lower_lim", "upper_lim")
plot(preds_interval)

# Compute prediction interval width
preds_pi <- preds_interval[[2]] - preds_interval[[1]]
plot(preds_pi, main = "90-percent prediction interval width")

