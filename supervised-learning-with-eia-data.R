## ------------------------------------------------------------------------
# Import all required libraries
library(broom)
library(caret)
library(knitr)
library(PerformanceAnalytics)
library(Quandl)
library(sigr)
library(tidyverse)

## ------------------------------------------------------------------------
# Import data from the EIA Database in Quandl
wti <- Quandl(code = "CHRIS/ICE_T1", type = "xts")  # West Texas Intermediate Traded Prices
inv <- Quandl(code = "EIA/STEO_COSXPUS_M", type = "xts")  # U.S. Crude Oil Inventories
prod <- Quandl(code = "EIA/STEO_COPRPUS_M", type = "xts")  # U.S. Crude Oil Production
impts <- Quandl(code = "EIA/PET_MCEIMUS1_M", type = "xts")  # U.S. Crude Oil Imports
refy <- Quandl(code = "EIA/PET_MOPUEUS2_M", type = "xts")  # U.S. Refinery Percent Utilization Rates
cons <- Quandl(code = "EIA/STEO_TETCFUEL_M", type = "xts")  # U.S. Total Energy Consumption
gdp <- Quandl(code = "EIA/STEO_GDPQXUS_PCT_M", type = "xts")  # U.S. Real GDP

# Examine the structure of the 'wti' xts object
str(wti)
head(wti)
tail(wti)



## ------------------------------------------------------------------------
# Plot a time series chart of West Texas Intermediate Closing Prices from 2006 to 2019
ggplot(data = wti, mapping = aes(x= index(wti), y = Settle)) +
  geom_line() +
  labs(title = "West Texas Intermediate Crude Oil Prices", subtitle = "From 2006-02-03 to 2019-06-14") +
  xlab("Date") +
  ylab("Price")


## ------------------------------------------------------------------------
# Examine the structure of the 'prod' xts object
str(prod)
head(prod)
tail(prod)

## ------------------------------------------------------------------------
names(prod) <- "Production"

ggplot(data = prod, mapping = aes(x = index(prod), y = Production)) +
  geom_line() +
  labs(title = "U.S. Crude Oil Production, Monthly") +
  xlab("Date") +
  ylab("Production in Million Barrels Per Day")



## ------------------------------------------------------------------------
# Examine the structure of the 'refy' xts object
str(refy)
head(refy)
tail(refy)

## ------------------------------------------------------------------------
names(refy) <- "Utilization"

ggplot(data = refy, mapping = aes(x = index(refy), y = Utilization)) +
  geom_line() +
  labs(title = "U.S. Percent Utilization of Refinery Operable Capacity") +
  xlab("Date") +
  ylab("Percent Utilization in %")


## ------------------------------------------------------------------------
# Convert the daily WTI time series to a monthly time series on 'Settle'
wti_monthly <- to.monthly(wti$Settle)

# Examine the monthly WTI xts object
str(wti_monthly)
head(wti_monthly)
tail(wti_monthly)

## ------------------------------------------------------------------------
# Combine all time series into a single object for analysis
oil <- merge(wti_monthly, inv, prod, impts, refy, cons, gdp)

# Drop the Open, High, Low columns from oil
oil <- oil[, -c(1, 2, 3)]

# Filter to a subset with available data on all variables
oil <- oil["2006-02/2019-03"]

# Rename columns
names(oil) <- c("price", "inv", "prod", "impts", "util", "cons", "gdp")

# Conver the xts object to a data.frame
oil <- as.data.frame(oil)

# Examine the oil data.frame object
str(oil)
head(oil)
tail(oil)


## ------------------------------------------------------------------------
# Explore Linear Regression with one variable: Production
fmla_lm <- as.formula("price ~ prod")

# Train a Linear Regression model
model_lm <- lm(formula = fmla_lm, data = oil)

# Examine the results of the model fit
summary(model_lm)
glance(model_lm)
wrapFTest(model_lm)


## ------------------------------------------------------------------------
# Visualize a linear model between price and prod
oil$pred <- predict(model_lm)

# Plot the Actual Prices against the Model's Predicted Prices
ggplot(data = oil, mapping = aes(x = pred, y = price)) +
  geom_point() +
  geom_abline(color = "darkblue") +
  labs(title = "Predicted WTI Price versus Actual WTI Price", subtitle = "In-Sample Predictions") +
  xlab("Predicted Price") +
  ylab("Actual Price")

# Remove the predictions from the univariate linear model
oil <- oil %>% select(-pred)


## ------------------------------------------------------------------------
# Set seed to 42 to replicate randomized results
set.seed(42)

# Shuffle the rows of the data.frame
permuted_rows <- sample(nrow(oil))
shuffled_oil <- oil[permuted_rows, ]

# Implement a split at 80%/20% of data
split <- round(nrow(shuffled_oil) * 0.80)

# Training Data defined as rows prior to 80%
oil_train <- shuffled_oil[1:split, ]

# Testing Data defined as rows after 80%
oil_test <- shuffled_oil[(split + 1):nrow(shuffled_oil), ]

# Examine both the training and testing datasets
str(oil_train)
str(oil_test)


## ------------------------------------------------------------------------
# Create a 5-fold object
myFolds <- createFolds(oil_train$price, k = 5)

# Define a standardized trainControl object for caret::train()
myControl <- trainControl(savePredictions = TRUE, index = myFolds)



## ------------------------------------------------------------------------
# Create a Grid Search object for the glmnet Linear Regression model
myGrid <- expand.grid(alpha = 0:1, lambda = seq(0.0001, 0.1, length = 10))

# Train a glmnet model on the training dataset
model_glmnet <- train(
  price ~ ., 
  data = oil_train, 
  method = "glmnet",
  tuneGrid = myGrid,
  trControl = myControl,
  preProcess = c("medianImpute", "center", "scale", "pca")
)

# Return a summary of the glmnet_model
print(model_glmnet)
summary(model_glmnet)

## ------------------------------------------------------------------------
# Visualize the results of the glmnet model
plot(model_glmnet, metric = "Rsquared")

plot(model_glmnet$finalModel)


## ------------------------------------------------------------------------
# Generate predictions for the testing dataset with predict()
oil_test$pred_glmnet <- predict(model_glmnet, oil_test)

# Visualize the results of the predictive modelling
ggplot(data = oil_test, mapping = aes(x = pred_glmnet, y = price)) +
  geom_point() +
  geom_abline(color = "darkblue") +
  labs(title = "Actual WTI Prices vs. Predicted WTI Prices by glmnet Model", subtitle = "Out-of-sample Predictions") +
  xlab("glmnet Predicted WTI Prices") +
  ylab("Actual WTI Prices")


## ------------------------------------------------------------------------
# Train a Random Forest model on the oil_train dataset
model_rf <- train(
  price ~ .,
  data = oil_train,
  method = "ranger",
  trControl = myControl,
)

# Return a summary of the model_rf object
print(model_rf)
summary(model_rf)


## ------------------------------------------------------------------------
# Generate predictions for the testing dataset with predict()
oil_test$pred_rf <- predict(model_rf, oil_test)

# Visualize the results of the predictive modelling
ggplot(data = oil_test, mapping = aes(x = pred_rf, y = price)) +
  geom_point() +
  geom_abline(color = "darkblue") +
  labs(title = "Actual WTI Prices vs. Predicted WTI Prices by Random Forest Model", subtitle = "Out-of-sample Predictions") +
  xlab("Random Forest Predicted WTI Prices") +
  ylab("Actual WTI Prices")


## ------------------------------------------------------------------------
# Create a list of trained models: model_list
model_list <- list(glmnet = model_glmnet, rf = model_rf)

# Use caret::resampels() to generate evaluative statistics for each model
resamps <- resamples(model_list)

# Print the resamples object
print(resamps)


## ------------------------------------------------------------------------
# Print a summary of the resamples object
summary(resamps)

## ------------------------------------------------------------------------
bwplot(resamps, metric = "Rsquared")

## ------------------------------------------------------------------------
dotplot(resamps, metric = "Rsquared")

## ------------------------------------------------------------------------
densityplot(resamps, metric = "Rsquared")

## ------------------------------------------------------------------------
xyplot(resamps, metric = "Rsquared")


## ------------------------------------------------------------------------
purl("supervised-learning-with-eia-data.Rmd")

