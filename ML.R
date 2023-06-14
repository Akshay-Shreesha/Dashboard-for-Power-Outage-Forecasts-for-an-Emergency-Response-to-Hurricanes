#install.packages("tidyverse")
library(tidyverse)

# Install necessary packages (if not already installed)
install.packages("glm2")

# Load the required library
library(glm2)

data <- read_csv("C:/Users/omhai/OneDrive/Documents/GitHub/Dashboard-for-Power-Outage-Forecasts-for-an-Emergency-Response-to-Hurricanes/regression_df_dif.csv")

# Rename the "_mean" column to "mean_value"
colnames(data)[colnames(data) == "_mean"] <- "mean_value"

# Remove rows with NA/NaN/Inf in the mean_value column
data <- data[!is.na(data$mean_value) & is.finite(data$mean_value), ]

data<- data[data$dif_nightlight > 0, ]

shape <- dim(data)
print(shape)

# Create the formula with "mean_value" as the dependent variable and specific column names
formula <- as.formula("dif_nightlight ~ HISTO_0 + HISTO_1 + HISTO_5 + HISTO_6 + HISTO_8 + HISTO_10 + HISTO_14 + HISTO_15 + HISTO_16 + HISTO_17 + HISTO_18 + Vmax + nightlight_prev + Rainfall")

# Perform quasi-Poisson regression
qp_model <- glm(formula, data = data, family = quasipoisson(link = "log"))

# Print the summary of the regression results
summary(qp_model)

# Standardize the independent variables
scaled_data <- scale(data[, -which(names(data) != "mean_value")])

# Split the data into training and test sets
set.seed(42)  # For reproducibility
train_indices <- sample(1:nrow(data), nrow(data) * 0.8)  # 80% for training
train_data <- scaled_data[train_indices, ]
test_data <- scaled_data[-train_indices, ]
train_mean <- data$mean_value[train_indices]
test_mean <- data$mean_value[-train_indices]

# Perform quasi-Poisson regression
qp_model <- glm(formula, data = train_data, family = quasipoisson(link = "log"))

# Make predictions on the test set
predicted <- predict(qp_model, newdata = test_data)

# Calculate the mean squared error
mse <- mean((predicted - test_mean)^2)

# Print the mean squared error
print(mse)