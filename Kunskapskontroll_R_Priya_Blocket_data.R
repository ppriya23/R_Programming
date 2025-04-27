# Load Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(Metrics)
library(tidyr)
library(corrplot)

# Load and Preprocess Data
file_path <- "C:/Priya/Data_Scientist/R/Volkswagen.xlsx"
cars <- read_excel(file_path) %>%
  na.omit() %>%
  mutate(
    Säljare = as.factor(Säljare),
    Bränsle = as.factor(Bränsle),
    Växellåda = as.factor(Växellåda),
    Biltyp = as.factor(Biltyp),
    Drivning = as.factor(Drivning),
    Färg = as.factor(Färg),
    Modell = as.factor(Modell),
    Region = as.factor(Region)
  )

#view data(EDA)
dim(cars)
head(cars)
str(cars)
summary(cars)

ggplot(cars, aes(x = Försäljningspris, y = `Hästkrafter(HK)`)) +
  geom_point(color = "blue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Linear fit line 
  theme_minimal()


ggplot(cars, aes(x = Försäljningspris, y = Miltal)) +
  geom_point(color = "blue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear fit line
  theme_minimal()

ggplot(cars, aes(x = Bränsle, y = Försäljningspris)) + 
  geom_boxplot(fill = "lightblue") + 
  theme_minimal()


# Explore Correlation 
corr_data <- cars %>% select(where(is.numeric))
nocorr_matrix <- cor(corr_data)
corrplot(corr_matrix, method = "circle")

#Train/Validation/Test Split
set.seed(123)
spec <- c(train = 0.6, validate = 0.2, test = 0.2)

g <- sample(cut(
  seq(nrow(cars)),
  nrow(cars) * cumsum(c(0, spec)),
  labels = names(spec)))

split_data <- split(cars, g)

cars_train <- split_data$train
cars_val <- split_data$validate
cars_test <- split_data$test

# Remove Outliers 
threshold <- quantile(cars_train$Försäljningspris, 0.99)
cars_train <- filter(cars_train, Försäljningspris < threshold)

# Model 1: Basic Linear Regression 
lm_1 <- lm(Försäljningspris ~ Miltal + Modellår + `Hästkrafter(HK)` + Växellåda , data = cars_train)
summary(lm_1)

# Diagnostics for Model 1
par(mfrow = c(2, 2))
plot(lm_1)
vif(lm_1)

# Cook's Distance
cooksd <- cooks.distance(lm_1)
plot(cooksd, type="h", main="Cook's Distance", ylab="Distance")

# High-leverage points
leverage_values <- hatvalues(lm_1)
plot(leverage_values, type="h", main="Leverage Values", ylab="Leverage")

# Check threshold
influential_points <- which(cooksd > 1 | leverage_values > (2 * mean(leverage_values)))
print(influential_points)

cars_train_cleaned <- cars_train[-influential_points, ] # Remove all flagged points
lm_1_cleaned <- lm(Försäljningspris ~ Miltal + Modellår + `Hästkrafter(HK)` + Växellåda , data = cars_train_cleaned)
summary(lm_1_cleaned)
par(mfrow = c(2, 2))
plot(lm_1_cleaned)
vif(lm_1_cleaned)

# Model 2: Add Fuel Type 
lm_2 <- lm(Försäljningspris ~ Miltal + Modellår + `Hästkrafter(HK)` + Bränsle + Växellåda, data = cars_train )
summary(lm_2)

#  Diagnostics for Model 2 
par(mfrow = c(2, 2))
plot(lm_2)
vif(lm_2)

# Cook's Distance
cooksd <- cooks.distance(lm_2)
plot(cooksd, type="h", main="Cook's Distance", ylab="Distance")

# High-leverage points
leverage_values <- hatvalues(lm_2)
plot(leverage_values, type="h", main="Leverage Values", ylab="Leverage")

# Check threshold
influential_points <- which(cooksd > 1 | leverage_values > (2 * mean(leverage_values)))
print(influential_points)

cars_train_cleaned <- cars_train[-influential_points, ]# Removing Influential Observations
lm_2_cleaned <- lm(Försäljningspris ~ Miltal + Modellår + `Hästkrafter(HK)` + Bränsle + Växellåda , data = cars_train_cleaned)
summary(lm_2_cleaned)
summary(lm_2)  # Original model
par(mfrow = c(2, 2))
plot(lm_2_cleaned)
vif(lm_2_cleaned)


#  Model 3: Minimal Model 
lm_3 <- lm(Försäljningspris ~ Modellår + `Hästkrafter(HK)`, data = cars_train)
summary(lm_3)

#  Diagnostics for Model 3 
par(mfrow = c(2, 2))
plot(lm_3)
vif(lm_3)


# Evaluate on Validation Set 
val_pred_m1 <- predict(lm_1_cleaned, newdata = cars_val)
val_pred_m2 <- predict(lm_2_cleaned, newdata = cars_val)
val_pred_m3 <- predict(lm_3, newdata = cars_val)

results <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  RMSE_Val = c(rmse(cars_val$Försäljningspris, val_pred_m1),
               rmse(cars_val$Försäljningspris, val_pred_m2),
               rmse(cars_val$Försäljningspris, val_pred_m3)),
  Adj_R2 = c(summary(lm_1_cleaned)$adj.r.squared,
             summary(lm_2_cleaned)$adj.r.squared,
             summary(lm_3)$adj.r.squared),
  BIC = c(BIC(lm_1_cleaned), BIC(lm_2_cleaned), BIC(lm_3))
)
print(results)

# Evaluate Best Model (Model 2) on Test Set 
test_predictions <- predict(lm_2_cleaned, newdata = cars_test)
test_rmse <- rmse(cars_test$Försäljningspris, test_predictions)
cat("Test RMSE (Model 2):", test_rmse, "\n")
summary(lm_2_cleaned)
confint(lm_2_cleaned)

# Plot: Actual vs Predicted (Model 2)
ggplot(cars_test, aes(x = Försäljningspris, y = test_predictions)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted Prices (Model 2)", x = "Actual Prices", y = "Predicted Prices") +
  theme_minimal()




