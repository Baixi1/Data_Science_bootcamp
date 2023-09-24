library(caret)
library(tidyverse)
library(ggplot2)

House_Price_India <- read_excel("House Price India.xlsx")
df <- House_Price_India

## check NA
df %>%
  complete.cases() %>%
  mean()

## Select Price

# Before Prepare Data
p1 <- ggplot(df, aes(Price)) +
  geom_density() +
  theme_minimal() +
  labs(
    x = "Price",
    y = "Count"
  )
p1

# After Prepare Data
p2 <- ggplot(df, aes(log(Price))) +
  geom_density() +
  theme_minimal() +
  labs(
    x = "Price",
    y = "Count"
  )
p2

## Select column
All_df <- df %>%
  select(`number of bedrooms`, 
         `number of floors`, 
         `Area of the house(excluding basement)`, 
         `Distance from the airport`, 
         `grade of the house`, 
         Price)

All_df$Price <- log(df$Price)

## 1. split data 80% train, 20% test
split_df <- function(df) {
  set.seed(42)
  n <- nrow(df)
  train_id <- sample(1:n, size = 0.8*n)
  train_df <- df[train_id,]
  test_df <- df[-train_id,]
  list(training = train_df, 
       testing = test_df) 
}

prep_data <- split_df(All_df)
train_df <- prep_data[[1]]
test_df <- prep_data[[2]]

## 2. train model
set.seed(42)
df_model <- train(Price ~ .,
                  data = train_df,
                  method = "lm")

df_model

## 3. score model
p <- predict(df_model, newdata=test_df)

## 4. evaluate model
# mean absolute error
(mae <- mean(abs(p - test_df$Price)))

# root mean square error
(rmse <- sqrt(mean((p - test_df$Price)**2)))