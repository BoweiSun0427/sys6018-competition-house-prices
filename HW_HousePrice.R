library(readr)
library(dplyr)
library(tidyverse)


# Load Data ---------------------------------------------------------------

train <- read_csv("train.csv")
test <- read_csv("test.csv")

train.lm <- lm(SalePrice ~ ., data =train)
summary(train.lm)
mse1 <- mean(train.lm$residuals^2)
mse1

#predict
test.lm <- predict(train.lm, newdata=test)
