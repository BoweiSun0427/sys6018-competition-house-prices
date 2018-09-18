library(readr)

library(dplyr)

library(tidyverse)

library(FNN)

library(lme4)

options("max.print" = 10000000)

# Load Data ---------------------------------------------------------------

train <- read.csv("train.csv")

test <- read.csv("test.csv")



missing_value = data.frame(matrix(ncol = 2, nrow = 81))

names(missing_value) = c("colName","MissingValue")



#cleaning data - find columns with missing value

#combine train and test data once and do the cleaning at once.

test['SalePrice'] <- 0

train_test <- rbind(train,test)



for (i in 1:81){
  
  temp = as.data.frame(table(is.na(train_test[,i])))
  
  missing_value[i,] <- temp[2,2]
  
  missing_value[i,1] <- colnames(train_test[i])
  
}

missing_value <- filter(missing_value, missing_value$MissingValue != 'NA')

missing_value$colName

# [1] "MSZoning"     "LotFrontage"  "Alley"        "Utilities"    "Exterior1st"  "Exterior2nd" 

# [7] "MasVnrType"   "MasVnrArea"   "BsmtQual"     "BsmtCond"     "BsmtExposure" "BsmtFinType1"

# [13] "BsmtFinSF1"   "BsmtFinType2" "BsmtFinSF2"   "BsmtUnfSF"    "TotalBsmtSF"  "Electrical"  

# [19] "BsmtFullBath" "BsmtHalfBath" "KitchenQual"  "Functional"   "FireplaceQu"  "GarageType"  

# [25] "GarageYrBlt"  "GarageFinish" "GarageCars"   "GarageArea"   "GarageQual"   "GarageCond"  

# [31] "PoolQC"       "Fence"        "MiscFeature"  "SaleType" 



#recode missing values 

#recode with most common

train_test$MSZoning[is.na(train_test$MSZoning)] <- names(which.max(table(train_test$MSZoning)))

train_test$Utilities[is.na(train_test$Utilities)] <- names(which.max(table(train_test$Utilities))) 

train_test$Exterior1st[is.na(train_test$Exterior1st)] <- names(which.max(table(train_test$Exterior1st)))

train_test$Exterior2nd[is.na(train_test$Exterior2nd)] <- names(which.max(table(train_test$Exterior2nd)))

train_test$MasVnrType[is.na(train_test$MasVnrType)] <- names(which.max(table(train_test$MasVnrType)))

train_test$Electrical[is.na(train_test$Electrical)] <- names(which.max(table(train_test$Electrical)))

train_test$BsmtFullBath[is.na(train_test$BsmtFullBath)] <- names(which.max(table(train_test$BsmtFullBath)))

train_test$BsmtHalfBath[is.na(train_test$BsmtHalfBath)] <- names(which.max(table(train_test$BsmtHalfBath)))

train_test$KitchenQual[is.na(train_test$KitchenQual)] <- names(which.max(table(train_test$KitchenQual)))

train_test$Functional[is.na(train_test$Functional)] <- names(which.max(table(train_test$Functional)))

train_test$FireplaceQu[is.na(train_test$FireplaceQu)] <- names(which.max(table(train_test$FireplaceQu)))

train_test$GarageType[is.na(train_test$GarageType)] <- names(which.max(table(train_test$GarageType)))

# train_test$GarageYrBlt[is.na(train_test$GarageYrBlt)] <- names(which.max(table(train_test$GarageYrBlt)))

train_test$GarageCars[is.na(train_test$GarageCars)] <- names(which.max(table(train_test$GarageCars)))

train_test$SaleType[is.na(train_test$SaleType)] <- names(which.max(table(train_test$SaleType)))



#recode with mean value
train_test$GarageYrBlt <-as.numeric(train_test$GarageYrBlt)

train_test$LotFrontage[is.na(train_test$LotFrontage)] <- mean(train_test$LotFrontage, na.rm = TRUE)

train_test$MasVnrArea[is.na(train_test$MasVnrArea)] <- mean(train_test$MasVnrArea, na.rm = TRUE)

train_test$BsmtFinSF1[is.na(train_test$BsmtFinSF1)] <- mean(train_test$BsmtFinSF1, na.rm = TRUE)

train_test$BsmtUnfSF[is.na(train_test$BsmtUnfSF)] <- mean(train_test$BsmtUnfSF, na.rm = TRUE)

train_test$TotalBsmtSF[is.na(train_test$TotalBsmtSF)] <- mean(train_test$TotalBsmtSF, na.rm = TRUE)

train_test$GarageArea[is.na(train_test$GarageArea)] <- mean(train_test$GarageArea, na.rm = TRUE)

train_test$BsmtFinSF2[is.na(train_test$BsmtFinSF2)] <- mean(train_test$BsmtFinSF2, na.rm = TRUE)

train_test$GarageYrBlt[is.na(train_test$GarageYrBlt)] <- mean(train_test$GarageYrBlt, na.rm = TRUE)

#NA to "none"

train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)] <-  lapply(train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)], as.character)

train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)][is.na(train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)])] <- "None"

train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)] <-  lapply(train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)], as.factor)



#run the for loop again to check whether cleaning is done

for (i in 1:81){
  
  temp = as.data.frame(table(is.na(train_test[,i])))
  
  missing_value[i,] <- temp[2,2]
  
  missing_value[i,1] <- colnames(train_test[i])
  
}

missing_value <- filter(missing_value, missing_value$MissingValue != 'NA')

missing_value$colName



#split test_train into test AND train. cleaning is done here 

train <- filter(train_test, train_test$SalePrice != 0)

test <- filter(train_test, train_test$SalePrice == 0)



##################### Parametric ###############################################

# Split the training set 50:50 for cross-validation
set.seed(18)
sub <- sample(1:1460, size = 730)
train.train <- train[sub,]
train.valid <- train[-sub,]

# First check the full model to identify significant features
full.lm <- lm(SalePrice ~ ., data = train.train)
summary(full.lm)
mse.full <- mean(full.lm$residuals^2)
# Significant variables are: LotArea, Neighborhood, OverallQual, OverallCond, ExterQual, BsmtFinType1
#                            BSmtFinSF1, X1stFlrSF, X2ndFlrSF
#                            BedroomAbvGr, GarageQual
# MSE = 277386559

# Use the significant features to build linear model
model1.lm <- lm(SalePrice ~ LotArea+Neighborhood+OverallCond+OverallQual+ExterQual+BsmtFinType1+BsmtFinSF1+
                  X1stFlrSF+X2ndFlrSF+BedroomAbvGr+GarageQual, data = train.train)
summary(model1.lm)
mse.model1 <- mean(model1.lm$residuals^2)
# Significant variables are: LotArea, OverallCond, Neighborhood, OverallQual, ExterQual, BsmtFinSF1, 
#                            X1stFlrSF, X2ndFlrSF, BedroomAbvGr, GarageQual
# MSE = 745475393

# BsmtFinType1 seems insignificant, drop it and build the new model.
model2.lm <- lm(SalePrice ~ LotArea+Neighborhood+OverallCond+OverallQual+ExterQual+BsmtFinSF1+
                  X1stFlrSF+X2ndFlrSF+BedroomAbvGr+GarageQual, data = train.train)
summary(model2.lm)
mse.model2 <- mean(model2.lm$residuals^2)
# Significant variables are: All of them
# MSE = 758971232

# Cross-Validation to compare models. We will select the one with smaller Root Mean Square Log Error. 

# Model 1
y1 <- predict(model1.lm, newdata = train.valid)
RMSLE.1 <- sqrt(sum((log(y1+1)-log(train.valid$SalePrice+1))^2)/length(y1))
RMSLE.1
# [1] 0.2001631

# Model 2
y2 <- predict(model2.lm, newdata = train.valid)
RMSLE.2 <- sqrt(sum((log(y2+1)-log(train.valid$SalePrice+1))^2)/length(y2))
RMSLE.2
# [1] 0.1959042

# So we will use model 2 to make predictions
# Use full training set to train the model
final.model <- lm(SalePrice ~ LotArea+Neighborhood+OverallCond+OverallQual+ExterQual+BsmtFinSF1+
                    X1stFlrSF+X2ndFlrSF+BedroomAbvGr+GarageQual, data = train)

# Make predictions with test dataset
test_predict <- c(predict(final.model, newdata = test))
para_result <- data.frame(test$Id)
para_result["SalePrice"] <- c(test_predict)
write.table(para_result, file = "para_result1.csv", row.names=F, col.names=c("Id","SalePrice"), sep=",")



################################ KNN #######################################

#use only numeric cols

train_numeric <- select_if(train,is.numeric)

test_numeric <- select_if(test,is.numeric)



#By using cor(), here we decide what X variables will be added to KNN model.

cor(train_numeric)

#The following has the higest correlation between salePrice and it.

# c(5,17,24,13)

# OverallQual

# GrLivArea

# GarageArea

# TotalBsmtSF

# X1stFlrSF

# FullBath

# TotRmsAbvGrd

# YearBuilt

# YearRemodAdd



test_numeric['SalePrice'] <- NULL

train_numeric['SalePrice'] <- NULL



# calculate the number of K

k <- floor(sqrt(nrow(train_numeric)))



#nomalize(reference: https://rpubs.com/bskc/300711)

normalize <- function(x) {
  
  norm <- ((x - min(x))/(max(x) - min(x)))
  
  return (norm)
  
}



train_numeric[,2:33] <- apply(train_numeric[,2:33],2,normalize) 

test_numeric[,2:33] <- apply(test_numeric[,2:33],2,normalize) 



#Final selection

train_numeric <- train_numeric[, c(5,17,24,13)]

test_numeric <- test_numeric[, c(5,17,24,13)]



#

# function to calculate euclidean distance(reference: https://rpubs.com/bskc/300711)

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#

eist <- c()

distance <- c()



for (j in 1:1459) {
  
  for (i in 1:1460) {
    
    distance[i] <- euc.dist(train_numeric[i,],test_numeric[j,])
    
    
    
  }
  
  edist_temp <- data.frame(dist=distance, salePrice = train[,81])
  
  edist_temp <- edist_temp[order(edist_temp$dist),]
  
  edist_temp <- edist_temp[1:20,]
  
  distance_mean <- mean(edist_temp$salePrice)
  
  eist[j] <- distance_mean
  
}



#submission - export predictions 

final_result <- data_frame(test$Id)

final_result["salePrice"] <- eist

write.table(final_result, file = "submission.csv", row.names=F, col.names=c("Id","salePrice"), sep=",")