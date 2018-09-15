library(readr)
library(dplyr)
library(tidyverse)


# Load Data ---------------------------------------------------------------
train <- read.csv("train.csv")
test <- read.csv("test.csv")

missing_value = data.frame(matrix(ncol = 2, nrow = 81))
names(missing_value) = c("colName","MissingValue")


#cleaning data - find columns with missing value
#conbine train and test data once and do the cleaning at once.
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
train_test$GarageYrBlt[is.na(train_test$GarageYrBlt)] <- names(which.max(table(train_test$GarageYrBlt)))
train_test$GarageCars[is.na(train_test$GarageCars)] <- names(which.max(table(train_test$GarageCars)))
train_test$SaleType[is.na(train_test$SaleType)] <- names(which.max(table(train_test$SaleType)))

#recode with mean value
train_test$LotFrontage[is.na(train_test$LotFrontage)] <- mean(train_test$LotFrontage, na.rm = TRUE)
train_test$MasVnrArea[is.na(train_test$MasVnrArea)] <- mean(train_test$MasVnrArea, na.rm = TRUE)
train_test$BsmtFinSF1[is.na(train_test$BsmtFinSF1)] <- mean(train_test$BsmtFinSF1, na.rm = TRUE)
train_test$BsmtUnfSF[is.na(train_test$BsmtUnfSF)] <- mean(train_test$BsmtUnfSF, na.rm = TRUE)
train_test$TotalBsmtSF[is.na(train_test$TotalBsmtSF)] <- mean(train_test$TotalBsmtSF, na.rm = TRUE)
train_test$GarageArea[is.na(train_test$GarageArea)] <- mean(train_test$GarageArea, na.rm = TRUE)
train_test$BsmtFinSF2[is.na(train_test$BsmtFinSF2)] <- mean(train_test$BsmtFinSF2, na.rm = TRUE)

#NA to "none"
train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)] <-  lapply(train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)], as.character)
train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)][is.na(train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)])] <- "None"
train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)] <-  lapply(train_test[, c(7,31,32,33,34,36,61,64,65,73,74,75)], as.factor)

#run the for loop again to check whether clearning is done
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





train.lm <- lm(SalePrice ~ ., data =train)
summary(train.lm)
mse1 <- mean(train.lm$residuals^2)
mse1

#predict
test.lm <- predict(train.lm, newdata=test)
