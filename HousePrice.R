# preparation
library(tidyverse)
library(naniar)
library(simputation)
test = read.csv("test.csv")
train = read.csv("train.csv")

# combining datasets
test$SalePrice = 0
train$Train = "YES"
test$Train = "NO"
merged_dat = rbind(train, test)

# imputing missing values
vars_int = c("BsmtFullBath", "BsmtHalfBath", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "GarageCars", "GarageArea")
vars_factor = c("MSZoning", "Utilities", "Functional", "Exterior1st", "Exterior2nd", "Electrical", "KitchenQual", "SaleType")
merged_dat = impute_mean_at(merged_dat, vars_int)