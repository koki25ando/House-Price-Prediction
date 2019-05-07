# preparation
library(tidyverse)
library(naniar)
library(simputation)
library(rpart)
library(randomForest)
test = read.csv("test.csv")
train = read.csv("train.csv")

# combining datasets
test$SalePrice = 0
train$Train = "YES"
test$Train = "NO"
merged_dat = rbind(train, test)




### ------------------------ Missing Values imputation ----------------------------------------------



vars_int = c("BsmtFullBath", "BsmtHalfBath", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "GarageCars", "GarageArea")
vars_factor = c("MSZoning", "Utilities", "Functional", "Exterior1st", "Exterior2nd", "Electrical", "KitchenQual", "SaleType")
merged_dat = impute_mean_at(merged_dat, vars_int)
merged_dat = merged_dat %>% 
  impute_cart(MSZoning ~ .) %>% 
  impute_cart(Utilities ~ .) %>% 
  impute_cart(Functional ~ .) %>% 
  impute_cart(Exterior1st ~ .) %>% 
  impute_cart(Exterior2nd ~ .) %>% 
  impute_cart(Electrical ~ .) %>% 
  impute_cart(KitchenQual ~ .) %>% 
  impute_cart(SaleType ~ .)


merged_dat$PoolQC = as.character(merged_dat$PoolQC)
merged_dat[is.na(merged_dat$PoolQC),]$PoolQC = "Hi"
merged_dat$PoolQC = factor(merged_dat$PoolQC)

merged_dat$MiscFeature = as.character(merged_dat$MiscFeature)
merged_dat[is.na(merged_dat$MiscFeature),]$MiscFeature = "High"
merged_dat$MiscFeature = factor(merged_dat$MiscFeature)

merged_dat$Alley = as.character(merged_dat$Alley)
merged_dat[is.na(merged_dat$Alley),]$Alley = "HighSP"
merged_dat$Alley = factor(merged_dat$Alley)

merged_dat$Fence = as.character(merged_dat$Fence)
merged_dat[is.na(merged_dat$Fence),]$Fence = "Hig"
merged_dat$Fence = factor(merged_dat$Fence)

merged_dat$FireplaceQu = as.character(merged_dat$FireplaceQu)
merged_dat[is.na(merged_dat$FireplaceQu),]$FireplaceQu = "Hg"
merged_dat$FireplaceQu = factor(merged_dat$FireplaceQu)

merged_dat = impute_lm(merged_dat, GarageYrBlt ~ SalePrice)

merged_dat[is.na(merged_dat$GarageType),]$GarageType = "Attchd"
merged_dat[is.na(merged_dat$GarageQual),]$GarageQual = "TA"
merged_dat[is.na(merged_dat$GarageCond),]$GarageCond = "TA"
merged_dat[is.na(merged_dat$GarageFinish),]$GarageFinish = "Unf"

merged_dat[is.na(merged_dat$BsmtFinType1),]$BsmtFinType1 = "GLQ"
merged_dat[is.na(merged_dat$BsmtFinType2),]$BsmtFinType2 = "Unf"
merged_dat[is.na(merged_dat$BsmtCond),]$BsmtCond = "TA"
merged_dat[is.na(merged_dat$BsmtExposure),]$BsmtExposure = "No"
merged_dat[is.na(merged_dat$BsmtQual),]$BsmtQual = "TA"

merged_dat[is.na(merged_dat$MasVnrType), ]$MasVnrType = "None"
merged_dat[is.na(merged_dat$MasVnrArea), ]$MasVnrArea = 0

merged_dat[is.na(merged_dat$LotFrontage), ]$LotFrontage = 0


## -------------- xgboost start ------------------------------

library(caret)
merged_dat_dummy = dummyVars(~., data = merged_dat)
merged_dat_dummy2 = as.data.frame(predict(merged_dat_dummy, merged_dat))

train_treat = merged_dat_dummy2 %>% 
  filter(TrainYES == 1) %>% 
  select(-contains("Train"))
test_treat = merged_dat_dummy2 %>% 
  filter(TrainYES == 1) %>% 
  select(-contains("Train"))
library(xgboost)
cv <- xgb.cv(data = as.matrix(train_treat), 
             label = train$SalePrice,
             nrounds = 100,
             nfold = 5,
             objective = "reg:linear",
             eta = .3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0
)
elog <- as.data.frame(cv$evaluation_log)

elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),
            ntrees.test  = which.min(test_rmse_mean))



model_xgb <- xgboost(data = as.matrix(train_treat), 
                     label = train$SalePrice,
                     nrounds = 25,
                     objective = "reg:linear",
                     eta = .3,
                     depth = 6,
                     verbose = 0
)


# Check accuracy

train_treat$pred_xgb = predict(model_xgb, as.matrix(train_treat))
train_treat %>% 
  mutate(xgb_residuals = pred_xgb - SalePrice) %>% 
  summarise(xgb_rmse = sqrt(mean(xgb_residuals^2)))















## -------------- xgboost end --------------------------------




# Predict
train = merged_dat %>% 
  filter(Train == "YES") %>% 
  select(-Train)
test = merged_dat %>% 
  filter(Train == "NO") %>% 
  select(-Train, -SalePrice)

model_rpart = rpart(data = train, SalePrice ~ .)
# model_rf = randomForest(SalePrice ~ ., data = train,
#                         ntree = 500, importance = TRUE)


# Model Accuracy
library(modelr)
mae(model = model_rpart, data = train)
mae(model = model_rf, data = train)
mae(model = model_bag, data = train)

test$SalePrice = predict(model_rf, test)
# submission = select(test, Id, SalePrice)
# write.csv(submission, "submission.csv", row.names = FALSE)