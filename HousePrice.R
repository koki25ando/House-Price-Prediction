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

# imputing missing values
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