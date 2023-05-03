library(xgboost)
library()
library(caret)


# create tuning grid
tune_grid <- expand.grid(nrounds = seq(50,300, by = 50), max_depth = 1:4, eta = seq(.01, .3, by = 0.01), gamma=0, colsample_bytree=1, min_child_weight=1, subsample=1)

x_train <- data.matrix(heart[,c(2:14)])
y_train <- as.factor(heart$y)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 8, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_tune <- caret::train(
  x = x_train,
  y = y_train,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE,
)

tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(xgb_tune$results$Accuracy, probs = probs), min(xgb_tune$results$Accuracy))) +
    theme_bw()
}

tuneplot(xgb_tune)