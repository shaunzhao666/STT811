# bagging example without random forest package
library(tree)

#train/test split
split_pct <- 0.7
n <- length(OJ$Purchase)*split_pct # train size
row_samp <- sample(1:length(OJ$Purchase), n, replace = FALSE)
train <- OJ[row_samp,]
test <- OJ[-row_samp,]

# re-sampling, build trees, predict on test
predicts <- matrix(nrow = length(test$Purchase), ncol = 0)
for(i in 1:1000){
  rows <- sample(1:length(train$Purchase), length(train$Purchase), replace = TRUE)
  samp <- OJ[rows,]
  OJ_trees <- tree(Purchase ~ LoyalCH + PriceDiff + as.factor(StoreID), data = samp)
  predicts = cbind(predicts, predict(OJ_trees, test)[,1])
}
ens <- rowMeans(predicts)

confusionMatrix(as.factor(ifelse(ens < 0.5, 'MM', 'CH')), reference = test$Purchase)

# Random Forest model
library(randomForest)
OJ_rf <- randomForest(Purchase ~ LoyalCH + PriceDiff + StoreID, data = train, mtry = 2, importance = TRUE, ntree = 1000, maxnodes = 4)
rf_predict <- predict(OJ_rf, test)
confusionMatrix(rf_predict, test$Purchase)
barplot(OJ_rf$importance[,3])