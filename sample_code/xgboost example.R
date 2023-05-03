library(xgboost)
library()
library(caret)

split_pct <- 0.7
n <- length(heart$y)*split_pct # train size
row_samp <- sample(1:length(heart$y), n, replace = FALSE)
train <- heart[row_samp,]
test <- heart[-row_samp,]

heart_xgb <- xgboost(data = data.matrix(train[,c(2:14)]), nrounds = 100, max_depth = 2, eta = 0.3, label = train$y, objective = "binary:logistic")

pred <- predict(heart_xgb, data.matrix(test[,c(2:14)]))
confusionMatrix(as.factor(as.integer(2*pred)), as.factor(test$y))

xgb.importance(colnames(train[,c(2:14)]), model = heart_xgb)