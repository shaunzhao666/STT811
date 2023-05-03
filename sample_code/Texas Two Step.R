library(neuralnet)
library(caret)
library(xgboost)
split_pct <- 0.7
n <- length(OJ$Purchase)*split_pct # train size
row_samp <- sample(1:length(OJ$Purchase), n, replace = FALSE)
train <- OJ[row_samp,]
test <- OJ[-row_samp,]
OJnn <- neuralnet(Purchase ~ PriceDiff + LoyalCH, data = train, act.fct = 'logistic', hidden = c(4), linear.output = FALSE)
OJpred <- ifelse(predict(OJnn, test)[,1] > 0.5, 'CH', 'MM')
confusionMatrix(as.factor(OJpred), test$Purchase)

# Manually creating neural network predictions
xval <- cbind(train$PriceDiff,train$LoyalCH)
logi <- function(x) 1/(1 + exp(-1*x))
n1 <- logi(OJnn$result.matrix[4] + OJnn$result.matrix[5]*xval[,1] + OJnn$result.matrix[6]*xval[,2])
n2 <- logi(OJnn$result.matrix[7] + OJnn$result.matrix[8]*xval[,1] + OJnn$result.matrix[9]*xval[,2])
n3 <- logi(OJnn$result.matrix[10] + OJnn$result.matrix[11]*xval[,1] + OJnn$result.matrix[12]*xval[,2])
n4 <- logi(OJnn$result.matrix[13] + OJnn$result.matrix[14]*xval[,1] + OJnn$result.matrix[15]*xval[,2])

train2 <- cbind(n1,n2,n3,n4)
tts_train <- ifelse(train$Purchase == "MM", 1, 0)

tts <- xgboost(data = data.matrix(train2), nrounds = 100, max_depth = 2, eta = 0.3, label = tts_train, objective = "binary:logistic")

xval <- cbind(test$PriceDiff,test$LoyalCH)
n1 <- logi(OJnn$result.matrix[4] + OJnn$result.matrix[5]*xval[,1] + OJnn$result.matrix[6]*xval[,2])
n2 <- logi(OJnn$result.matrix[7] + OJnn$result.matrix[8]*xval[,1] + OJnn$result.matrix[9]*xval[,2])
n3 <- logi(OJnn$result.matrix[10] + OJnn$result.matrix[11]*xval[,1] + OJnn$result.matrix[12]*xval[,2])
n4 <- logi(OJnn$result.matrix[13] + OJnn$result.matrix[14]*xval[,1] + OJnn$result.matrix[15]*xval[,2])
test2 <- cbind(n1,n2,n3,n4)
pred <- predict(tts, data.matrix(test2[]))
confusionMatrix(as.factor(as.integer(2*pred)), as.factor(ifelse(test$Purchase == "MM",1,0)))
