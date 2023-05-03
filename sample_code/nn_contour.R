library(neuralnet)
library(caret)
split_pct <- 0.7
n <- length(OJ$Purchase)*split_pct # train size
row_samp <- sample(1:length(OJ$Purchase), n, replace = FALSE)
train <- OJ[row_samp,]
test <- OJ[-row_samp,]
relu <- function(x) x
OJnn <- neuralnet(Purchase ~ PriceDiff + LoyalCH, data = train, act.fct = 'logistic', hidden = c(2), linear.output = FALSE)

OJpred <- ifelse(predict(OJnn, test)[,1] > 0.5, 'CH', 'MM')
confusionMatrix(as.factor(OJpred), test$Purchase)
x <- seq(-0.67,0.64, by = 0.01)
y <- seq(0 , 1, by = 0.01)
con_df <- expand.grid(x,y)
colnames(con_df) <- c('PriceDiff','LoyalCH')
con_df$z <- predict(OJnn, con_df)[,1]
ggplot(data = con_df, aes(x = PriceDiff, y = LoyalCH, z = z))+geom_contour(aes(color = factor(..level..)))

# Manually creating neural network predictions
xval <- cbind(test$PriceDiff,test$LoyalCH)
logi <- function(x) 1/(1 + exp(-1*x))
n1 <- logi(OJnn$result.matrix[4] + OJnn$result.matrix[5]*xval[,1] + OJnn$result.matrix[6]*xval[,2])
n2 <- logi(OJnn$result.matrix[7] + OJnn$result.matrix[8]*xval[,1] + OJnn$result.matrix[9]*xval[,2])
pr <- logi(OJnn$result.matrix[10] + OJnn$result.matrix[11]*n1 + OJnn$result.matrix[12]*n2)
cbind(pr,predict(OJnn, test)[,1])
