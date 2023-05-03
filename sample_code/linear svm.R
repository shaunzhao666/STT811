library(e1071)
library(caret)
library(ISLR2)
# data plot
ggplot(data = OJ, aes(x = PriceDiff, y = LoyalCH, color = Purchase)) + geom_point()
split_pct <- 0.7
n <- length(OJ$Purchase)*split_pct # train size
row_samp <- sample(1:length(OJ$Purchase), n, replace = FALSE)
train <- OJ[row_samp,]
test <- OJ[-row_samp,]

svm_mod <- svm(y ~ PriceDiff + LoyalCH, data = train, type = 'C-classification', kernel = 'linear', cost = 1, gamma = 0.5)
# other kernel options: polynomial, radial, sigmoid, linear

pred <- predict(svm_mod, test)
confusionMatrix(as.factor(pred), as.factor(test$y))

# Examining prediction space
dot_df <- data.frame('PriceDiff' = numeric(0), 'LoyalCH' = numeric(0), 'predict' = numeric(0))
for(i in seq(min(OJ$PriceDiff), max(OJ$PriceDiff), by =0.01)){
  for(j in seq(min(OJ$LoyalCH), max(OJ$LoyalCH), by = 0.01)){
    dot_df[nrow(dot_df)+1,] = c(i,j, 0)
  }
}
dot_df$predict = predict(svm_mod, dot_df)
ggplot(data = dot_df, aes(x = PriceDiff, y = LoyalCH, color = predict)) + geom_point()

