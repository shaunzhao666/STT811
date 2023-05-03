library(tree)

split_pct <- 0.7
n <- length(OJ$Purchase)*split_pct # train size
row_samp <- sample(1:length(OJ$Purchase), n, replace = FALSE)
train <- OJ[row_samp,]
test <- OJ[-row_samp,]
train <-

OJ_tree <- tree(Purchase ~ PriceDiff + LoyalCH, data = train)

plot(OJ_tree)
text(OJ_tree)
tree_pred <- predict(OJ_tree, test, type = 'class')

confusionMatrix(tree_pred, test$Purchase)