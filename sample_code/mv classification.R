# multivalued target
# From heart dataset, predict ChestPain

split_pct <- 0.7
n <- length(heart$y)*split_pct # train size
row_samp <- sample(1:length(heart$y), n, replace = FALSE)
train <- heart[row_samp,]
test <- heart[-row_samp,]


# Multinomial Logistic regression
library(nnet)
pain_mod <- multinom(data = train, ChestPain ~ RestBP + Age + MaxHR)
test_predict <- predict(pain_mod, test, "class")
confusionMatrix(test_predict, as.factor(test$ChestPain))

# Naive Bayes model
pain_nb <- naiveBayes(data = train, ChestPain ~ RestBP + Age + MaxHR)
nb_predict <- predict(pain_nb, test, "class")
confusionMatrix(nb_predict, as.factor(test$ChestPain))

# QDA model
pain_qda <- qda(data = train, ChestPain ~ RestBP + Age + MaxHR)
qda_predict <- predict(pain_qda, test, type = "class")
confusionMatrix(qda_predict$class, as.factor(test$ChestPain))