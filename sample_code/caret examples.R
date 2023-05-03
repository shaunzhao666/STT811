library(caret)


# Set training rules
tc <- trainControl(method = 'repeatedcv',
             number = 14,
             repeats =  20,
             search = 'random')

# Build logistic regression model
ht_logit <- train(AHD ~ RestBP + ChestPain + Age,
      data = heart,
      method = 'glmnet',
      trControl = tc,
      family = 'binomial' )

ht_logit$results$Accuracy

# Build Naive Bayes model
ht_nb <- train(AHD ~ RestBP + ChestPain + Age,
                  data = heart,
                  method = 'nb',
                  trControl = tc)
ht_nb$results$Accuracy

# Build LDA model
ht_lda <- train(AHD ~ RestBP + ChestPain + Age,
                data = heart,
                method = 'lda',
                trControl = tc)
ht_lda$results$Accuracy

# Build QDA model
ht_qda <- train(AHD ~ RestBP + ChestPain + Age,
                data = heart,
                method = 'qda',
                trControl = tc)
ht_qda$results$Accuracy

# Build KNN model
ht_knn <- train(AHD ~ RestBP + ChestPain + Age,
                          data = heart,
                          method = 'knn',
                          trControl = tc, 
                          preProcess = c("center","scale"))
ht_knn$results$Accuracy