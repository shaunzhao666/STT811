heart <- read.csv("Heart.csv")
library(caret)
library(ggplot2)

# train-test split
split_pct <- 0.7
n <- length(heart$y)*split_pct # train size
row_samp <- sample(1:length(heart$y), n, replace = FALSE)
train <- heart[row_samp,]
test <- heart[-row_samp,]
heart_train_mod <- glm(data = train, y ~ MaxHR + RestBP + ChestPain, family = binomial)
test_pred <- predict(heart_train_mod,test, type = "response")
train_cm <- confusionMatrix(as.factor(as.integer(2*heart_train_mod$fitted.values)), reference = as.factor(train$y))
test_cm <- confusionMatrix(as.factor(as.integer(2*test_pred)), reference = as.factor(test$y))
train_cm$table
test_cm$table

# bootstrapped parameter estimates
coeff1 <- rep(0, 1000)
coeff2 <- rep(0, 1000)
n <- nrow(heart)
for(i in 1:1000){
  row_samp <- sample(1:n, replace = TRUE)
  heart_samp <- heart[row_samp,]
  temp_mod <- glm(data = heart_samp, y ~ MaxHR + RestBP, family = binomial)
  coeff1[i] <- temp_mod$coefficients[2]
  coeff2[i] <- temp_mod$coefficients[3]
}
quantile(coeff1, c(0.025, 0.975))
quantile(coeff2, c(0.025, 0.975))

# calibration test
df <- data.frame('y' = heart_train_mod$y, 'fit' = heart_train_mod$fitted.values)
calib <- data.frame('count' = numeric(0), 'bin' = numeric(0), 'prob' = numeric(0))
for(i in 1:10){
  temp <- filter(df, fit > (i-1)/10 & fit < i/10)
  calib[nrow(calib) + 1,]$count <- nrow(temp)
  calib[nrow(calib),]$bin <- (i - .5)/10
  calib[nrow(calib),]$prob <- mean(temp$y)
}
calib