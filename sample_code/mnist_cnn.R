library(keras)
library(tidyverse) 

mnist <- dataset_mnist()

# Exploratory Data Analysis
hist(mnist$train$y, breaks = 9)

flip <- matrix(nrow = 28, ncol = 28, rep(0,28*28))
for(i in 1:28){
  flip[i, 29-i] = 1
}
choice <- as.integer(runif(1,0,60000))
samp_image <- flip %*% mnist$train$x[choice,,]
contour( t(samp_image))
mnist$train$y[choice]

# Split train/test
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# Normalize x data
x_train <- x_train/255
x_test <- x_test/255

# model structure
modelnn <- keras_model_sequential()
modelnn %>%
  layer_dense(units = 256, activation = "relu",
              input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = "softmax")

modelnn %>% compile(loss = "categorical_crossentropy",
                    optimizer = optimizer_rmsprop(), metrics = c("accuracy")
)

summary(modelnn)

modelnn %>% compile(loss = "categorical_crossentropy",
                    optimizer = optimizer_rmsprop(), metrics = c("accuracy")
)

system.time(
  history <- modelnn %>%
    #     fit(x_train, y_train, epochs = 30, batch_size = 128,
    fit(x_train, y_train, epochs = 30, batch_size = 128,
        validation_split = 0.2)
)
# plot(history, smooth = FALSE)

accuracy <- function(pred, truth) {
  mean(drop(as.numeric(pred)) == drop(truth)) }
modelnn %>% predict(x_test) %>% k_argmax() %>% accuracy(g_test)

nn_pred <- predict(modelnn, x_test)
image_pred <- rep(0, 10000)
actual <- rep(0,10000)
for(i in  1:10000){
  image_pred[i] = which.max(nn_pred[i,])
  actual[i] = which.max(y_test[i,])
}
data.frame("actual" = actual, "predict" = image_pred)
