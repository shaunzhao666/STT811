library(class)
# Select only numerical fields and scale them (removing also ID and numerical y)
heart_knn <- scale(select_if(heart[,c(2:15)], is.numeric))

split_pct <- 0.7
n <- split_pct * nrow(heart_knn)
row_samp <- sample(1:nrow(heart_knn), n, replace = FALSE)
train <- heart_knn[row_samp,]
test <- heart_knn[-row_samp,]
train.Y <- heart[row_samp,]$AHD
test.Y <- heart[-row_samp,]$AHD

knn_mod <- knn(train, test, cl = train.Y , k = 5, prob = TRUE)
confusionMatrix(knn_mod , reference = as.factor(test.Y))