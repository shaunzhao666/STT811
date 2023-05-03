library(MASS)

Heart_lda_mod <- lda(data = heart, AHD ~ Age + MaxHR)

x <- min(heart$Age):max(heart$Age)
y <- min(heart$MaxHR):max(heart$MaxHR)

#cleaned up contour plot
lda_contour <- data.frame("MaxHR" = numeric(), "Age"= numeric())
for(i in x){
  for(j in y){
    lda_contour[nrow(lda_contour)+1,] <- c(j,i)
  }
}
lda_predict <- predict(Heart_lda_mod, lda_contour)
lda_contour$z <- lda_predict$posterior[,2]

ggplot(data = lda_contour, aes(x = MaxHR, y = Age, z = z)) + geom_contour(aes(color = factor(..level..)), bins = 20)

# Look at covariance matrix for MaxHR and Age
cov(heart[,c(2,9)])

# covariance matrix for more X's
cor(heart[,c(2,5,6,9)])

# or correlation matrix
cor(heart[,c(2,5,6,9)])
cor(filter(heart,AHD=="Yes")[,c(2,5,6,9)])
cor(filter(heart,AHD=="No")[,c(2,5,6,9)])

# by class
cov(filter(heart,AHD=="Yes")[,c(2,9)])
cov(filter(heart,AHD=="No")[,c(2,9)])
mean(filter(heart,AHD=="Yes")$Age)
mean(filter(heart,AHD=="No")$Age)
mean(filter(heart,AHD=="Yes")$MaxHR)
mean(filter(heart,AHD=="No")$MaxHR)