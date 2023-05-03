heart <- read.csv("Heart.csv")
library(caret)
library(ggplot2)

# Convert target to 0/1
heart$y <- ifelse(heart$AHD=="Yes",1,0)

# logistic regression model
heart_mod <- glm(data = heart, y ~ Age + MaxHR + Thal + as.factor(ExAng), family = binomial)

confusionMatrix(data = as.factor(as.integer(2*heart_mod$fitted.values)), reference = as.factor(heart$y))

contourdata <- data.frame("ExAng" = as.numeric(), "Thal" = as.character(), "Age" = as.numeric(), "MaxHR" = as.numeric())
for(i in 0:1){
  for(j in c('fixed', 'normal', 'reversable')){
    for(k in min(heart$Age):max(heart$Age)){
      for(l in min(heart$MaxHR):max(heart$MaxHR)){
        contourdata[nrow(contourdata)+1,]$ExAng <- i
        contourdata[nrow(contourdata),]$Thal <- j
        contourdata[nrow(contourdata),]$Age <- k
        contourdata[nrow(contourdata),]$MaxHR <- l
      }
    }
  }
}
contourdata$y <- predict(heart_mod, contourdata, type = "response")

ggplot(data = contourdata, aes(x = Age, y = MaxHR, z = y)) + geom_contour(aes(color = factor(..level..))) + facet_wrap(~as.factor(ExAng)+Thal)