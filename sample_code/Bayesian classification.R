# Naive Bayes
# Categorical Calculation
# P(Y = y| X = x) = (P(X = x|Y = y)*P(Y = y))/Sum((P(X = x|Y = y_i)*P(Y = y_i)))
# For heart, using single categorical variable
# 139 Yes out of 303 total --> P(Y = 'yes') = 139/303
# P(Y = 'Yes' | X = x) = (P(X = x|Y = 'Yes')*P(Y = 'Yes'))/(P(X = x|Y = 'Yes)*P(Y = 'Yes) + P(X = x | Y = 'No') *P(Y = 'No'))
# P(Y = 'Yes' | Chestpain = typical) = P(Chestpain = Typical | Y = 'Yes') *P('Yes')/()
# = 7/139 * 139/303 /(7/139 * 139/303 + 16/164*164/303) = 7/(7+16)

# Naive Bayes from definition
CH_mean <- mean(filter(OJ, Purchase == "CH")$PriceDiff)
CH_sd <- sd(filter(OJ, Purchase == "CH")$PriceDiff)
MM_mean <- mean(filter(OJ, Purchase == "MM")$PriceDiff)
MM_sd <- sd(filter(OJ, Purchase == "MM")$PriceDiff)

MM_frac<- sum(OJ$Purchase == 'MM')/nrow(OJ)

LDA_pred <- MM_frac * dnorm(OJ$PriceDiff, MM_mean, MM_sd)/(MM_frac * dnorm(OJ$PriceDiff, MM_mean, MM_sd) + (1 - MM_frac) * dnorm(OJ$PriceDiff, CH_mean, CH_sd))

summary(LDA_pred)

# Naive Bayes with Package
nb_mod_2 <- naiveBayes(data = heart, AHD ~ MaxHR + Age)

# contour plot
Ht_NB_contour <- data.frame('MaxHR' = numeric(0), 'Age' = numeric())
for(i in min(heart$MaxHR):max(heart$MaxHR)){
  for(j in min(heart$Age):max(heart$Age)){
    Ht_NB_contour[nrow(Ht_NB_contour)+1,]$MaxHR <- i
    Ht_NB_contour[nrow(Ht_NB_contour),]$Age <- j
  }
}
Ht_NB_contour <- cbind(Ht_NB_contour, predict(nb_mod_2, Ht_NB_contour, type = "raw"))
ggplot(data = Ht_NB_contour, aes(x = MaxHR, y = Age, z = Yes)) + geom_contour(aes(color = factor(..level..)))