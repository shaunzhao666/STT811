# k-means clustering

numclus <- 3
maxiter <- 10
iris_sc <- scale(iris[,1:4])
n <- length(iris$Sepal.Length)
row_samp <- sample(1:n, numclus, replace = FALSE)
clus_loc <- iris_sc[row_samp,]
clus_dist <- matrix(rep(0, n*numclus), nrow = n, ncol = numclus)
clus_pick <- rep(0, n)
clus_pick_old <- rep(0, n)
for(k in 1:maxiter){
  for(i in 1:n){
    for(j in 1:numclus)
    {
      clus_dist[i,j] <- sqrt(sum((iris_sc[i,1:4] - clus_loc[j,1:4])^2))
    }
    clus_pick[i] = which.min(clus_dist[i,])
  }
  clus_loc_new <- aggregate(iris_sc[,1:4], list(clus_pick), FUN=mean) 
  clus_loc <-clus_loc_new[2:5]
  #  if(clus_pick == clus_pick_old){
  #    break
  #    }
  #  clus_pick = clus_pick_old
}
iris_clus <- cbind(clus_pick, iris)
ggpairs(iris_clus, mapping = ggplot2::aes(color=as.factor(clus_pick)))


# k-means with penguins

numclus <- 3
maxiter <- 10
penguin_sc <- scale(penguins[,2:5])
row_samp <- sample(1:length(penguins$bill_length_mm), numclus, replace = FALSE)
clus_loc <- penguin_sc[row_samp,1:4]
clus_dist <- matrix(rep(0, length(penguins$bill_length_mm)*numclus), nrow = length(penguins$bill_length_mm), ncol = numclus)
clus_pick <- rep(0,length(penguins$bill_length_mm))
clus_pick_old <- rep(0,length(penguins$bill_length_mm))
for(k in 1:maxiter){
  for(i in 1:length(penguins$bill_length_mm)){
    for(j in 1:numclus)
    {
      clus_dist[i,j] <- sqrt(sum((penguin_sc[i,1:4] - clus_loc[j,1:4])^2))
    }
    clus_pick[i] = which.min(clus_dist[i,])
  }
  clus_loc_new <- aggregate(penguin_sc[,1:4], list(clus_pick), FUN=mean) 
  #  clus_loc <-clus_loc_new[1:4]
  #  if(clus_pick == clus_pick_old){
  #    break
  #    }
  #  clus_pick = clus_pick_old
}
pen_clus <- cbind(clus_pick, penguin_sc)
ggpairs(penguins, mapping = ggplot2::aes(color = as.factor(clus_pick)))

# k-means clustering with pizza
numclus <- 9
maxiter <- 10
pizza_sc <- scale(pizza[3:8])
row_samp <- sample(1:length(pizza$mois), numclus, replace = FALSE)
clus_loc <- pizza_sc[row_samp,]
clus_dist <- matrix(rep(0, length(pizza$fat)*numclus), nrow = length(pizza$cal), ncol = numclus)
clus_pick <- rep(0,length(pizza$prot))
clus_pick_old <- rep(0,length(pizza$prot))
for(k in 1:maxiter){
  for(i in 1:length(pizza$mois)){
    for(j in 1:numclus)
    {
      clus_dist[i,j] <- sqrt(sum((pizza_sc[i,] - clus_loc[j,1:6])^2))
    }
    clus_pick[i] = which.min(clus_dist[i,])
  }
  clus_loc_new <- aggregate(pizza[,2:7], list(clus_pick), FUN=mean) 
  #  clus_loc <-clus_loc_new[2:5]
  #  if(clus_pick == clus_pick_old){
  #    break
  #    }
  #  clus_pick = clus_pick_old
}
pizza_clus <- cbind(clus_pick, pizza)
ggpairs(pizza, mapping = ggplot2::aes(color = as.factor(clus_pick)))

# kmeans function in R
Weekly_sc <- scale(Weekly[,2:6])
weekly_km <- kmeans(Weekly_sc, centers = 4, nstart = 25)
