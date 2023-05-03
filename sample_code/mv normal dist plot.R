#3-d plot of z = 2x^2 - y^2
x_min <- -3
x_max <- 3
y_min <- -3
y_max <- 3
x <- seq(-3, 3, by = 0.1)
y <- seq(-3, 3, by = 0.1)
z <- matrix(nrow=length(x), ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    z[i,j] <- 2*x[i]^2 - y[j]^2
  }
}
contour(x,y,z)

library(mvtnorm)
min <- -3
max <- 3
var_x <- 1.5
var_y <- 1.5
cor_xy <- 0.25
Sigma <- cbind(c(var_x,cor_xy*sqrt(var_x)*sqrt(var_y)),c(cor_xy*sqrt(var_x)*sqrt(var_y),var_y))
x <- seq(min, max, by = 0.1)
y <- seq(min, max, by = 0.1)
z <- matrix(nrow=length(x), ncol=length(y))
co_df <- data.frame('x', 'y', 'z')
for (i in 1:length(x)){
  for (j in 1:length(y)){
    z[i,j] <- dmvnorm(c(x[i],y[j]),c(0,0),Sigma)
  }
}
contour(x, y, z)

co_df <- data.frame('x' = x, 'y' = y)
ggplot(co_df, aes(x = x, y = y, z = z)) + geom_contour()
df.grad <- expand.grid(x = seq(-4,4, by = 0.1),y = seq(-4,4, by = 0.1))
dens <- cbind(df.grad, z = dmvnorm(df.grad,c(0,0), Sigma))
ggplot(dens, aes(x = x, y = y, z = z)) + geom_contour_filled()

library(MASS)
plot(mvrnorm(1000, mu = c(0,0), Sigma))