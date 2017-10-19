f<- function(x)
{
  
  total_num <- 0
  for(i in 1:length(x))
  {
    total_num <- total_num + x[i]
  }
  mean_value <- total_num / length(x)
  print(mean_value)
  total_num <- 0
  for(i in 1:length(x))
  {

    total_num <- (total_num + abs(x[i] - mean_value)) 
  }
  return(total_num/ length(x))
}


f(1:5)
f(c(7,2,2,1,-4))


#visualize 
new_x <- c()
for (i in 1:200) {
  new_x[i] <- f(rnorm(i))
}
library(ggplot2)
qplot(x=1:200, y=new_x, geom = "line", ylim = c(0.5,1))

#question 2

hilbert <- function(i,j)
{

  mat <- matrix(c(0), nrow = i, ncol = j)
  
  for (i in 1:i) 
  {
    for (j in 1:j) {
      mat[i,j] <- (1 / (i + j - 1))
    }
  }
  mat
}


h<- hilbert(5,5)

Ht <- det(t(h) %*% h)
Ht

#testhat part will not implemented

#question

