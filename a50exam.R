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

#question 3
library(tidyr)
my_tidy_titanic_data<- function()
{
  data("Titanic")
  x <- as.data.frame(as.matrix(ftable(Titanic)))
  tit_1 <- dplyr::mutate(x, tmp = rownames(x))
  tit <- tidyr::gather(data = tit_1, key = survived, value = counts, -tmp)
  tit_tidy <- tidyr::separate(data = tit, col = "tmp", into = c("class", "sex", "age"), sep = "_")
  return(tit_tidy)
}


tita<- my_tidy_titanic_data()

aggregate_away_sex <- function(tita)
{
  
  as.data.frame(summarise(group_by(tita, class, age, survived), counts = sum(counts)))
}

aggregate_away_sex(tita)
head(aggregate_away_sex(tita))






#question
counter_factory <- function(start, max)
{
  i <- start
  function(){
    if(i < max) i <<- i + 1
    res <- c(i, max)
    a<- setRefClass("counter", res, method= list())
    a
  }
}
cnt <- counter_factory(start = 2, max = 4)
cnt$methods(summary = function(){
  print(res)
})
class(cnt)


sample(1:6,2)


inverse_triangular_block_matrix <- function(a,b,c)
{
  A<-  solve(a)
  C<- solve(c)
  B <- solve(-a) %*% b %*% C 
  print(A)
  print(B)
  print(C)
}

inverse_triangular_block_matrix(diag(2), 2*diag(2), 3*diag(2))

