
clipped_normal <- function(limit = 200, given_mean = 5, given_sd = 2){
  i <- 0
  j<- 1
  result_v <- c()
  while(i < limit)
  {
    result_v[j] = rnorm(1,given_mean, given_sd)

    j<- j+1
    if(result_v[j]  > 2* given_sd)
    {
      i <- i +1
    }
  }

  return(result_v)
}


set.seed(4711)
given_sd <- 2
given_mean <- 5
clipped <- clipped_normal(200,given_mean,given_sd);

sum(clipped > (2*given_sd))
length(clipped)
df <- data.frame(a=clipped)
ggplot(df, aes(x=a)) + geom_histogram(bins = 15) 


