
cor_matrix <- function(x)
{
  col_name <- colnames(x)
  col_len <- ncol(x)
  n_rows <- nrow(x)
  
  mean_v <- c()
  median_v <- c()
  sd_v <- c()
  for (i in 1:col_len) 
  {
    data_to_v <- as.numeric(unlist(x[i]))
    #mean
    mean_v[i] <- sum(data_to_v)/nrow(x)
    # median value
    data_to_v <- sort(data_to_v, decreasing = TRUE)
    result <- 0
    for (k in 1:length(data_to_v)) 
    {
      result <- result + (data_to_v[k] - mean_v[i] )^2
    }
    
    sd_v[i] <- result / (nrow(x) - 1) 
    
  }
  print(sd_v)

}

data(iris)
cor_matrix(iris[,1:4])


t <- seq(from = 1 , to = 2 * pi , by = 0.1)
t



