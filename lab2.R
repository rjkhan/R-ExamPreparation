rm(list=ls())
name <- "Rabnawaz Jansher"
liuid <- "rabsh696"

library(markmyassignment)
# Set assignment Path 
lab_path <- "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml"
set_assignment(lab_path)


#1.1.1
sheldon_game <- function(player1, player2)
{
  alt <- c("rock", "lizard", "spock", "scissors", "paper") 
  stopifnot(player1 %in% alt, player2 %in% alt)
  alt1 <- which(alt %in% player1)
  alt2 <- which(alt %in% player2)
  
  if(alt1 == alt2)
    return ("Draw!")
  
  if(any(alt1+c(1,3) %% 5== alt2)) 
    return("Player 1 wins!")
  else 
    return("Player 2 wins!")
}

sheldon_game("spock","scissors")
mark_my_assignment(tasks = "sheldon_game")

#1.2.1
my_moving_median<- function(x, n, ...)
{
  
  if(!is.vector(x) || !is.numeric(x) || !is.numeric(n) || is.integer(n) )
    stop()
  
  vector_length <- length(x)
  new_vector <- NA
  for(iterate in 1:vector_length)
  {
    if(vector_length >=  (iterate + n ) )
      new_vector[iterate] <- ifelse( !missing(...) , 
                                     median(x[iterate:(iterate + n)],na.rm = ...),
                                     median(x[iterate:(iterate + n)]))
  }
  new_vector
}

my_moving_median(c(5,1,2,NA,2,5,6,8,9,9), 2, na.rm=TRUE)
mark_my_assignment(tasks = "my_moving_median")

#1.2.2
for_mult_table<-function(from,to)
{
  if(length(from)!=1 || length(to) != 1)
    stop()
  
  iterator <-  from:to
  #creating a matrix
  multiplication_table <- matrix(c(from:to),nrow = (to-from)+1, ncol = (to-from)+1 )
  i<- 1; j<-1 # i and j index
  for(mrow in iterator) #row iterator to matrix length
  {
    for(mcol in iterator) #col iterator to matrix length
    {
      multiplication_table [i,j]<- mrow * mcol
      j<- j +1
    }
    i<-i+1
    j<-1
  }
  multiplication_table
}

for_mult_table(1,10)
mark_my_assignment(tasks = "for_mult_table")


#1.3.1
find_cumsum<- function(x, find_sum)
{
  total_sum<-0;  j<-1; result<-0 #variable initilize
  if(!(is.vector(x)) || !(is.numeric(find_sum) || !(is.numeric(x) )) || length(find_sum) != 1 )
    stop()
  
  while(total_sum < find_sum && length(x) >= j)  #while loop untill sum greater than find_sum and length of vector greater than j Iterator
  {
    total_sum<- total_sum+x[j]
    result<- total_sum
    j<- j+1
  }
  result
}

find_cumsum(x=1,find_sum=1000)
mark_my_assignment(tasks = "find_cumsum")

#1.3.2
while_mult_table<-function(from, to)
{
  stopifnot(length(from) == 1 , length(to) == 1)
  
  multiplication_matrix<- matrix(c(from:to),nrow = (to-from)+1,ncol = (to-from)+1 )
  i<-1; j<-1 #i and j index
  temp_copy_from = from
  
  while(temp_copy_from <= to)
  {
    f<-from
    while(f<=to)
    {
      multiplication_matrix[i,j]<- temp_copy_from*f
      j<- j +1; f<-f+1
    }
    temp_copy_from<- temp_copy_from+1
    i<-i+1; j<-1
  }
  multiplication_matrix
}
while_mult_table(10,12)
mark_my_assignment(tasks = "while_mult_table")


#1.4.1
repeat_find_cumsum<- function(x, find_sum)
{
  total_sum<-0; j<-1; result<-0
  
  if(!(is.vector(x)) || !(is.numeric(find_sum) || !(is.numeric(x) )) || length(find_sum) != 1 )
    stop()
  j<- length(x)
  iterator <- 1
  repeat
  {
    if(total_sum > find_sum || j <= 0)
      break
    else
    {
      total_sum <- total_sum + x[iterator]
      result<- total_sum
      j<- j-1
      iterator<- iterator+1
    }
    
  }
  result
  
}

repeat_find_cumsum(x=1:100,find_sum=500)
mark_my_assignment(tasks = "repeat_find_cumsum")

#.4.2
repeat_my_moving_median<- function(x, n, ...)
{
  vector_length <- length(x)
  new_vector <- NA
  iterate<-1
  
  if(!is.vector(x) || !is.numeric(x) || !is.numeric(n) || is.integer(n) )
    stop()
  repeat
  {
    if(iterate < vector_length)
    {
      if(vector_length >= (iterate+n))
      {
        new_vector[iterate] <- ifelse( !missing(...) , median(x[iterate:(iterate + n)],na.rm = ...), median(x[iterate:(iterate + n)]))
      }
      iterate<-iterate+1
    }
    else
      break
  }
  return(new_vector)
}
repeat_my_moving_median(x= 1:10,n=2)
mark_my_assignment(tasks = "repeat_my_moving_median")


#1.5.1
in_environment<-function(env)
{
  v_result <- ls(env)
  v_result
}

env <- search()[length(search())]
env
funs<- in_environment(env)
funs[1:5]
mark_my_assignment(tasks = "in_environment")

#1.6.1
cov<- function(X)
{
  if(!is.data.frame(X))
  {
    stop()
  }
  v_result<- NA
  column_name <- names(X)
  
  for(y in column_name)
  {
    mean_value<- lapply(X[y],function(y){mean(y)})
    sd_value<- lapply(X[y],function(y){sd(y)})
    
    v_result[y]<- (as.numeric(unlist(sd_value))/as.numeric(unlist(mean_value)))
  }
  v_result<- v_result[!is.na(v_result)]
  v_result
}

data(iris)
cov(iris[3:4])
mark_my_assignment(tasks = "cov")



#1.7.1
moment <- function(i) {
  
  if(!(is.numeric(i))   )
    stop()
  
  result <- function(x) 
  {
    expected_mean <- x-mean(x)
    mean((expected_mean)^i)
  }
  result
}

m1 <- moment(1)
m2 <- moment(2)
m1(1:100)
m2(1:100)
mark_my_assignment(tasks = "moment")