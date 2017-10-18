#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)


library(ggplot2)
gen_shape<- function()
{
  t <- seq(from = 1, to = 2 * pi , by = 0.1)
  x_v <- 16 * (sin(t)^3)
  y_v <- 13 * cos(t) -5 * cos(2*t) - 2* cos(3* t) - cos(4*t)
  return (data.frame(t=t, x=x_v, y=y_v))
}


df <- gen_shape()

ggplot(df , aes(x= x , y = y), col= "red") + geom_polygon()


library(testthat)

test_that("gen_shape()",{
  df <- gen_shape()
  expect_match(class(df),"data.frame")
  t <- seq(from = 1, to = 2 * pi , by = 0.1)
  x_v <- 16 * (sin(t)^3)
  y_v <- 13 * cos(t) -5 * cos(2*t) - 2* cos(3* t) - cos(4*t)
  expect_equal(df$t,t)
  expect_equal(df$x, x_v)
  expect_equal(df$y, y_v)
  
})



# question 3

nuclear_plant_factory <- function()
{
  heat = 0
  in_meltdown = FALSE
  power_plant <- function( watts )
  {
    if(in_meltdown == FALSE)
    {
      draw = rpois(1, lambda = heat)
      if( draw > 11 )
      {
        print("Warning: Reactor is in MELTDOWN!")
        in_meltdown <<- TRUE
        return(invisible(heat))
      }
      
      if(heat < 10 )
      {
        heat <<- heat + watts
        print("Extracting more power")
      }
      else
      {
        print("Warning: Reactor overheated, cannot extract more power")
        heat <<- heat -2
      }
    }
    else
    {
      print("Warning: Reactor is in MELTDOWN!")
      return(invisible(heat))
    }
    invisible(heat)
  }
}

set.seed(4712)
power_plant <- nuclear_plant_factory()
power_plant(5)



save_plant <- function(power_plant)
{
  ref <- environment(power_plant)
  ref$heat <- 0
  ref$in_meltdown <- FALSE
  print("Yay, Scorpion has saved the reactor!")
}


save_plant(power_plant)



#question 2 with s3 
build_city <- function(name, nr_region)
{
  city_ <- list(name=name, bombings=data.frame(id=1:nr_region, hits=numeric(nr_region)))
  class(city_) <- "city"
  return(city_)
}

city_obj<- build_city("london",576)

simulate_bombings.city <- function(city, number_of_raids)
{
  UseMethod()
  for (i in 1:number_of_raids) {
    for (j in 1:nrow(city$bombings)) {
      city$bombings$hits[j] <- city$bombings$hits[j] + rpois(1,lambda = 0.93) 
    }
  }
  return(city)
}

print = function(city) {
  UseMethod("print", city)
}
a<-print.abc(city_obj,34)


account <-  setRefClass("account", fields = list(balance="numeric"), methods = list(
  withdraw = function(x)
  {
    balance <<- balance - x
  },
  deposite = function(x)
  {
    balance <<- balance + x
  }
))


newaccount_type <- setRefClass("newaccount_type", contains = "account", methods = list(
  withdraw = function(x)
  {
    if(balance < x)
    {
      print("you cannot with draw")
    }
    else
    {
      balance <<- balance - x
    }
  }
))

a<- newaccount_type$new(balance = 100)
a$balance
a$withdraw(20)
a$balance

b <- a
a$balance
b$balance

new

a$balance
account
RnG <- setRefClass("RnG", fields = list(), methods = list())

beta<- setRefClass("beta", fields = list(),methods = list(), contains = "RnG")


df2 <- data.frame(id=1:100, x=seq(1,100,by= 1), y=rnorm(1:100))

ggplot(df) + geom_line(aes(x = x , y = y)) + geom_line(data= df2, aes(x=x,y=y))


Account2 <- set("Account",
                        public = list(
                          initialize = function(balance = 0){
                            private$balance = balance
                          },
                          withdraw = function(x){
                            if (private$balance < x) stop("Not enough money")
                            private$balance <- private$balance - x
                          },
                          deposit = function(x) {
                            private$balance <- private$balance + x
                          }
                        ),
                        private = list(
                          balance = NULL
                        )
)

a<-Account2$new(4)


a<- c(1:10)
result<- sapply(a, function(x) {
      xx<- 0
      xx <- xx + x
      xx
    }
)
result
