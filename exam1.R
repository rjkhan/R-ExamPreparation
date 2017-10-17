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
