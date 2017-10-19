

'%+%' <- function(a,b)
{
  print(a)
  print(b)
}


"hello" %+% "world"




factorial = function(n)
{
  if (n == 0) 1 else n * factorial(n - 1)
}

factorial(5)



counter_fac <- function()
{
  i <- 0
  nested_fn <- function(n)
  {
    i<<- i+n
    print(i)
  }
}

a<- counter_fac()
a(5)

z<- environment(a)
c<-environment(a)
c$i <- 0
proc.time()


library(memoise)
