Random <- setRefClass(
  "rnd",
  fields = list(),
  methods = list()
)


norm_rnd <- setRefClass(
  "norm",
  contains = "rnd",
  fields = list(mu="numeric",sigma="numeric"),
  methods = list(
    rnorm_gen = function(mu,sigma)
      r <- rnorm(1,mu,sigma)
  )
)

beta_rnd <- setRefClass(
  "beta", 
  contains = "rnd",
  fields = list(shape1="numeric", shape2="numeric"),
  methods = list(
    rnd_beta = function()
    {
     rbeta(1,shape1,shape2)
    }
  )
  
)

print.rnd <- function (x , ... ) {
  print ( paste0 ("I  am of type : ￿" , class (x)))
}

b <- beta_rnd$new(shape1=2,shape2=3)
b$rnd_beta()
print.rnd(b)
