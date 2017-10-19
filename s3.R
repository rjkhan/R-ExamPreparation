beta <-  setRefClass ("beta" ,
                       fields = list ( shape1 = "numeric" ,
                                           shape2 = "numeric" ) ,
                       methods = list (
                         draw_rnd = function ( ) {
                           rbeta (1 , shape1 , shape2 )
                         }
                       ) )



be =beta$new(shape33 = 2, shape44 = 3)
be$draw_rnd()


draw_rnd <- function (x , ... ) {
  x$draw_rnd()
}
draw_rnd(be)
