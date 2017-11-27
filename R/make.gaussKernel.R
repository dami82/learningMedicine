make.gaussKernel <-
function(n = 5, A = 1, sigma = 1) {
  #
  # check n, has to be a positive odd number > 2
  n <- as.integer(n)
  if ( n < 2 | 
       (n-1) %% 2 != 0)
    stop("n has to be a positive odd number >= 3")
  #
  #
  semi.n <- (n-1) / 2
  x.range <- (-1 * semi.n) : semi.n
  y.range <- (-1 * semi.n) : semi.n
  #
  #
  x0 <- 0; y0 <- 0
  #
  # do!
  sapply(y.range, (function(y){
    sapply(x.range, (function(x){
      A * exp (-( (((x - x0)^2) / 2 * (sigma ^2) )  + (((y - y0)^2) / 2 * (sigma ^2) )) )
    }))
  }))
}
