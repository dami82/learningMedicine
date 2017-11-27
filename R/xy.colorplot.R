xy.colorplot <-
function(imputed.data, x, y, ...) {
  imp.ave <- imputed.data
  X <- imp.ave$data[,x]
  Y <- imp.ave$data[,y]
  my.class <- sapply(1:length(X), (function(i){
    A = is.na(X)[i]
    B = is.na(Y)[i]
    A+B
  }))
  # 
  DF <- mice::complete(imp.ave)
  subDF <- DF[,c(x,y)]
  plot(subDF,
       col = c("gray75","orange","red2")[(1+my.class)],
       ... = ...)
}
