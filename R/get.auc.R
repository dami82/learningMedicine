get.auc <-
function(y, x=NULL) {
  
  # Args check
  if (is.null(x))
    x = 1:length(y)
  if(length(y) != length(x))
    stop("Bad Input!")
  
  # split in intervals
  roll.mean <- sapply(2:length(y), (function(i){
    mean(c(y[i], y[(i-1)]))
  }))
  
  #compute areas
  auc <- sum((x[2:length(x)]-x[1:(length(x)-1)]) * roll.mean)
  return(auc)
}
