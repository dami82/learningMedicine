custom.aggrStat <-
function(vect, funct = "min"){
  #
  my.funct <- get(funct)
  mid.x <- as.integer(length(vect) / 2)
  #
  v1 <- as.integer(median(vect[1:mid.x]))
  v2 <- as.integer(median(vect[(1+mid.x):length(vect)]))
  tryCatch(my.funct(v1, v2, na.rm = TRUE), error = function(e){
    my.funct(v1, v2)
  })
}
