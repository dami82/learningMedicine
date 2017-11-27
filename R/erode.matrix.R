erode.matrix <-
function(image.data, struct.elem = matrix(1, ncol=3, nrow=3), tolerance = 2) {
  #
  # check odd dims
  if (sum(dim(struct.elem) %% 2 == 1) < 2)
    stop("Bad sstruct element")
  # define windows, start and end
  my.dims <- dim(struct.elem)
  #
  # adjust size
  half.exp <- (my.dims - 1) / 2
  #
  add.pixels.A <- image.data[, 1]
  add.pixels.B <- image.data[, ncol(image.data)]
  block.A <- do.call(cbind, lapply(1:half.exp[2], (function(i){add.pixels.A})))
  block.B <- do.call(cbind, lapply(1:half.exp[2], (function(i){add.pixels.B})))
  #
  image.data <- cbind(block.A, 
                      image.data,
                      block.B
  )
  #
  add.pixels.A <- image.data[1, ]
  add.pixels.B <- image.data[nrow(image.data), ]
  block.A <- do.call(rbind, lapply(1:half.exp[1], (function(i){add.pixels.A})))
  block.B <- do.call(rbind, lapply(1:half.exp[1], (function(i){add.pixels.B})))
  #
  image.data <- rbind(block.A, 
                      image.data,
                      block.B
  )
  #
  out <- sapply(1:(ncol(image.data) - my.dims[2] +1), (function(ci){
    sapply(1:(nrow(image.data) - my.dims[1] + 1), (function(ri){
      #
      my.window <- image.data[ri:(ri+my.dims[1]-1),
                              ci:(ci+my.dims[2]-1)]
      #
      if (class(my.window[,1]) == "logical") {
        new.window <- matrix(1, 
                             nrow = nrow(my.window), 
                             ncol = ncol(my.window))
        new.window[!my.window] <- 0
        my.window <- new.window
      }
      if (sum(my.window == struct.elem) >= 
          (length(as.vector(struct.elem))) - tolerance) {
        1
      } else {
        0
      }
    }))  
  }))
  out
}
