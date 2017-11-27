conv.matrix <-
function(image.data, kernel.mat, hist.correct = TRUE) {
  #
  # make sure both are matrices
  if (! (is.matrix(image.data) & is.matrix(kernel.mat))) {
    stop("Non matrix argument")
  }
  #
  # make sure kernel is much smaller than image
  if (nrow(kernel.mat) > (nrow(image.data) / 20) |
      ncol(kernel.mat) > (ncol(image.data) / 20)) {
    stop("Kernel not suitable")
  }
  #
  dim.kernel <- dim(kernel.mat)
  dim.image <- dim(image.data)
  #
  # convolute
  output <- do.call(rbind, lapply(1:(1 + dim.image[1] - dim.kernel[1]), (function(ri){
    sapply(1:(1 + dim.image[2] - dim.kernel[2]), (function(ci){
      # subset
      tmp <- image.data[ri : (ri + dim.kernel[1] - 1),
                        ci : (ci + dim.kernel[2] - 1)]
      sum (tmp * kernel.mat)
    }))
  })))
  #
  # scale
  if (hist.correct) {
    set.thresholds <- quantile(as.vector(output), probs = c(0.05, 0.95))
    output[output < set.thresholds[1]] <- set.thresholds[1]
    output[output > set.thresholds[2]] <- set.thresholds[2]
    output <- (output - min(output)) / ( max(output) - min(output))  
  }
  #
  # return
  output
}
