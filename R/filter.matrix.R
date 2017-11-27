filter.matrix <-
function(image.data, square.edge = 5, filter.function = "median", hist.correct = F) {
  #
  # make sure both are matrices
  if (! (is.matrix(image.data) & is.numeric(square.edge))) {
    stop("Non matrix argument")
  }
  #
  if (!exists(filter.function)) {
    stop("Unrecognized Function")
  }
  my.function <- get(filter.function)
  #
  # make sure kernel is much smaller than image
  if (square.edge[1] > (nrow(image.data) / 20) |
      square.edge[1] > (ncol(image.data) / 20)) {
    stop("Kernel not suitable")
  }
  #
  dim.kernel <- rep(square.edge[1], 2)
  dim.image <- dim(image.data)
  #
  # filter
  output <- do.call(rbind, lapply(1:(1 + dim.image[1] - dim.kernel[1]), (function(ri){
    sapply(1:(1 + dim.image[2] - dim.kernel[2]), (function(ci){
      # subset
      tmp <- image.data[ri : (ri + dim.kernel[1] - 1),
                        ci : (ci + dim.kernel[2] - 1)]
      out <- tryCatch({my.function(tmp)}, error = function(e) {NA})
      if (is.na(out))
        stop("An error occurred")
      out
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
