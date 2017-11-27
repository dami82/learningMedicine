vector.toPic <-
function(vec.data, 
                         incl.elems = NULL, 
                         pic.dim, 
                         plot.img = F, 
                         main = '') {
  #
  if (!is.null(incl.elems)) {
    vct <- rep(0, length(incl.elems))
    vct[!incl.elems] <- 0
    vct[incl.elems] <- vec.data
  } else {
    vct <- vec.data
  }
  if (length(vct) == pic.dim[1] * pic.dim[2]) {
    img <- matrix(vct, nrow = pic.dim[1], ncol = pic.dim[2])
    if (plot.img)
      img.matrix(img, negative = T, main = main)
    img
  } else {
    NULL
  }
}
