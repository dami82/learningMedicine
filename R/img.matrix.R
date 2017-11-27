img.matrix <-
function(image.data, negative = FALSE, col.vect = NULL, main = "")
{
  if (!is.matrix(image.data)) {
    stop("Data not a matrix")
  }
  n.row <- nrow(image.data)
  n.col <- ncol(image.data)
  #
  my.col = colorRampPalette(colors = c("black", "white"))(32)
  if (negative) {
    my.col <- rev(my.col)  
  } else if (!is.null(col.vect)) {
    my.col <- col.vect
  }
  image(t(image.data)[,n.row:1], col = my.col, main = main )
}
