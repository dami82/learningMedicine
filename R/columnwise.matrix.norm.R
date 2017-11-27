columnwise.matrix.norm <-
function(mat) {
  t(t(mat)/apply(mat, 2, sum))
}
