mask_along_x <-
function(bw.matrix, thresh.pixNum = 2) {
  #
  #
  new.mask <- lapply(1:nrow(bw.matrix), (function(i){
    #
    # initialize vect
    vect <- bw.matrix[i,]
    #
    out <- rep(0, length(vect))
    mark.pos <- which(vect > 0.5)
    if( length(mark.pos) > 2) {
      #
      tmp.pos <- mark.pos[1]
      left.pos <- mark.pos[-1]
      i.group <- 1
      bor.width <- 1
      #
      while (length(left.pos) > 0) {
        if (sum( c(tmp.pos + c(1,2,3) ) %in% left.pos) > 0) {
          out[tmp.pos] <- -1
          bor.width <- bor.width + 1  
        } else if (bor.width > thresh.pixNum){
          out[(tmp.pos + 1) : length(out)] <- i.group
          i.group <- i.group + 1
          bor.width <- 1
        } else {
          bor.width <- 1
        }
        tmp.pos <- left.pos[1]
        left.pos <- left.pos[-1]
      }  
    }
    out
  }))
  new.mask <- do.call(rbind, new.mask)
  new.mask
}
