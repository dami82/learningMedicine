expand.matrix <-
function(image.data, 
                          struct.elem = matrix(1, nrow = 5, ncol = 5)) {
  #
  full.dims <- dim(image.data)
  struct.dims <- dim(struct.elem)
  if(!sum((struct.dims %% 2) == 1) == 2)
    stop("Bad structuring element")
  #
  half.extra.room.rw <- (struct.dims[1] - 1) / 2 
  half.extra.room.cl <- (struct.dims[2] - 1) / 2
  #
  out.mat <- matrix(0, 
                    nrow = (full.dims[1] + (2 * half.extra.room.rw)),
                    ncol = (full.dims[2] + (2 * half.extra.room.cl)))
  #
  for(ri in 1:full.dims[1]) {
    for(ci in 1:full.dims[2]) {
      if (image.data[ri, ci] == struct.elem[(half.extra.room.rw + 1), 
                                            (half.extra.room.cl + 1)]) {
        #
        ri.01 <- ri
        ri.02 <- ri + (2 * half.extra.room.rw)
        ci.01 <- ci
        ci.02 <- ci + (2 * half.extra.room.cl)  
        #
        out.mat[ri.01:ri.02, ci.01:ci.02] <- 
          out.mat[ri.01:ri.02, ci.01:ci.02] + struct.elem
        #
      }  
    }
  }
  out.mat[out.mat > 0] <- 1
  #
  out.mat <- out.mat[(1 + half.extra.room.rw):
                       (nrow(out.mat) - half.extra.room.rw), 
                     (1 + half.extra.room.cl) : 
                       (ncol(out.mat) - half.extra.room.cl)]
  out.mat
}
