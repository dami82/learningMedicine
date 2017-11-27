create_mask <-
function(bw.matrix, 
                        method = "xy", 
                        thresh.pixNum = 2, 
                        aggr.fun = "max",
                        aggr.edge = 5, 
                        aggr.precise = TRUE) {
  #
  # initialize return variable
  out.data <- NA
  #
  # along x
  if (grepl("x", method)[1]) {
    x1.mat <- mask_along_x(bw.matrix, 
                           thresh.pixNum = thresh.pixNum) 
    x2.mat <- mask_along_x(bw.matrix[,ncol(bw.matrix):1], 
                           thresh.pixNum = thresh.pixNum)[,ncol(bw.matrix):1] 
    x.mat <- sapply(1:ncol(bw.matrix), (function(ci){
      sapply(1:nrow(bw.matrix), (function(ri){
        min(x1.mat[ri,ci], x2.mat[ri,ci], na.rm = TRUE)  
      }))
    }))
    #
    out.data <- x.mat
  }
  #
  # along y
  if (grepl("y", method)[1]) {
    tr.mat <- t(bw.matrix)
    y1.mat <- t(mask_along_x(tr.mat, 
                             thresh.pixNum = thresh.pixNum) )
    y2.mat <- t(mask_along_x(tr.mat[,ncol(tr.mat):1], 
                             thresh.pixNum = thresh.pixNum)[,ncol(tr.mat):1]) 
    y.mat <- sapply(1:ncol(bw.matrix), (function(ci){
      sapply(1:nrow(bw.matrix), (function(ri){
        min(y1.mat[ri,ci], y2.mat[ri,ci], na.rm = TRUE)  
      }))
    }))
    #
    out.data <- y.mat
  }
  #
  #
  if (grepl("(xy)|(yx)", method)[1]) {
    #
    # handle aggregation parameters
    if ((aggr.edge %% 2) != 1){
      aggr.edge <- aggr.edge + 1
    }
    aggr.rad <- (aggr.edge - 1) / 2
    #
    # handle agggregation function 
    run.fun <- tryCatch({get(aggr.fun)}, error = (function(e) {
      message('custom function not found, applying "median"...')
      get("median")
    }))
    #
    # expand matrices
    x.mat <- cbind(matrix(0, nrow = nrow(x.mat), ncol = aggr.rad), 
                   x.mat, 
                   matrix(0, nrow = nrow(x.mat), ncol = aggr.rad))
    x.mat <- rbind(matrix(0, nrow = aggr.rad, ncol = ncol(x.mat)), 
                   x.mat, 
                   matrix(0, nrow = aggr.rad, ncol = ncol(x.mat)))
    #
    y.mat <- cbind(matrix(0, nrow = nrow(y.mat), ncol = aggr.rad), 
                   y.mat, 
                   matrix(0, nrow = nrow(y.mat), ncol = aggr.rad))
    y.mat <- rbind(matrix(0, nrow = aggr.rad, ncol = ncol(y.mat)), 
                   y.mat, 
                   matrix(0, nrow = aggr.rad, ncol = ncol(y.mat)))
    #
    # execute
    out.data <- sapply((1 + aggr.rad): (ncol(x.mat) - aggr.rad), (function(ci){
      sapply((1 + aggr.rad): (nrow(x.mat) - aggr.rad), (function(ri){
        #
        # handle precision
        if (aggr.precise & ( x.mat[ri,ci] == (-1) | y.mat[ri,ci] == (-1))) {
          (-1)
        } else {
          tmp.X <- x.mat[(ri-aggr.rad) : (ri+aggr.rad),
                         (ci-aggr.rad) : (ci+aggr.rad)]
          tmp.Y <- y.mat[(ri-aggr.rad) : (ri+aggr.rad),
                         (ci-aggr.rad) : (ci+aggr.rad)]
          
          run.fun(c(as.vector(tmp.X), as.vector(tmp.Y)))
        }
      }))
    }))
  }
  return(out.data)
}
