texture.matrix <-
function(pixel.matrix, 
                           threshold = 0.4, 
                           direct = "y", 
                           sapcer.lim = c(5,15), 
                           spacer.min.void = 0.8,
                           na.semplif = FALSE) {
  #
  # make working matrix
  working.mat <- pixel.matrix
  working.mat[working.mat < 0] <- NA
  working.mat[working.mat <= threshold] <- 0
  working.mat[working.mat > threshold] <- 1
  #
  pat.count.matrix <- sapply(1:ncol(working.mat), (function(ci){
    sapply(1:nrow(working.mat), (function(ri){
      #
      # initialize
      out.value <- 0
      #
      # if non-NA, proceed
      cur.pix <- working.mat[ri,ci]
      if (!is.na(cur.pix) &
          cur.pix == 1) {
        #
        # handle x-direction
        if (direct[1] == "x") {
          tmp.vect <-working.mat[ri,(ci:ncol(working.mat))]
          #
          # handle y-direction
        } else if (direct[1] == "y") {
          tmp.vect <-working.mat[(ri:nrow(working.mat)), ci]
          #
          # handle yx-direction
        } else if (direct[1] == "xy" |
                   direct[1] == "yx") {
          pixRange.cl <- ci:ncol(working.mat)
          pixRange.rw <- ri:nrow(working.mat)
          tmp.vect <- sapply(1: min(length(c(pixRange.cl, pixRange.rw))), (function(i){
            working.mat[pixRange.rw[i], pixRange.cl[i] ]  
          }))
          #
          # handle issues of any kind
        } else {
          tmp.vect <- 1
        }
        #
        # Now correct and make sure about min. len
        #
        # tmp.vect[is.na(tmp.vect)] <- 0
        # ALTERNATIVE
        if (sum(is.na(tmp.vect)) > 0){
          tmp.vect <- tmp.vect[1:(min(which(is.na(tmp.vect))) - 1)]
        }
        # allow rumor, block signal that is further away
        if (length(tmp.vect) > 4) {
          for (ii in 2:((length(tmp.vect) - 2))) {
            if (sum(tmp.vect[ii:(ii+2)]) == 0) {
              break()
            }
          }
          if (length(tmp.vect) - (ii+2) > 4) {
            for (jj in (ii+3):((length(tmp.vect) - 2) )) {
              if (sum(tmp.vect[jj:(jj+2)]) == 3) {
                break()
              }
            }
            #
            # detect first zero and then zero everything.
            if (length(tmp.vect) - (jj+2) > 4) {
              for (kk in (jj+3):((length(tmp.vect) - 2))) {
                if (sum(tmp.vect[kk:(kk+2)]) == 0) {
                  tmp.vect[kk:length(tmp.vect)] <- 0
                  break()
                }
              }
            }
          }
        }
        #
        if (length(sapcer.lim) == 1)
          sapcer.lim <- rep (sapcer.lim, 2)
        #
        if (length(tmp.vect) >= (2 + min(sapcer.lim))){
          #
          #
          # handle dynamic range
          max.extent <- min(c(max(sapcer.lim), (length(tmp.vect) - 2)))
          min.extent <- min(sapcer.lim)
          #
          #
          if (min.extent <= max.extent) {
            full.extent <- seq(min.extent, max.extent, by = 1)
            #
            target.oneone <- tmp.vect[(2+full.extent)]
            if (sum(target.oneone == 1) > 0) {
              #
              # detect all possible gaps
              available.gaps <- which(target.oneone == 1)
              if (length(available.gaps) > 0) {
                #
                # we should check gap by gap
                gap.check <- sapply(available.gaps, (function(gi){
                  curr.gap <- tmp.vect[2:full.extent[(gi)] + 1] 
                  #
                  # check again and look up for zero ratios
                  my.ratio <- sum(curr.gap == 0) / length(curr.gap)
                  ifelse(my.ratio >= spacer.min.void, 1, 0)
                }))
                #
                # final eval
                if (sum(gap.check) >0) {
                  #
                  # COUNT
                  out.value <- 1
                }
                #
              }
            }
          }   
        } else {
          #
          # handle positions close to boundaries
          out.value <- NA
        }
      }
      out.value
    }))  
  }))
  #
  if (na.semplif) {
    pat.count.matrix <- pat.count.matrix[!apply(pat.count.matrix, 1, (function(rw){ sum(is.na(rw)) == length(rw) })), ]
    pat.count.matrix <- pat.count.matrix[, !apply(pat.count.matrix, 2, (function(cl){ sum(is.na(cl)) == length(cl) }))]
  }
  #
  final.texture.val <- sum(pat.count.matrix, na.rm = TRUE) / (ncol(pat.count.matrix) * nrow(pat.count.matrix))
  return(final.texture.val)
}
