\name{mask_along_x}
\alias{mask_along_x}
\title{Apply Masking Along Direction}
\description{
Apply Masking Along the x axis/direction
}
\usage{
mask_along_x(bw.matrix, thresh.pixNum = 2)
}
\arguments{
  \item{bw.matrix}{boolean matrix, corresponds to a binary image}
  \item{thresh.pixNum}{numer of pixels for thresholding}
}
\examples{
my.mat <- matrix(0, ncol = 50, nrow = 50)
my.mat[,c(12:16, 25:30)] <- 1
par(mfrow = c(1,2))
img.matrix(my.mat)
msk.mat <- mask_along_x(my.mat>0)
img.matrix(msk.mat)
par(mfrow = c(1,1))
}
