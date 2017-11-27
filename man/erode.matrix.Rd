\name{erode.matrix}
\alias{erode.matrix}
\title{
Erosion}
\description{
Perform a MM operation: Erosion
}
\usage{
erode.matrix(image.data, struct.elem = matrix(1, ncol = 3, nrow = 3), tolerance = 2)
}
\arguments{
  \item{image.data}{numeric matrix, this is the numeric image}
  \item{struct.elem}{numeric matrix, structuring element (1s or 0s)}
  \item{tolerance}{integer; defines tolerance of such operation}
}
\examples{
my.mat <- matrix(sample(0:1, 5000, TRUE, c(0.6, 0.4)), 
                 ncol = 50, 
                 nrow = 100)
ero.mat <- erode.matrix(my.mat, tolerance = 3)
par(mfrow = c(1,2))
img.matrix(my.mat)
img.matrix(ero.mat)
par(mfrow = c(1,1))
}

