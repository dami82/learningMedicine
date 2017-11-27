\name{expand.matrix}
\alias{expand.matrix}
\title{Dilation}
\description{Perform MM operation: Dilation}
\usage{
expand.matrix(image.data, struct.elem = matrix(1, nrow = 5, ncol = 5))
}
\arguments{
  \item{image.data}{numeric matrix, this is the numeric image}
  \item{struct.elem}{nnumeric matrix, this is the structuring element (include 0s and 1s)}
}
\examples{
my.mat <- matrix(sample(0:1, 5000, TRUE, c(0.95, 0.05)), 
                 ncol = 50, 
                 nrow = 100)
dil.mat <- expand.matrix(my.mat)
par(mfrow = c(1,2))
img.matrix(my.mat)
img.matrix(dil.mat)
par(mfrow = c(1,1))
}
