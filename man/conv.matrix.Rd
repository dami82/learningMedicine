\name{conv.matrix}
\alias{conv.matrix}
\title{Convolute a Numeric Image}
\description{Perform Convolution on a numeric matrix}
\usage{
conv.matrix(image.data, kernel.mat, hist.correct = TRUE)
}
\arguments{
  \item{image.data}{is a numeric matrix}
  \item{kernel.mat}{is the kernel matrix}
  \item{hist.correct}{normalize signal; this is based on the signal intensity distribution}
}
\examples{
par(mfrow = c(1,2))
img.1 <- do.call(cbind, lapply(c(0,5,0), function(i) 
  matrix(rnorm(2700,i), ncol = 30)))
kern <- matrix(seq(-12, 12, length.out = 9), ncol = 3)
img.matrix(img.1)
img.c <- conv.matrix(image.data = img.1, 
                     kernel.mat = kern, 
                     hist.correct = FALSE)
img.matrix(img.c)
par(mfrow = c(1,1))
}
