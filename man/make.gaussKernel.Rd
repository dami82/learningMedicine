\name{make.gaussKernel}
\alias{make.gaussKernel}
\title{Create a Gaussian Kernel Matrix}
\description{
Generates a Matrix corresponding to a Gaussian Kernel for convolution}
\usage{
make.gaussKernel(n = 5, A = 1, sigma = 1)
}
\arguments{
  \item{n}{length in pixels of the kernel/structuring element edge}
  \item{A}{numeric, A param, default is 1}
  \item{sigma}{numeric, sigma param, default is 1}
}
\examples{
make.gaussKernel(5)
}
