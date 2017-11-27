\name{filter.matrix}
\alias{filter.matrix}
\title{Filtering for Numeric Images}
\description{
Performs filtering, such as median filtering, on matrices corresponding to numeric images
}
\usage{
filter.matrix(image.data, square.edge = 5, filter.function = "median", hist.correct = F)
}
\arguments{
  \item{image.data}{numeric matrix, this is the numeric image}
  \item{square.edge}{numeric, length of the structuring element edge in pixels}
  \item{filter.function}{string matcching the name of the math function used for the filter}
  \item{hist.correct}{correct pixel intensity distribution}
}
\examples{
my.mat <- matrix(sample(0:1, 15000, TRUE, c(0.95, 0.05)), 
                 ncol = 150, 
                 nrow = 100)
my.mat[55:90, 25:65] <- 1
fil.mat <- filter.matrix(my.mat)
par(mfrow = c(1,2))
img.matrix(my.mat)
img.matrix(fil.mat)
par(mfrow = c(1,1))
}
