\name{img.matrix}
\alias{img.matrix}
\title{
Visualize a Numeric Image
}
\description{
Visualize a Numeric matrix, corresponding to a digital image
}
\usage{
img.matrix(image.data, negative = FALSE, col.vect = NULL, main = "")
}
\arguments{
  \item{image.data}{numeric matrix}
  \item{negative}{boolean, invert colors}
  \item{col.vect}{set colors for the visualization}
  \item{main}{Title of the image}
}
\examples{
my.mat <- matrix(sample(0:1, 15000, TRUE, c(0.95, 0.05)), 
                 ncol = 150, 
                 nrow = 100)
my.mat[25:95, 20:85] <- 0.5
my.mat[55:90, 25:65] <- 1
img.matrix(my.mat, col.vect=c("gray90", "orange", "red2"))
}