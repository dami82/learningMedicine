\name{columnwise.matrix.norm}
\alias{columnwise.matrix.norm}
\title{Columnwise Matrix Normalization}
\description{Normalize a numeric matrix so that all columns sum up to unity. Requires anumeric matrix as input}
\usage{
columnwise.matrix.norm(mat)
}
\arguments{
  \item{mat}{numeric matrix (or numeric data.frame)}
}
\examples{
my.mat <- sapply(1:8, function(i) sample(1:100, 15))
my.mat
columnwise.matrix.norm(my.mat)
}
