\name{vector.toPic}
\alias{vector.toPic}
\title{
Devectorize Images
}
\description{
Convert a vectorized image back to a numeric matrix. 
}
\usage{
vector.toPic(vec.data, incl.elems = NULL, pic.dim, plot.img = F, main = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec.data}{
Numeric vector
}
  \item{incl.elems}{
NULL or logical matrix specifying which elements (pixels) were includded or excluded from the vectorizeed image
}
  \item{pic.dim}{
numeric vector of length 2, defining number of rows and ccolumns of the final image
}
  \item{plot.img}{
logical: indicates if an image should be generated and visualized
}
  \item{main}{
string: Title of the image
}
}
\examples{
vect <- rnorm(n = 200, mean = 2)
guide <- matrix(TRUE, nrow = 24, ncol = 10)
guide[11:14,] <- FALSE
vector.toPic(vec.data = vect, incl.elems = guide, pic.dim = c(24, 10), plot.img = TRUE)
}
