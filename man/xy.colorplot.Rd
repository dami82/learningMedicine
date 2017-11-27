\name{xy.colorplot}
\alias{xy.colorplot}
\title{Visualize Imputed Data}
\description{Visualize a scatterplot (xy plot) or thw numeric variables that were imputed using *mice* }
\usage{
xy.colorplot(imputed.data, x, y, ...)
}
\arguments{
  \item{imputed.data}{
an object belonging to the mids class; typically, it's the result of a NA imputation using mice
}
  \item{x}{
name or index of the first attribute to visualize
}
  \item{y}{
name or index of the second attribute to visualize
}
  \item{\dots}{
optional arguments to pass to the plot function, succh as main, xlims and so on
}
}
\examples{
require(mice)
miss.data <- data.frame(sapply(c('x1', 'x2'), function(i) rnorm(100)))
miss.data[1:15, 1] <- NA
imp.data <- mice::mice(miss.data)
xy.colorplot(imp.data, x = 'x1', y = 'x2', main= "Imputed Data Points", pch = 19)
}
