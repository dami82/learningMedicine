\name{custom.aggrStat}
\alias{custom.aggrStat}
\title{Custom Aggregation}
\description{
Performs a custom Aggregation, not for final user usage
}
\usage{
custom.aggrStat(vect, funct = "min")
}
\arguments{
  \item{vect}{numeric vector}
  \item{funct}{string matching the name of the math function to apply}
}
\examples{
custom.aggrStat(sample(0:5, size = 100, replace = TRUE), "min")
}
