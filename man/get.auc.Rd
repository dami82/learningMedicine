\name{get.auc}
\alias{get.auc}
\title{
Compute Area Under Curve
}
\description{
Compute the Area Under the curve. Useful for dettermining AUC of ROC curves or probability distributions.
}
\usage{
get.auc(y, x = NULL)
}
\arguments{
  \item{y}{
value of the points on the y-axis
}
  \item{x}{
value of the points on the x-axis
}
}
\examples{
y1 <- c(0, 0.6, 0.8, 0.82, 0.87, 0.92, 1)
x1 <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1)
plot(x1, y1, type = 'l', lwd = 2, col = 'red2', xlim = c(-.3, 1.3))
abline(coef = c(0, 1), lty = 2, col = "gray80")
get.auc(y = y1, x = x1)
}
