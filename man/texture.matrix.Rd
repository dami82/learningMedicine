\name{texture.matrix}
\alias{texture.matrix}
\title{Texture Analysis
}
\description{Perform Txture Analysis
}
\usage{
texture.matrix(pixel.matrix, 
               threshold = 0.4, 
               direct = "y", 
               sapcer.lim = c(5, 15), 
               spacer.min.void = 0.8, 
               na.semplif = FALSE)
}
\arguments{
  \item{pixel.matrix}{numeric matrix, image}
  \item{threshold}{pixel intensity for thresholding}
  \item{direct}{c("x", "y", "xy")}
  \item{sapcer.lim}{numeric vector of length 2, min and max length for the spacer (non-signal)}
  \item{spacer.min.void}{minimum fraction of non-signal to prevent stopping of the void count}
  \item{na.semplif}{semplification by converting to NA}
}
\examples{
set.seed(123)
my.mat <- do.call(cbind, lapply(1:100, function(i) 
  { tmp <- rep(sample(0:1, 1, prob = c(0.8,0.2)), 20);
    cbind(tmp, tmp, tmp, tmp, tmp)
  }))
par(mfrow = c(1,2))
img.matrix(my.mat)
my.seq <- seq(1, 25, by = 3)
my.txtr <- sapply(my.seq, function(i) texture.matrix(my.mat, 0.5, "x", c(i, i+2)))
plot(my.txtr ~ my.seq, type = "o", col = "red2", pch = 19)
par(mfrow = c(1,1))
}
