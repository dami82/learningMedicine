\name{create_mask}
\alias{create_mask}
\title{Create a Mask for Image Segmentation}
\description{Builds a Mask based on edgges used for selection of Objects/regions in a Numeric Image}
\usage{
create_mask(bw.matrix, 
            method = "xy", 
            thresh.pixNum = 2, 
            aggr.fun = "max", 
            aggr.edge = 5, 
            aggr.precise = TRUE)
}
\arguments{
  \item{bw.matrix}{numeric image}
  \item{method}{Method used for mask generation. c("x", "y", "xy")}
  \item{thresh.pixNum}{signal threshold for generating a binary image}
  \item{aggr.fun}{mathematical function for combining edge-boundary crossing from one or the other side of the figure}
  \item{aggr.edge}{minimum width of a edge/contour in pixels}
  \item{aggr.precise}{boolean: defines tolerance in edge/contour detection}
}
\examples{
img.1 <- do.call(cbind, lapply(c(0,8,0,8), function(i) 
  matrix(rnorm(2700,i), ncol = 30)))
kern <- matrix(seq(-12, 12, length.out = 9), ncol = 3)
img.c <- conv.matrix(image.data = img.1, 
                     kernel.mat = kern, 
                     hist.correct = TRUE)
img.msk <- create_mask(bw.matrix = img.c, method = 'x', aggr.edge = 3) 
img.matrix(img.msk>=3)
}
