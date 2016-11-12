\name{bottles}
\alias{bottles}
\docType{data}
\title{
Defetive bottles sample
}
\description{
The data give the number of defective bottles in a fixed sample size
}
\usage{data(bottles)}
\format{
  A data frame with 80 observations on the following variable.
  \describe{
    \item{\code{D}}{a numeric vector of integer number of defective bottles}
  }
}
\examples{
data(bottles)
require(XRSCC)
p_gr(bottles, n=100)
}
\keyword{datasets}
\keyword{defective}
