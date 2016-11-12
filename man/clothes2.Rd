\name{clothes2}
\alias{clothes2}
\docType{data}
\title{
Defective number per unit
}
\description{
The data give a nonconformities number in a clothes process in a variable sample
}
\usage{data(clothes2)}
\format{
  A data frame with 90 observations and two variables.
  \describe{
    \item{\code{d}}{a numeric vector of integer number of nonconformities in a sample}
    \item{\code{n}}{a numeric vector of sample size}
  }
}
\examples{
require(XRSCC)
data(clothes2)
u_gr(clothes2)
}
\keyword{datasets}
