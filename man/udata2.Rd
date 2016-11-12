\name{udata2}
\alias{udata2}
\docType{data}
\title{
Defective number per unit
}
\description{
The data give a nonconformities number on a clothes manufacturing process, the sample size is fixed.
}
\usage{data(udata2)}
\format{
  A data frame with 90 observations and two variables.
  \describe{
    \item{\code{d}}{a numeric vector of integer number of nonconformities in a sample}
    \item{\code{n}}{a numeric vector of sample size}
  }
}
\examples{
require(XRSCC)
data(udata2)
u_gr(udata2)
}
\keyword{datasets}
\keyword{attributes}
