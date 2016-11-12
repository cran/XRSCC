\name{clothes}
\alias{clothes}
\docType{data}
\title{
Defective number per sample
}
\description{
The data give a defectives number in a clothes process
}
\usage{data(clothes)}
\format{
  A data frame with 90 observations on the following variable.
  \describe{
    \item{\code{c}}{a numeric vector of integer number of nonconformities in a sample}
  }
}
\examples{
require(XRSCC)
data(clothes)
c_gr(clothes)
}
\keyword{datasets}
\keyword{defectives}
