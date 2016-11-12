\name{vol_sample}
\alias{vol_sample}
\docType{data}
\title{
Volume in ml
}
\description{
A volume sample in milliliters
}
\usage{data(vol_sample)}
\format{
  A data frame with 100 subgroup of five observations
  \describe{
    \item{\code{n1}}{a numeric vector of volume}
    \item{\code{n2}}{a numeric vector of volume}
    \item{\code{n3}}{a numeric vector of volume}
    \item{\code{n4}}{a numeric vector of volume}
    \item{\code{n5}}{a numeric vector of volume}
  }
}
\examples{
data(vol_sample)
require(XRSCC)
xrs_gr(vol_sample)
}
\keyword{datasets}
\keyword{variable}
