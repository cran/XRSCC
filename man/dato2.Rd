\name{dato2}
\alias{dato2}
\docType{data}
\title{
The piston hole length in mm
}
\description{
A sample containing piston hole length in mm
}
\usage{data(dato2)}
\format{
  A data frame with 45 subgroup of 5 observations
  \describe{
    \item{\code{n1}}{a numeric vector of length in mm}
    \item{\code{n2}}{a numeric vector of length in mm}
    \item{\code{n3}}{a numeric vector of length in mm}
    \item{\code{n4}}{a numeric vector of length in mm}
    \item{\code{n5}}{a numeric vector of length in mm}
  }
}
\examples{
data(dato2)
require(XRSCC)
results1<-xrs_gr(dato2)
results2<-X_it(results1)
results3<-R_it(results2)
}
\keyword{datasets}
