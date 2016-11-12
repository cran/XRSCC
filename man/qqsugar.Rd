\name{qqsugar}
\alias{qqsugar}
\docType{data}
\title{
Sugar bags weights in pounds
}
\description{
A sample containing weights of sugar bags
}
\usage{data(qqsugar)}
\format{
  A data frame with 100 subgroup of ten observations
  \describe{
    \item{\code{muestra1}}{a numeric vector of weights in pounds}
    \item{\code{muestra2}}{a numeric vector of weights in pounds}
    \item{\code{muestra3}}{a numeric vector of weights in pounds}
    \item{\code{muestra4}}{a numeric vector of weights in pounds}
    \item{\code{muestra5}}{a numeric vector of weights in pounds}
    \item{\code{muestra6}}{a numeric vector of weights in pounds}
    \item{\code{muestra7}}{a numeric vector of weights in pounds}
    \item{\code{muestra8}}{a numeric vector of weights in pounds}
    \item{\code{muestra9}}{a numeric vector of weights in pounds}
    \item{\code{muestra10}}{a numeric vector of weights in pounds}
  }
}
\examples{
data(qqsugar)
require(XRSCC)
xrs_gr(qqsugar)
}
\keyword{datasets}
\keyword{variable}
