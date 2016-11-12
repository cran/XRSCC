\name{Beta.X}
\alias{Beta.X}
\title{
X chart OC Curve
}
\description{
Calculates and plots the risk of not detecting shifts and the Average Run Length
}
\usage{
Beta.X(k,n)
}
\arguments{
  \item{k}{A numeric vector, of length one, is the \emph{k} standard deviations factor since the known mean}
  \item{n}{An integer, equal the sample size}
}
\value{
\item{beta}{risk of not detecting shifts}
\item{ARL}{Average Run Lengh}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{xrs_gr}}
}
\examples{
Beta.X(k=1,n=5)
Beta.X(k=0.5,n=5)
Beta.X(k=1,n=3)
}
\keyword{shift}
\keyword{ARL}
