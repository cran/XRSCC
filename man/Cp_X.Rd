\name{Cp_X}
\alias{Cp_X}
\alias{capability}
\title{
Calculates the process capability
}
\description{
Given a variable sample, the function calculates the process capability and, assuming a normal distribution of the X chart, after the true control limits were found.
}
\usage{
Cp_X(prev.results, LES, LEI, mu)
}
\arguments{
  \item{prev.results}{
Is a list of previous results obtained by the \code{\link{xrs_gr}} function in the first iteration, or the results obtained in further iterations by the \code{\link{X_it}} function.
}
  \item{LES}{
A numeric vector of length one, containing the upper specification limit.
}
  \item{LEI}{
A numeric vector of length one, containing the lower specification limit.
}
  \item{mu}{
A numeric vector of length one, containing the average specification, if not exists, function takes the Control Limit of previous results.
}
}
\details{
The function stops for the lack of any arguments.
}
\value{
\item{Cp}{The process capability index}
\item{Cpk}{The process capability index in case is not centered}
\item{P.cp}{The specification range percentage used by the control limits}
\item{X.sigma}{The process standard deviation}
\item{Conclusion del proceso}{A phrase to take conclusion about the process capability}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{xrs_gr}}  \code{\link{X_it}}  \code{\link{R_it}}   \code{\link{we_rules}}
}
\examples{
data(vol_sample)
results1<-xrs_gr(vol_sample)
results2<-X_it(results1)
# Type dev.off() function before use Cp_X
Cp_X(results2, LES=510, LEI=490, mu=500)
}
\keyword{capability}
\keyword{sixsigma}
