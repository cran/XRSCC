\name{R_it}
\alias{R_it}
\alias{Range}
\alias{R_chart}
\title{
Calculates the i'th iteration R Chart
}
\description{
Calculates the  iteration i'th for R chart, after the X chart is under control. The function estimates if any value (range) is out of control limits, and returns a values list.
}
\usage{
R_it(prev.results)
}
\arguments{
  \item{prev.results}{Is a list of previous results obtained by the \code{\link{xrs_gr}}, followed by \code{\link{X_it}} function if it is necessary. In other cases, needs more than one iteration to obtain the true control limits for R chart, before take conclusions about the process.
}
}
\details{
The function stops if the R chart is under control already, and also stops if there is not any active graphic device.
}
\value{
\item{in.control}{The under control row list for the X chart}
\item{R.in.control}{The \emph{under control} row list for the R chart}
\item{out.control}{The \emph{out of control} row list for the X chart}
\item{Iteraciones}{The number of iterations, It is assumed to be the second or later}
\item{data.0}{The original data frame}
\item{data.1}{The filtered data frame}
\item{data.r.1}{The calculated ranges of data.0}
\item{bin}{The binary values for \emph{out of control} equal to one and \emph{under control} equal to zero, for X and R charts}
\item{LX}{The X chart control limits vector}
\item{LR}{The R chart control limits vector}
\item{Limites Grafixa X}{The X chart control limits vector}
\item{Limites Grafixa R}{The R chart control limits vector}
\item{Conclusion del proceso}{The same results in a phrase as the \emph{bin} values}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{xrs_gr}}  \code{\link{X_it}}   \code{\link{we_rules}}   \code{\link{Cp_X}}
}
\examples{
data(dato2)
results1<-xrs_gr(dato2)
results2<-X_it(results1)
results3<-R_it(results2)
}
\keyword{range}
\keyword{variable}
