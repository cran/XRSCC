\name{X_it}
\alias{X_it}
\alias{X_chart}
\alias{chart_iteration}
\title{
Calculates the iteration i'th  X Chart
}
\description{
With the results of \code{\link{xrs_gr}} followed by previous X_it iterations, the function calculates the X control limits charts, using a data frame with a fixed subgroup size \emph{n}. In the graph plotting, the function estimates if any value (row or subgroup average) is out of control limits, and returns a list with calculations. Also, gives the R chart and control limits, which will be used in \code{\link{R_it}} function.
}
\usage{
X_it(prev.results)
}
\arguments{
  \item{prev.results}{Is a list of previous results obtained by the \code{\link{xrs_gr}} function in the first iteration, or a list of results obtained in further iterations by the X_it function.
}
}
\details{
The function stops if the X chart is under control already, and also stops if there is not any active graphic device.
}
\value{
\item{in.control}{The \emph{under control} row list for the X chart}
\item{R.in.control}{The \emph{under control} row list for the R chart}
\item{out.control}{The \emph{out of control} row list for the X chart}
\item{Iteraciones}{The iterations number, It is assumed to be the second or later}
\item{data.0}{The original data frame}
\item{data.1}{The \emph{under control} subset after iteration}
\item{data.r.1}{The calculated ranges of data.0}
\item{bin}{The binary values for \emph{out of control} equal to one and \emph{under control} equal to zero, for X and R charts}
\item{LX}{The X chart control limits vector}
\item{LR}{The R chart control limits vector}
\item{Limites Grafixa X}{The X chart control limits vector}
\item{Limites Grafixa R}{The R chart control limits vector}
\item{Conclusion del proceso}{The same results in a phrase as the \emph{bin} values}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3}
}
\author{
Erick Marroquin
}
\note{
For the true Range control limits calculation, use \code{\link{R_it}}.
}
\seealso{
\code{\link{xrs_gr}},  \code{\link{R_it}},   \code{\link{Cp_X}}, \code{\link{we_rules}}
}
\examples{
data(vol_sample)
results1<-xrs_gr(vol_sample)
results2<-X_it(results1)
}
\keyword{X_chart}
\keyword{variable}
