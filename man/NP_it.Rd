\name{NP_it}
\alias{NP_it}
\title{
Iteration of np control chart for attributes
}
\description{
Calculates the iteration i'th for the control limits of  \emph{p} chart using the results obtained in \code{\link{np_gr}} or further NP_it iterations.
}
\usage{
NP_it(prev.results)
}
\arguments{
  \item{prev.results}{
Is a list of previous results obtained by the \code{\link{np_gr}} function. In other cases, needs more than one iteration, to obtain the true control limits for \emph{np} chart before take conclusions about the process.}
}
\value{
\item{in.control}{The \emph{under control} row list for the \emph{np}  chart in this iteration}
\item{out.control}{The \emph{out of control} row list for the \emph{np}  chart}
\item{Iteraciones}{The number of iterations, It is assumed to be the second or later}
\item{data.n}{The fixed sample size}
\item{data.0}{The original data frame}
\item{data.1}{The \emph{under control} subset after iteration}
\item{bin}{The binary values for \emph{out of control} equal to one and \emph{under control} equal to zero}
\item{Limites de Control Grafica  \emph{np}}{The \emph{np} chart control limits vector}
\item{Conclusion del proceso}{The same results in a phrase as the \emph{bin} values}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{p_gr}},  \code{\link{np_gr}}, \code{\link{c_gr}}, \code{\link{u_gr}}, \code{\link{P_it}}, \code{\link{C_it}},  \code{\link{U_it}}
}
\examples{
data(bottles)
r1<-np_gr(bottles, n=100)
r2<-NP_it(r1)
r3<-NP_it(r2)
}
\keyword{defectives}
\keyword{attributes}
