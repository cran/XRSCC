\name{C_it}
\alias{C_it}
\title{
Iteration of c control chart for attributes
}
\description{
Calculates the iteration i'th, for the control limits of \emph{c} chart using the results obtained in \code{\link{c_gr}} and previous \code{\link{C_it}} iteration.
}
\usage{
C_it(prev.results)
}
\arguments{
  \item{prev.results}{
Its a list of previous results obtained by the \code{\link{c_gr}} function. In other cases, needs more than one iteration, to obtain the true control limits, before take conclusions about the process.}
}
\value{
\item{in.control}{The \emph{under control} row list for the \emph{c} chart}
\item{out.control}{The \emph{out of control} row list for the \emph{c} chart}
\item{Iteraciones}{The number of iterations, It is assumed to be the second or later}
\item{data.0}{The original data frame or vector}
\item{data.1}{The \emph{under control} subset after iteration}
\item{bin}{The binary values for \emph{out of control} equal to one and \emph{under control} equal to zero}
\item{Limites de Control Grafica \emph{c}}{The \emph{c} chart control limits vector}
\item{Conclusion del proceso}{The same results in a phrase as the \emph{bin} values}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{p_gr}},  \code{\link{np_gr}}, \code{\link{u_gr}}, \code{\link{c_gr}}, \code{\link{P_it}}, \code{\link{NP_it}}, \code{\link{U_it}}
}
\examples{
data(clothes)
r1<-c_gr(clothes)
r2<-C_it(r1)
r3<-C_it(r2)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
