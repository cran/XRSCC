\name{U_it}
\alias{U_it}
\title{
Iteration of u control chart for attributes
}
\description{
Calculates the iteration i'th  for the control limits of  \emph{c} chart using the results obtained in \code{\link{c_gr}} and previous \code{\link{U_it}} iteration.
}
\usage{
U_it(prev.results)
}
\arguments{
  \item{prev.results}{
Is a list of previous results obtained by the \code{\link{u_gr}} function. In other cases, needs more than one iteration, to obtain the true control limits for \emph{u} chart before take conclusions about the process.}
}
\value{
\item{in.control}{The \emph{under control} row list for the \emph{u} chart}
\item{out.control}{The \emph{out of control} row list for the \emph{u } chart}
\item{Iteraciones}{The number of iterations, in this function always will be the first and the last one}
\item{data.0}{The original data frame}
\item{data.1}{Subsetting the data frame with \emph{under control} rows }
\item{bin}{The binary values for \emph{out of control} equal to one and \emph{under control} equal to zero}
\item{Limites de Control Grafica \emph{u}}{The \emph{u} chart control limits vector}
\item{Conclusion del proceso}{The same results in a phrase as the \emph{bin} values}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{p_gr}},  \code{\link{np_gr}}, \code{\link{c_gr}}, \code{\link{u_gr}}, \code{\link{P_it}}, \code{\link{NP_it}}, \code{\link{C_it}}
}
\examples{
data(udata2)
r1<-u_gr(udata2)
r2<-U_it(r1)
}
\keyword{nonconformities}
\keyword{attributes}
