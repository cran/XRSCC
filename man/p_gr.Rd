\name{p_gr}
\alias{p_gr}
\alias{p_gr}
\alias{p_chart}
\alias{proportion}
\title{
P control chart for attributes
}
\description{
Calculates the \emph{p} control chart for attributes, using a sample \emph{D} of number of defectives or nonconforming items and a constant sample size \emph{n}. The values plotted in graph are the fractions \emph{p}of defectives.
}
\usage{
p_gr(D, n)
}
\arguments{
  \item{D}{A data frame containing in one column the non conforming items, and must be integer and non negative.
}
  \item{n}{
A vector of length one, integer and nonnegative, to fix the sample size.
}
}
\value{
\item{in.control}{The \emph{under control} row list for the \emph{p} chart}
\item{out.control}{The \emph{out of control} row list for the \emph{p} chart}
\item{Iteraciones}{The number of iterations, in this function always will be the first and the last one}
\item{data.n}{The fixed sample size}
\item{data.0}{The original data frame}
\item{data.1}{The filtered data frame}
\item{bin}{The binary values for \emph{out of control} equal to one and \emph{under control} equal to zero}
\item{Limites de Control Grafica p}{The \emph{p} chart control limits vector}
\item{Conclusion del proceso}{The same results in a phrase as the \emph{bin} values}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{P_it}}, \code{\link{c_gr}}, \code{\link{C_it}}, \code{\link{np_gr}}, \code{\link{NP_it}}, \code{\link{u_gr}}, \code{\link{U_it}}
}
\examples{
data(bottles)
p_gr(bottles, n=100)
}
\keyword{defectives}
\keyword{attributes}
\keyword{fraction}
