\name{np_gr}
\alias{np_gr}
\title{
The np chart control for attributes
}
\description{
Calculates the \emph{np} control chart for attributes, using a sample \emph{D} of number of defectives or nonconforming items and a constant sample size \emph{n}. The values plotted in graph are the defectives number.
}
\usage{
np_gr(D, n)
}
\arguments{
    \item{D}{A data frame containing the non conforming items, and must be integer and non negative.
}
  \item{n}{
A vector of length one, integer and nonnegative, to fix the sample size.
}
}
\value{
\item{in.control}{The \emph{under control} row list for the \emph{np} chart}
\item{out.control}{The \emph{out of control} row list for the \emph{np} chart}
\item{Iteraciones}{The number of iterations, in this function always will be the first and the last one}
\item{data.n}{The fixed sample size}
\item{data.0}{The original data frame}
\item{data.1}{The filtered data frame}
\item{bin}{The binary values for \emph{out of control} equal to one and \emph{under control} equal to zero}
\item{Limites de Control Grafica \emph{np}}{The \emph{np} chart control limits vector}
\item{Conclusion del proceso}{The same results in a phrase as the \emph{bin} values}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{p_gr}},  \code{\link{u_gr}},  \code{\link{c_gr}}, \code{\link{P_it}}, \code{\link{NP_it}}, \code{\link{C_it}},  \code{\link{U_it}}
}
\examples{
data(bottles)
np_gr(bottles, n=100)
}
\keyword{defectives}
\keyword{attributes}
