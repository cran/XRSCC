\name{u_gr}
\alias{u_gr}
\title{
The u chart control for attributes
}
\description{
Calculates the \emph{u} control chart for attributes, given a variable sample \emph{n} and a number of nonconformities \emph{u} per sample. The plotted values in graph are the average number of nonconformities per unit.
}
\usage{
u_gr(U)
}
\arguments{
  \item{U}{
  A data frame containing the number \emph{d} of nonconformities per sample, the sample \emph{n} can be variable. Note that the variable names must be lowercase letter, say  \strong{\emph{d}} and  \strong{\emph{n}}.
}
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
\code{\link{p_gr}},  \code{\link{np_gr}}, \code{\link{c_gr}}, \code{\link{P_it}}, \code{\link{NP_it}}, \code{\link{C_it}},  \code{\link{U_it}}
}
\examples{
data(udata2)
u_gr(udata2)
}
\keyword{nonconformities}
\keyword{attributes}
