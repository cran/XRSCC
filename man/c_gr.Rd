\name{c_gr}
\alias{c_gr}
\title{
The c chart control for attributes
}
\description{
Calculates the \emph{c} control chart for attributes, using a sample \emph{C} of number of nonconformities. The plotted values in graph are the nonconformities number on each sample at a regular time interval when there is not a standard  given.
}
\usage{
c_gr(C)
}
\arguments{
  \item{C}{
A data frame or a vector containing the number of nonconformities per sample. Note that the variable name must be the uppercase letter, like \strong{\emph{D}}.
}
}
\value{
\item{in.control}{The \emph{under control} row list for the \emph{c} chart}
\item{out.control}{The \emph{out of control} row list for the \emph{c} chart}
\item{Iteraciones}{The number of iterations, in this function always will be the first and the last one}
\item{data.0}{The original data frame}
\item{data.1}{Subsetting the data frame with \emph{under control} rows }
\item{bin}{The binary values for \emph{out of control} equal to one, and results \emph{under control} equal to zero}
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
\code{\link{p_gr}},  \code{\link{np_gr}}, \code{\link{u_gr}}, \code{\link{P_it}}, \code{\link{NP_it}}, \code{\link{C_it}},  \code{\link{U_it}}
}
\examples{
data(clothes)
c_gr(clothes)
}
