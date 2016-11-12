\name{xrs_gr}
\alias{xrs_gr}
\title{Calculate and plot the X, R and S Charts for variable charts
}
\description{
Calculates the control limits for X, R and S charts, using a data frame with a fixed subgroup size. Plots the corresponding graph, the function estimates if any value is out of the control limits, returns a list with calculations.
}
\usage{
xrs_gr(X)
}
\arguments{
  \item{X}{A sample in a dataframe object, with \emph{m} rows like subgroups, and \emph{n} columns like sample size.
}
}
\value{
\item{in.control}{The \emph{under control} row list for the X chart}
\item{R.in.control}{The  \emph{under control} row list for the R chart}
\item{out.control}{The \emph{out of control} row list for the X chart}
\item{Iteraciones}{The iterations number, the firts and the last one on this function}
\item{data.0}{The original data frame}
\item{data.1}{The \emph{under control} subset after iteration}
\item{data.r.1}{The calculated ranges of data.0}
\item{bin}{The binary values for \emph{out of control}  equal to one and \emph{under control}  equal to zero, for X, R and S charts}
\item{LX}{The X chart control limits vector}
\item{LR}{The R chart control limits vector}
\item{LS}{The S chart control limits vector}
\item{Limites Grafixa X}{The X chart control limits vector}
\item{Limites Grafixa R}{The R chart control limits vector}
\item{Limites Grafixa S}{The S chart control limits vector}
\item{Conclusion del proceso}{The same results in a phrase as the \emph{bin} values}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{Erick Marroquin
}
\seealso{
\code{\link{X_it}},  \code{\link{we_rules}},   \code{\link{R_it}},   \code{\link{Cp_X}},    \code{\link{Beta.X}}
}
\examples{
data(vol_sample)
results1<-xrs_gr(vol_sample)
}
\keyword{X_chart}
\keyword{variable}
