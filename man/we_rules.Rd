\name{we_rules}
\alias{we_rules}
\title{
Estimates the first four Western Electric Rules for detecting patterns
}
\description{
Estimates the first four Western Electric Rules for detecting patterns, starting with under control X chart obtained in the sequence \code{\link{xrs_gr}}, \code{\link{X_it}}, \code{\link{R_it}} functions. At the same time, plots the X chart including the zones above and below the central limit. For last, a binary value for each rule is presented if at least one rule is violated, '1' for 'yes', 0 for 'no'.
}
\usage{
we_rules(prev.results)
}
\arguments{
  \item{prev.results}{Its a list of previous results obtained by the \code{\link{xrs_gr}} function in the first iteration, or a list of results obtained in further iterations by the \code{\link{X_it}}, and if necessary by the \code{\link{R_it}} function.
}
}
\details{
The previous results may say that the process is under control, but, it's a conclusion concerning the first Western Electric rule only.
}
\value{
\item{Resultados de analisis}{A phrarse saying the process is or not under control}
\item{Las siguientes reglas tienen al menos un grupo que viola la regla}{The conclussion about the Western Electric rules from 1 to 4, showing a binary response, '1' for 'yes', 0 for 'no'.}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3

SMALL, Bonnie B. (1956) \emph{Statistical Quality Control Handbook}, 2th ed. Easton : Western Electric Co, Inc.

yhat \emph{The Yhat Blog. Machine Learning, Data Science, Engineering}, [On line] http://blog.yhathq.com/posts/quality-control-in-r.html

}
\author{
Erick Marroquin
}
\seealso{
\code{\link{xrs_gr}},  \code{\link{X_it}},   \code{\link{R_it}},  \code{\link{Cp_X}}
}
\examples{
data(qqsugar)
results1<-xrs_gr(qqsugar)
results2<-R_it(results1)
we_rules(results2)
}
\keyword{patterns}
\keyword{range}
