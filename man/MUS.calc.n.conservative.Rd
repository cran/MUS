\encoding{UTF-8}
\name{MUS.calc.n.conservative}
\alias{MUS.calc.n.conservative}
\title{Calculate a conservative sample size.}
\description{
  Calculate a conservative sample size (AICPA, 2012).
  Based on Technical Notes on the AICPA Audit Guide Audit Sampling, Trevor Stewart, AICPA, 2012.
}
\usage{
  MUS.calc.n.conservative(confidence.level, tolerable.error, expected.error, book.value)
}
\arguments{
  \item{confidence.level}{dito.}
  \item{tolerable.error}{Tolerable error in monetary units.}
  \item{expected.error}{Expected error in monetary units.}
  \item{book.value}{Book value in monetary units.}
}
\value{
  Returns the (conservative) sample size.
}
\author{Andre Guimaraes <alsguimaraes@gmail.com>}
\examples{

MUS.calc.n.conservative(0.95, 100000, 50000, 10000000)

}
\keyword{MUS}
\keyword{audit}
