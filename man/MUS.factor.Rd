\encoding{UTF-8}
\name{MUS.factor}
\alias{MUS.factor}
\title{Calculate MUS Factor.}
\description{
  Calculate MUS Factor (AICPA, 2012).
  Based on Technical Notes on the AICPA Audit Guide Audit Sampling, Trevor Stewart, AICPA, 2012.
}
\usage{
  MUS.factor(confidence.level, pct.ratio)
}
\arguments{
  \item{confidence.level}{dito.}
  \item{pct.ratio}{Expected.error by tolerable.error.}
}
\value{
  Returns the MUS factor.
}
\author{Andre Guimaraes <alsguimaraes@gmail.com>}
\examples{

MUS.factor(0.95, 0.5)

}
\keyword{MUS}
\keyword{audit}
