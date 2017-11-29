\encoding{UTF-8}
\name{print.MUS.extraction.result}
\alias{print.MUS.extraction.result}
\title{Pretty and comprehensive printing of MUS extraction results}
\description{
  Pretty and comprehensive printing of MUS extraction results that can
  be used for working papers.
}
\usage{
\method{print}{MUS.extraction.result}(x, print.title,
  print.planning, style, use.pander, ...)
}

\arguments{
  \item{x}{A MUS.evaluation.result object that you got by executing the
    function MUS.evaluation.}
  \item{print.title}{Boolean. Should title be printed? Defaults to TRUE.}
  \item{print.planning}{Boolean. Should planning parameters be printed? Defaults to FALSE.}
  \item{style}{Two options: "report" or "default". Report uses an alternative layout. Defaults to "default".}
  \item{use.pander}{Boolean. Uses pander to generate rmarkdown report. Defaults to FALSE.}
  \item{...}{Further arguments, currently ignored.}
}
\author{Henning Prömpers <henning@proempers.net>}
\seealso{
  \code{\link{MUS.extraction}} for extraction of the audit sample.
}
\keyword{MUS}
\keyword{audit}
