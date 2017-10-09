\encoding{UTF-8}
\name{print.MUS.evaluation.result}
\alias{print.MUS.evaluation.result}
\title{Pretty and comprehensive printing of MUS evaluation results}
\description{
  Pretty and comprehensive printing of MUS evaluation results that can
  be used for working papers.
}
\usage{
\method{print}{MUS.evaluation.result}(x, error.rate, print.misstatements,
  print.planning, print.extraction, print.error.as.pct, print.advice,
	style, use.pander, ...)
}
\arguments{
  \item{x}{A MUS.evaluation.result object that you got by executing the
    function MUS.evaluation.}
  \item{error.rate}{Selects type of error rate calculation (i.e., "high", "low", "both" or "auto"). Defaults to "auto".}
  \item{print.misstatements}{Boolean. Should misstatements table be printed? Defaults to TRUE.}
  \item{print.planning}{Boolean. Should planning parameters be printed? Defaults to FALSE.}
  \item{print.extraction}{Boolean. Should extraction parameters be printed? Defaults to FALSE.}
  \item{print.error.as.pct}{Boolean. Should errors as percentage be printed? Defaults to TRUE.}
  \item{print.advice}{Boolean. Should recommendations be printed? Defaults to TRUE.}
  \item{style}{Two options: "report" or "default". Report uses an alternative layout. Defaults to "default".}
  \item{use.pander}{Boolean. Uses pander to generate rmarkdown report. Defaults to FALSE.}
  \item{...}{Further arguments, currently ignored.}
}
\author{Henning Prömpers <henning@proempers.net>}
\seealso{
  \code{\link{MUS.evaluation}} for evaluation of the extracted and
  audited sample.
}
\keyword{MUS}
\keyword{audit}
