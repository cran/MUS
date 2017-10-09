\encoding{UTF-8}
\name{MUS.planning}
\alias{MUS.planning}
\title{Plan a sample using Monetary Unit Sampling.}
\description{
  Plan a sample for Monetary Unit Sampling. At the end of this planning step,
  you get to know the sample size.

  Be aware that this MUS routines cannot calculate with
  decimals. Furthermore, you must provide book values etc. as Euro-Cent so that no decimals occur.
}
\usage{
MUS.planning(data, col.name.book.values, confidence.level,
tolerable.error, expected.error, n.min, errors.as.pct, conservative, combined)
}
\arguments{
  \item{data}{A data frame or matrix which contains at least one column
    with the book values.}
  \item{col.name.book.values}{The name of the column that
    contains the book values. Default is "book.value".}
  \item{confidence.level}{The required confidence level. Default is
    95\%.}
  \item{tolerable.error}{The tolerable error (materiality) in Monetary
    Units.}
  \item{expected.error}{The expected error which is contained in the
    population in Monetary Units.}
  \item{n.min}{Minimum sample size that should be used. Default is 0.}
  \item{errors.as.pct}{Boolean. Tolerable and Expected error informed as percentages. Default is False.}
  \item{conservative}{Boolean. If true, use greater sample size between normal calculation and
     conservative algorithm (i.e., gamma-based, AICPA compatible).}
  \item{combined}{Boolean. Marks the dataset as a combination of multiple strata. Default is "FALSE".}
}
\value{
  An object MUS.planning.result is returned which is a list containing
  the following elements:
  \item{data}{For auditing acceptability and for further steps all inputs are
    also returned.}
  \item{col.name.book.values}{dito.}
  \item{confidence.level}{dito.}
  \item{tolerable.error}{dito.}
  \item{expected.error}{dito.}
  \item{book.value}{The calculated gross book value of the
    population. Negative values are ignored.}
  \item{n}{The calculated sample size based on the input parameters
    which is greater or egal than the provided minimum sample size.}
  \item{High.value.threshold}{Whenever a book value of an element is above the
  threshold, the element will be considered individually
  significant. Individual significant items will be audited completely,
  no sample extrapolation will be necessary.}
  \item{tolerable.taintings}{The number of taintings in the sample that will be
    acceptable at maximum.}
}
\author{Henning Prömpers <henning@proempers.net>}
\seealso{
  \code{\link{MUS.extraction}} for extraction of the planned sample and
  \code{\link{MUS.evaluation}} for evaluation of the extracted and
  audited sample.
}
\examples{
## Simple Example
# Assume 500 invoices, each between 1 and 1000 monetary units
example.data.1 <- data.frame(book.value=round(runif(n=500, min=1,
max=1000)))
# Plan a sample and cache it
plan.results.simple <- MUS.planning(data=example.data.1,
tolerable.error=100000, expected.error=20000)

## Advanced Example
example.data.2 <- data.frame(own.name.of.book.values=round(runif(n=500,
min=1, max=1000)))
plan.results.advanced <- MUS.planning(data=example.data.2,
col.name.book.values="own.name.of.book.values", confidence.level=.70,
tolerable.error=100000, expected.error=20000, n.min=3)
}
\keyword{MUS}
\keyword{audit}
