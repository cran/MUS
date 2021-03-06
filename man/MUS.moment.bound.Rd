\encoding{UTF-8}
\name{MUS.moment.bound}
\alias{MUS.moment.bound}
\title{Calculate the moment bound for a Monetary Unit Sampling evaluation.}
\description{
    Calculates the moment bound (Dworkin & Grimlund, 1984) for a Monetary Unit Sampling evaluation.

    Please treat as experimental.
}
\usage{
    MUS.moment.bound(x, confidence.level, as.pct, include.high.values)
}
\arguments{
  \item{x}{A MUS.evaluation.result object (or a tainting vector) used to calculate the moment bound.}
  \item{confidence.level}{The required confidence level. Default is 95\%.}
  \item{as.pct}{Boolean. Express results as percentage. Default is False.}
  \item{include.high.values}{Boolean. Whether the bound should include high values. Default is "TRUE".}
}
\value{
    Upper Error Limit calculed using the moment bound.
}
\author{Andre Guimaraes <alsguimaraes@gmail.com>}
\seealso{
  \code{\link{MUS.evaluation}} for evaluation of the audited sample.
}
\examples{

sample = c(rep(0, 96), -.16, .04, .18, .47)
MUS.moment.bound(sample)

# Assume 500 invoices, each between 1 and 1000 monetary units
data <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
# Plan a sample and cache it
plan <- MUS.planning(data=data, tolerable.error=10000, expected.error=2000)
# Extract a sample and cache it (no high values exist in this example)
extract <- MUS.extraction(plan)
# Copy book value into a new column audit values, and inject some error
audited <- extract$sample$book.value*(1-rbinom(nrow(extract$sample), 1, 0.05))
audited <- cbind(extract$sample, audit.value=audited)
# Evaluate the sample, cache and print it
evaluation <- MUS.evaluation(extract, audited)
MUS.moment.bound(evaluation)

}
\keyword{MUS}
\keyword{audit}
