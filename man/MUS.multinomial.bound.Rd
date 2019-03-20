\encoding{UTF-8}
\name{MUS.multinomial.bound}
\alias{MUS.multinomial.bound}
\title{Calculate a multinomial bound for a Monetary Unit Sampling evaluation.}
\description{
    Calculates a multinomial bound for a Monetary Unit Sampling evaluation.

    Please treat as experimental.
}
\usage{
    MUS.multinomial.bound(x, as.pct, include.high.values)
}
\arguments{
  \item{x}{A MUS.evaluation.result object used to calculate the multinomial bound.}
  \item{as.pct}{Boolean. Express results as percentage. Default is False.}
  \item{include.high.values}{Boolean. Whether the bound should include high values. Default is "TRUE".}
}
\value{
    Upper Error Limit calculed using the multinomial bound.
}
\author{Andre Guimaraes <alsguimaraes@gmail.com>}
\seealso{
  \code{\link{MUS.evaluation}} for evaluation of the audited sample.
}
\examples{

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
MUS.multinomial.bound(evaluation)

}
\keyword{MUS}
\keyword{audit}
