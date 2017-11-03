\encoding{UTF-8}
\name{MUS.combined.high.error.rate}
\alias{MUS.combined.high.error.rate}
\title{Calculate a high error rate bound for a combined Monetary Unit Sampling evaluation.}
\description{
    Calculate a high error rate bound for a combined Monetary Unit Sampling evaluation.

    Please treat as experimental.
}
\usage{
    MUS.combined.high.error.rate(evaluation, interval.type)
}
\arguments{
  \item{evaluation}{A MUS.evaluation.result object used to calculate the combined bound.}
  \item{interval.type}{Optional. Interval type for high error rate evaluation. Default is "one-sided".}
}
\value{
    Upper Error Limit calculed using high error rate evaluation for a combined sample.
}
\author{Andre Guimaraes <alsguimaraes@gmail.com>}
\seealso{
  \code{\link{MUS.evaluation}} for evaluation of the audited sample.
  \code{\link{MUS.combine}} for combining multiple evaluations.
}
\examples{

# Assume 500 invoices, each between 1 and 1000 monetary units
data1 <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
# Plan a sample and cache it
plan1 <- MUS.planning(data=data1, tolerable.error=10000, expected.error=2000)
# Extract a sample and cache it (no high values exist in this example)
extract1 <- MUS.extraction(plan1)
# Copy book value into a new column audit values, and inject some error
audited1 <- extract1$sample$book.value*(1-rbinom(nrow(extract1$sample), 1, 0.05))
audited1 <- cbind(extract1$sample, audit.value=audited1)
# Evaluate the sample, cache and print it
evaluation1 <- MUS.evaluation(extract1, audited1)

# Assume 500 invoices, each between 1 and 1000 monetary units
data2 <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
# Plan a sample and cache it
plan2 <- MUS.planning(data=data2, tolerable.error=10000, expected.error=2000)
# Extract a sample and cache it (no high values exist in this example)
extract2 <- MUS.extraction(plan2)
# Copy book value into a new column audit values, and inject some error
audited2 <- extract2$sample$book.value*(1-rbinom(nrow(extract2$sample), 1, 0.05))
audited2 <- cbind(extract2$sample, audit.value=audited2)
# Evaluate the sample, cache and print it
evaluation2 <- MUS.evaluation(extract2, audited2)

combined <- MUS.combine(list(evaluation1, evaluation2))
MUS.combined.high.error.rate(combined)

}
\keyword{MUS}
\keyword{audit}
