\name{MUS-package}
\alias{MUS-package}
\alias{MUS}
\docType{package}
\title{
\packageTitle{MUS}
}
\description{
\packageDescription{MUS}
}
\details{
  Monetary Unit Sampling (MUS), also known as Dollar
  Unit Sampling (DUS) or Probability-Proportional-to-Size Sampling (PPS),
  is a sampling approach that is widely used in auditing.

  This package was written mainly for a research project. However, it should be
  possible to use the methods for practical auditing, too. Furthermore, the package comes
  with ABSOLUTELY NO WARRANTY. Use it at your own risk!
  
  You have to walk through four steps:
  1. Plan a sample and determine the sample size, use function: MUS.planning
  2. Extract the sample, use function: MUS.extract
  3. Audit the extracted sample (e.g. by asking for debtor confirmations).
  4. Evaluate the audited sample, use function: MUS.evaluation
}
\author{
\packageAuthor{MUS}
Maintainer: \packageMaintainer{MUS}
}
\keyword{MUS}
\keyword{audit}
\seealso{
  \code{\link{MUS.planning}} for planning a sample,
  \code{\link{MUS.extraction}} for extraction of the planned sample and 
  \code{\link{MUS.evaluation}} for evaluation of the extracted and
  audited sample.
}
\examples{
## Simple Example
library(MUS)
# Assume 500 invoices, each between 1 and 1000 monetary units
example.data.1 <- data.frame(book.value=round(runif(n=500, min=1,
max=1000)))
# Plan a sample and cache it
plan.results.simple <- MUS.planning(data=example.data.1,
tolerable.error=100000, expected.error=20000)
# Extract a sample and cache it (no high values exist in this example)
extract.results.simple <- MUS.extraction(plan.results.simple)
# Copy book values into a new column audit values
audited.sample.simple <- extract.results.simple$sample
audited.sample.simple <- cbind(audited.sample.simple,
audit.value=audited.sample.simple$book.value)
# Edit manually (if any audit difference occur)
#audited.sample.simple <- edit(audited.sample.simple)
# Evaluate the sample, cache and print it
evaluation.results.simple <- MUS.evaluation(extract.results.simple,
audited.sample.simple)
print(evaluation.results.simple)
}
