\encoding{UTF-8}
\name{MUS.evaluation}
\alias{MUS.evaluation}
\title{Evaluate a sample using Monetary Unit Sampling.}
\description{
  Evaluate a sample using Monetary Unit Sampling. At the end of the
  evaluation step, you get to know the audit conclusion for the population. To
  conduct the evaluation step it is required that you audited the sample
  and high values before.
  You can use print() for a comprehensive output.
}
\usage{
MUS.evaluation(extract, filled.sample, filled.high.values,
col.name.audit.values, col.name.riskweights)
}
\arguments{
  \item{extract}{A MUS.extraction.result object that you got by executing the
    function MUS.extraction.}
  \item{filled.sample}{A data frame or matrix with the sample from the
    extraction routine that have an additional column with the audit
    values.}
  \item{filled.high.values}{A data frame or matrix with the high value
    items from the extraction routine that have an additional column
    with the audit values.}
  \item{col.name.audit.values}{Single character with the name of the
    column containing the audit value in filled.sample respectively
    filled.high.values. Default is "audit.value".}
  \item{col.name.riskweights}{Single character with the name of the
    column containing the risk weights in filled.sample respectively
    filled.high.values. Default is NULL, then no risk weights are
    included in the calcualations (the ordinary MUS case).}
}
\value{
  An object MUS.evaluation.result is returned which is a list containing
  the following elements:
  \item{MUS.extraction.result elements}{All elements that are contained
  in MUS.extraction.result object. For auditing acceptability and for further steps all inputs are
    also returned.}
  \item{filled.sample}{dito.}
  \item{filled.high.values}{dito.}
  \item{col.name.audit.values}{dito.}
\item{Overstatements.Result.Details}{Detail table for overstatements
  found in the sample.}
\item{Understatements.Result.Details}{Detail table for understatements
  found in the sample.}
\item{Results.Sample}{Comprehensive results of sample evaluation.}
\item{Results.High.values}{Comprehensive results of individually
  significant item evaluation.}
\item{Results.Total}{Comprehensive results of both evaluations (sample
  and individual significant items).}
\item{acceptable}{Boolean, if population is acceptable given results,
  confidence level and materiality.}
}
\author{Henning Prömpers <henning@proempers.net>}
\examples{
## Simple Example
# Assume 500 invoices, each between 1 and 1000 monetary units
example.data.1 <- data.frame(book.value=round(runif(n=500, min=1,
max=1000)))
# Plan a sample and cache it
plan.results.simple <- MUS.planning(data=example.data.1,
tolerable.error=100000, expected.error=20000)
# Extract a sample and cache it (no high values exist in this example)
extract.results.simple <- MUS.extraction(plan.results.simple)
# Copy book value into a new column audit values
audited.sample.simple <- extract.results.simple$sample
audited.sample.simple <- cbind(audited.sample.simple,
audit.value=audited.sample.simple$book.value)
# Edit manually (if any audit difference occur)
#audited.sample.simple <- edit(audited.sample.simple)
# Evaluate the sample, cache and print it
evaluation.results.simple <- MUS.evaluation(extract.results.simple,
audited.sample.simple)
print(evaluation.results.simple)

## Advanced Example
example.data.2 <- data.frame(own.name.of.book.values=round(runif(n=500,
min=1, max=1000)))
plan.results.advanced <- MUS.planning(data=example.data.2,
col.name.book.values="own.name.of.book.values", confidence.level=.70,
tolerable.error=100000, expected.error=20000, n.min=3)
extract.results.advanced <- MUS.extraction(plan.results.advanced,
start.point=5, seed=1, obey.n.as.min=TRUE)
extract.results.advanced <- MUS.extraction(plan.results.advanced)
audited.sample.advanced <- extract.results.advanced$sample
audited.sample.advanced <- cbind(audited.sample.advanced,
own.name.of.audit.values=audited.sample.advanced$own.name.of.book.values)
#audited.sample.advanced <- edit(audited.sample.advanced)
evaluation.results.advanced <- MUS.evaluation(extract.results.advanced,
audited.sample.advanced,
col.name.audit.values="own.name.of.audit.values")
print(evaluation.results.advanced)
}
\keyword{MUS}
\keyword{audit}
