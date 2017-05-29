\encoding{UTF-8}
\name{MUS.extraction}
\alias{MUS.extraction}
\title{Extract a sample using Monetary Unit Sampling.}
\description{
  Extract a sample using Monetary Unit Sampling. At the end of the
  extraction step, you get to know the items that you have to audit.
}
\usage{
MUS.extraction(plan, start.point, seed, obey.n.as.min)
}
\arguments{
  \item{plan}{A MUS.planning.result object that you got by executing the
    function MUS.planning.}
  \item{start.point}{The extraction method uses fixed interval
    sampling. The monetary unit specified by start.point will be
    drawn in each interval. Default is NULL, in this case a random
    number is drawn.}
  \item{seed}{A seed number which will be used to initialise the random
    number generator. Default is NULL which means that no new random
    number generator is initialised. This argument is mainly used for
    simulations or if you want to be able to regenerate the sample on
    another computer.}
  \item{obey.n.as.min}{If set to TRUE, the sample interval will be
    exactly recalculated and thus the sample size will be exactly the
    planned sample size. Default is FALSE which is what most commercial
    statistical software do. In this case the drawn sample size might be
    slightly smaller than specified.}
}
\value{
  An object MUS.extraction.result is returned which is a list containing
  the following elements:
  \item{MUS.planning.result elements}{All elements that are contained in
  MUS.planning.result object. For auditing acceptability and for further steps all inputs are
    also returned.}
  \item{start.point}{dito.}
  \item{seed}{dito.}
  \item{obey.n.as.min}{dito.}
  \item{high.values}{The part of the population that is classified as
    individually significant items. All of them have to be audited.}
  \item{sample.population}{The part of the population that is not in the
  high-values-subpopulation.}
  \item{sampling.interval}{The reassessed sampling interval that have to
    be used for evaluation.}
  \item{sample}{The extracted sample. All elements have to be audited.}
}
\author{Henning Prömpers <henning@proempers.net>}
\seealso{
  \code{\link{MUS.planning}} for planning a sample and 
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
# Extract a sample and cache it
extract.results.simple <- MUS.extraction(plan.results.simple)

## Advanced Example
example.data.2 <- data.frame(own.name.of.book.values=round(runif(n=500,
min=1, max=1000)))
plan.results.advanced <- MUS.planning(data=example.data.2,
col.name.book.values="own.name.of.book.values", confidence.level=.70,
tolerable.error=100000, expected.error=20000, n.min=3)
extract.results.advanced <- MUS.extraction(plan.results.advanced,
start.point=5, seed=0, obey.n.as.min=TRUE)
}
\keyword{MUS}
\keyword{audit}
