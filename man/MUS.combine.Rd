\encoding{UTF-8}
\name{MUS.combine}
\alias{MUS.combine}
\title{Combine MUS objects (joining strata into a full set).}
\description{
  Combine a list of MUS objects into a single object.
  Typical use case is to group multiple strata into a single object.
  Works with MUS.planning.result, MUS.extraction.result and MUS.evaluation.result objects.
}
\usage{
  MUS.combine(object.list)
}
\arguments{
  \item{object.list}{A list of MUS.planning.result, MUS.extraction.result and MUS.evaluation.result objects.}
}
\value{
  An object of the same type of the first item in the list is returned containing an aggregation of the objects in the list.
}
\author{Andre Guimaraes <alsguimaraes@gmail.com>}
\examples{

## Simple Example
# Assume 500 invoices, each between 1 and 1000 monetary units
stratum.1 <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
plan.1 <- MUS.planning(data=stratum.1, tolerable.error=100000, expected.error=20000)

stratum.2 <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
plan.2 <- MUS.planning(data=stratum.2, tolerable.error=100000, expected.error=20000)

plan.combined <- MUS.combine(list(plan.1, plan.2))

print(plan.combined)
}
\keyword{MUS}
\keyword{audit}
