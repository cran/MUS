\encoding{UTF-8}
\name{MUS.extend}
\alias{MUS.extend}
\title{Extend a MUS sample.}
\description{
  Extends a sample that requires further evidence.
  Works with MUS.extraction.result.

  Please treat as experimental.
}
\usage{
  MUS.extend(extract, additional.n)
}
\arguments{
  \item{extract}{An object of the type MUS.extraction.result to be extended.}
  \item{additional.n}{Extend the sample by additional.n items.}
}
\value{
  Returns an extended MUS.extraction.result object.
}
\author{Andre Guimaraes <alsguimaraes@gmail.com>}
\examples{

## Simple Example
# Assume 500 invoices, each between 1 and 1000 monetary units
example.data.1 <- data.frame(book.value=round(runif(n=500, min=1,
max=1000)))

# Plan a sample and cache it
plan.results.simple <- MUS.planning(data=example.data.1,
tolerable.error=100000, expected.error=20000)

# Extract a sample and cache it
extract1 <- MUS.extraction(plan.results.simple)

# extends the sample by 20 itens
extract2 <- MUS.extend(extract1, 20)

}
\keyword{MUS}
\keyword{audit}
