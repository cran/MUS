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
  MUS.extend(extract, new_plan=NULL, additional.n=NULL)
}
\arguments{
  \item{extract}{An object of the type MUS.extraction.result to be extended.}
  \item{new_plan}{Provide a new MUS plan. If null, you must provide the qty of items to extend the sample.}
  \item{additional.n}{Ignored if new_plan is provided, otherwise sample will be extended by additional.n items}
}
\value{
  Returns an extended MUS.extraction.result object.
}
\author{Andre Guimaraes <alsguimaraes@gmail.com>}
\examples{

## Simple Example
# Assume 500 invoices
mydata <- data.frame(book.value=
  round(c(runif(n=480, min=10,max=20000),
  runif(n=20, min=15000,max=50000)))
)

# Plan a sample and cache it
plan <- MUS.planning(data=mydata,
tolerable.error=50000, expected.error=3000)

# Extract a sample and cache it
extract <- MUS.extraction(plan, obey.n.as.min=TRUE)

# Create a new plan
new_plan <- MUS.planning(data=mydata,
tolerable.error=50000, expected.error=5000)

# extends the sample using the new plan
extended <- MUS.extend(extract, new_plan)

# extends the sample by 20 itens using the original plan
extended20 <- MUS.extend(extract, additional.n=20)

}
\keyword{MUS}
\keyword{audit}
