# Simple Example
library(MUS)

calc.n <- function(conf_level, pct_tolerable, pct_expected) {
  pct_ratio <- pct_expected / pct_tolerable
  conf_factor <- ceiling(MUSFactor(conf_level, pct_ratio)*100)/100
  n <- ceiling(conf_factor / pct_tolerable)
  n
}

calc.all <- function(dados, conf_level=0.95, pct_tolerable=0.1, pct_expected=0.05) {
  if(!"conf_level" %in% colnames(dados)) {
    dados$conf_level <- conf_level
  }
  if(!"pct_tolerable" %in% colnames(dados)) {
    dados$pct_tolerable <- pct_tolerable
  }
  if(!"pct_expected" %in% colnames(dados)) {
    dados$pct_expected <- pct_expected
  }
  if(!"stratum" %in% colnames(dados)) {
    dados$stratum <- 1
  }
  dados$sizes=0
  sizes <- by(dados, dados$stratum, function(x) { calc.n(conf_level=x$conf_level, pct_tolerable=x$pct_tolerable, pct_expected=x$pct_expected) })
  dados$sizes <- c(sizes)
  dados
}

if (!exists("MUS.step")) {
  MUS.step <- 3
}


conf_level <- 0.95
if ( !"sdados" %in% ls() ) {
  sdados = data.frame("stratum"=c(1),
    "conf_level"=c(conf_level),
    "pct_tolerable"=c(0.1),
    "pct_expected"=c(0.05)
  )
}
sdados <- calc.all(sdados)

# Assume 500 invoices, each between 1 and 1000 monetary units
if ( MUS.step==1 && ( !"dados" %in% ls() ) ) {
  dados <- data.frame(
    book.value=round(runif(n=5000, min=10, max=1000)),
    stratum=round(runif(n=5000, min=1, max=nrow(sdados)))
  )
}

if(!"stratum" %in% colnames(dados)) {
    dados$stratum <- 1
}
if(!"id" %in% colnames(dados)) {
  dados$id <- c(1:nrow(dados))
}
if(!"audit.value" %in% colnames(dados)) {
  dados$audit.value <- dados$book.value*(1-rbinom(nrow(dados), 1, 0.15))
}
if(!"selected" %in% colnames(dados)) {
  dados$selected <- 0
}

plans <- list()
extract <- list()
audited <- list()
audited.high <- list()
evaluation <- list()

strata = unique(sdados$stratum)

for (s in strata) {
  cat("\nStratum", s, "\n")
  rs <- c(ifelse(s==0, TRUE, dados$stratum == s))

  if (sum(rs)==0) {
    cat("\nno records...\n")
  } else {

    if (MUS.step > 1) {
#      cat("\n\tplanning...\n")
      plans[[s]] <- MUS.planning(data=dados[rs,],
        tolerable.error=sum(sdados$pct_tolerable[s] * dados$book.value[rs]),
        expected.error=sum(sdados$pct_expected[s] * dados$book.value[rs]),
        n.min=mean(sdados$sizes[s]) )
      print(plans[[s]])
    }

    if (MUS.step > 1) {
#      cat("\n\textracting...\n")
      extract[[s]] <- MUS.extraction(plans[[s]], seed=123, obey.n.as.min=TRUE)
      dados$selected[dados$stratum == s] <- 0
      dados$selected[dados$id %in% extract[[s]]$sample$id] <- 1
      dados$selected[dados$id %in% extract[[s]]$high.values$id] <- 2
      print(extract[[s]])
    }
    if (MUS.step > 2) {
#      cat("\n\tevaluating...\n")
      # Copy book values into a new column audit values
      audited[[s]] <- extract[[s]]$sample
      audited.high[[s]] <- extract[[s]]$high.values

      # Evaluate the sample, cache and print it
      evaluation[[s]] <- MUS.evaluation(extract[[s]], audited[[s]], audited.high[[s]], print.advice=FALSE, tainting.order="absolute")
      print(evaluation[[s]], print.misstatements=FALSE)
    }
    if (MUS.step > 3) {

      cat("\n\n\tre-evaluating...\n\n")
      # extract[[s]]$confidence.level=0.95
      #extract[[s]]$expected.error=extract[[s]]$expected.error*1.5
      tolratio <- extract[[s]]$tolerable.error/extract[[s]]$expected.error

      exp.error2 <- evaluation[[1]]$Results.Total$Gross.most.likely.error[1]/evaluation[[1]]$book.value
      tol.error2 <- exp.error2 * tolratio

      extract[[s]]$tolerable.error <- tol.error2*extract[[s]]$book.value
      extract[[s]]$expected.error <- exp.error2*extract[[s]]$book.value
      nn <- calc.n(extract[[s]]$confidence.level, extract[[s]]$tolerable.error/extract[[s]]$book.value, extract[[s]]$expected.error/extract[[s]]$book.value)
      cat("\n\tnew sample size: n = ", nn, "\n")
      # Evaluate the sample, cache and print it
      evaluation[[s]] <- MUS.evaluation(extract[[s]], audited[[s]], audited.high[[s]])
      print(evaluation[[s]])
    }

  }
}
cat("\n\n- Combined Upper Error Limit\t\t\t", combined.UEL.high.error.rate(evaluation[[1]]))

cat("\n\n- moment bound example\t\t\t\t", moment.bound(c(rep(0, 96), -.16, .04, .18, .47)))
cat("\n\nEND\n")

selected <- dados$id[dados$selected>0]

if (MUS.step == 9) {
  print(.libPaths())
  print(sessionInfo())
  print(version)
}
