# Simple Example
library(MUS)
library(pander)
library(ggplot2)

#evalsOptions('graph.unify', T)
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

use.pander <- TRUE
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
if(!"nf" %in% colnames(dados)) {
  dados$nf <- ceiling(runif(n=nrow(dados), min=1, max=1000))
}
if(!"rubrica" %in% colnames(dados)) {
  dados$rubrica <- ceiling(runif(n=nrow(dados), min=1, max=10))
}
if(!"uso" %in% colnames(dados)) {
  dados$uso <- ceiling(runif(n=nrow(dados), min=1, max=8))
}
if(!"fornec" %in% colnames(dados)) {
  dados$fornec <- ceiling(runif(n=nrow(dados), min=1, max=100))
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
format_si <- function(...) {
  function(x) {
    limits <- c(1e0,   1e3, 1e6,   1e9,   1e12)
    prefix <- c(" ",   "k", "M",   "B",   "T")

    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)

    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)

    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}
format_pct <- function(...) {
  function(x) {
      x*100
  }
}

strata = unique(sdados$stratum)
cat("\n\n")
for (s in strata) {
  mus.title(paste("Stratum", s), level=1, use.pander=use.pander)
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
      print(plans[[s]], style="report", use.pander=use.pander)
    }

    if (MUS.step > 1) {
#      cat("\n\textracting...\n")
      extract[[s]] <- MUS.extraction(plans[[s]], seed=123, obey.n.as.min=TRUE)
      dados$selected[dados$stratum == s] <- 0
      dados$selected[dados$id %in% extract[[s]]$sample$id] <- 1
      dados$selected[dados$id %in% extract[[s]]$high.values$id] <- 2
      print(extract[[s]], style="report", use.pander=use.pander)
    }
    if (MUS.step > 2) {
#      cat("\n\tevaluating...\n")
      # Copy book values into a new column audit values
      audited[[s]] <- extract[[s]]$sample
      audited.high[[s]] <- extract[[s]]$high.values

      # Evaluate the sample, cache and print it
      evaluation[[s]] <- MUS.evaluation(extract[[s]], audited[[s]], audited.high[[s]], print.advice=FALSE, tainting.order="decreasing", experimental=FALSE)
      print(evaluation[[s]], print.misstatements=FALSE, style="report", use.pander=use.pander)
      cat("\n")
#     plot(evaluation[[s]]$filled.sample$book.value, evaluation[[s]]$filled.sample$audit.value)
      cat("\n\\newpage\n")
      cat("\n## Gr&aacute;ficos\n")
      op<-par(mfrow=c(3,2))
      fsample <- evaluation[[s]]$filled.sample
      g1 <- ggplot(fsample, aes(x=as.factor(rubrica), y=book.value)) +
              geom_boxplot(fill=rgb(0.1,0.7,0.1,0.4)) +
              scale_y_continuous(labels=format_si()) +
              ggtitle("Boxplot - Sample rubrica") +
              xlab("rubrica") + ylab("book.value")
      print(g1)
      g2 <- ggplot(fsample, aes(x=as.factor(uso), y=book.value)) +
              geom_boxplot(fill=rgb(0.1,0.7,0.1,0.4)) +
              scale_y_continuous(labels=format_si()) +
              ggtitle("Boxplot - Sample itens de uso") +
              xlab("uso") + ylab("book.value")
      print(g2)
      h1 <- ggplot(fsample, aes(x=book.value)) +
              geom_histogram(bins = 6, color="white", fill=rgb(0.1,0.7,0.1,0.4)) +
              scale_x_continuous(labels=format_si()) +
              ggtitle("Histogram - Sample book.value") +
              xlab("book.value") + ylab("count")
      print(h1)
      h2 <- ggplot(fsample, aes(x=audit.value)) +
              geom_histogram(bins = 6, color="white", fill=rgb(0.1,0.1,0.7,0.4)) +
              scale_x_continuous(labels=format_si()) +
              ggtitle("Histogram - Sample audit.value") +
              xlab("audit.value") + ylab("count")
      print(h2)
      h3 <- ggplot(fsample[fsample$book.value != fsample$audit.value,], aes(x=(book.value - audit.value))) +
              geom_histogram(bins = 6, color="white", fill=rgb(0.7,0.1,0.1,0.4)) +
              scale_x_continuous(labels=format_si()) +
              ggtitle("Histogram - Sample misstatement") +
              xlab("misstatement") + ylab("count")
      print(h3)
      h4 <- ggplot(evaluation[[s]]$data, aes(x=book.value)) +
              geom_histogram(bins = 6, color="white", fill=rgb(0.1,0.7,0.1,0.4)) +
              scale_x_continuous(labels=format_si()) +
              ggtitle("Histogram - Population book.value") +
              xlab("book.value") + ylab("count")
      mat.fornec <- c(by(evaluation[[s]]$data$book.value, evaluation[[s]]$data$fornec, sum))
      df.fornec <- data.frame(fornec=names(mat.fornec), book.value=mat.fornec)
      rownames(df.fornec) <- names(mat.fornec)
      df.fornec <- df.fornec[order(-df.fornec$book.value),]
      df.fornec$seq <- (1:nrow(df.fornec))/nrow(df.fornec)
      df.fornec$csum <- cumsum(df.fornec$book.value)/evaluation[[s]]$book.value
      #plot(cumsum(df.fornec$book.value/evaluation[[1]]$book.value))
      h4 <- ggplot(df.fornec, aes(x=seq, y=csum)) +
              geom_line(color=rgb(0.1,0.7,0.1,1), size=1) +
              geom_point(data=df.fornec[ceiling(1+(nrow(df.fornec)-1)*seq(0, 1, 0.1)),], aes(x=seq, y=csum),
                color="steelblue", size=3, shape=21, fill="steelblue", stroke=1.5) +
              scale_y_continuous(labels=format_pct()) +
              scale_x_continuous(labels=format_pct()) +
              ggtitle("Cumsum - Population supplier") +
              xlab("suppliers") + ylab("cumsum book.value")

      print(h4)

      par(op)
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
      print(evaluation[[s]], use.pander=use.pander)
    }

  }
}
#cat("\n\n- Combined Upper Error Limit\t\t\t", combined.UEL.high.error.rate(evaluation[[1]]))
#cat("\n\n- moment bound example\t\t\t\t", moment.bound(c(rep(0, 96), -.16, .04, .18, .47)))

selected <- dados$id[dados$selected>0]

if (FALSE) {
  cat("\n\\newpage\n")
  mus.title(paste("Diagn&oacute;stico"), level=2, use.pander=use.pander)
  #mus.title(paste("Bibliotecas"), level=3, use.pander=use.pander)
  #print(.libPaths())
  mus.title(paste("Informa&ccedil;&otilde;es da Sess&atilde;o"), level=3, use.pander=use.pander)
  print(sessionInfo())
  mus.title(paste("Vers&otilde;es"), level=3, use.pander=use.pander)
  print(version)
}

moment.bound(c(rep(0, 96), -.16, .04, .18, .47))
moment.bound(c(rep(0, 95), -75, -25, 25, 40, 60, 75)/100)
moment.bound(c(rep(0, 96), 75, -60, -40, -25, 99)/100)
moment.bound(c(rep(0, 93), -50, -50,50,50,50,50,50,50)/100)
