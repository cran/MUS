# Simple Example
library(MUS)
library(pander)
library(ggplot2)
library(rmarkdown)
library(animation)

calc.n <- function(conf_level, pct_tolerable, pct_expected) {
  pct_ratio <- pct_expected / pct_tolerable
  conf_factor <- ceiling(MUS.factor(conf_level, pct_ratio)*100)/100
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
    dados$stratum <- ceiling(runif(n=nrow(dados), min=0, max=3))
  }
  dados$sizes=0
  sizes <- by(dados, dados$stratum, function(x) { calc.n(conf_level=x$conf_level, pct_tolerable=x$pct_tolerable, pct_expected=x$pct_expected) })
  dados$sizes <- c(sizes)
  dados
}

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

format_exp <- function(...) {
  function(x) {
    x <- exp(x)
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

# setup options and variables
ani.options(pdftk = '/bin/pdftk')
if (!file.exists('/bin/pdftk')) {
  ani.options(pdftk = '/tools/pdftk/pdftk.exe')
}
panderOptions('keep.trailing.zeros', T)
panderOptions('round', 2)
panderOptions('digits', 19)

if (!exists("id.amostragem")) {
  id.amostragem <- 1
}
MUS.seed <- id.amostragem %% 1000

if (!exists("MUS.step")) {
  MUS.step <- 3
}

use.pander <- TRUE
conf_level <- 0.95
if ( !"sdados" %in% ls() ) {
  H <- 3  # number of strata
  sdados = data.frame("stratum"=1:H,
    "conf_level"=rep(conf_level, H),
    "pct_tolerable"=rep(0.1, H),
    "pct_expected"=rep(0.05, H)
  )
}
sdados <- calc.all(sdados)

# Assume 5000 invoices, each between 1 and 1000 monetary units
if ( MUS.step==1 & ( !"dados" %in% ls() ) ) {
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
  dados$rubrica <- paste("rubrica ", ceiling(runif(n=nrow(dados), min=1, max=10)))
}
uniq.rubrica <- unique(dados$rubrica)
dados$id.rubrica <- as.factor(match(dados$rubrica, uniq.rubrica))
soma.rubrica <- c(by(dados$book.value, dados$id.rubrica, sum))
qtd.rubrica <- c(by(dados$book.value, dados$id.rubrica, length))
rubricas <- data.frame(id=as.numeric(names(soma.rubrica)), nome=uniq.rubrica[as.numeric(names(soma.rubrica))], qty=as.numeric(qtd.rubrica), value=as.numeric(soma.rubrica))

if(!"id" %in% colnames(dados)) {
  dados$id <- c(1:nrow(dados))
}
if(!"audit.value" %in% colnames(dados)) {
  dados$audit.value <- dados$book.value*(1-rbinom(nrow(dados), 1, 0.15))
}
if(!"selected" %in% colnames(dados)) {
  dados$selected <- 0
}

if (!exists("inclui_total")) {
  inclui_total <- FALSE
}
if (!inclui_total && MUS.step!=4) {
  plans <- list()
  extract <- list()
  audited <- list()
  audited.high <- list()
  evaluation <- list()
}

strata <- unique(sdados$stratum)
# combina estratos
if ((!inclui_total) & (length(strata)>1) & (MUS.step == 4)) {
	plans <- c(list(MUS.combine(plans)), plans)
	extract <- c(list(MUS.combine(extract)), extract)
	evaluation <- c(list(MUS.combine(evaluation)), evaluation)
	inclui_total <- TRUE
}

if (inclui_total & length(strata)>1) {
  strata <- c(1, 1+strata)
}

dm <- "R-MUS"
bindtextdomain(dm)

cat("\n\n")
resultados <- rep(NULL, length(strata))
erro.provavel <- rep(NULL, length(strata))
for (s in strata) {
  numStratum <- s
  if (inclui_total & length(strata)>1) {
    if (s>1) {
      cat("\n\\newpage\n")
    }
    numStratum <- numStratum - 1
  }
  MUS:::.title(ifelse(inclui_total & length(strata)>1 & s==1,
    paste0(gettext("Population", domain=dm), " (", length(strata)-1, " ", gettext("strata", domain=dm), ")"),
    paste(gettext("Stratum", domain=dm), numStratum)), level=1, use.pander=use.pander)
  rs <- (dados$stratum == numStratum)
  if (inclui_total & length(strata)>1 & s==1) {
      rs <- TRUE
  }

  if (sum(rs)==0) {
    cat("\nno records...\n")
  } else {

    if (MUS.step > 0) {
#      cat("\n\tplanning...\n")
      if (!inclui_total) {
        plans[[s]] <- MUS.planning(data=dados[rs,],
          tolerable.error=sum(sdados$pct_tolerable[s] * dados$book.value[rs]),
          expected.error=sum(sdados$pct_expected[s] * dados$book.value[rs]),
          n.min=mean(sdados$sizes[s]) )
      }
      print(plans[[s]], style="report", use.pander=use.pander)
    }

    if (MUS.step > 1) {
#      cat("\n\textracting...\n")
      if (!inclui_total) {
        extract[[s]] <- MUS.extraction(plans[[s]], seed=MUS.seed, obey.n.as.min=TRUE)
        dados$selected[dados$stratum == s] <- 0
        dados$selected[dados$id %in% extract[[s]]$sample$id] <- 1
        dados$selected[dados$id %in% extract[[s]]$high.values$id] <- 2
      }
      print(extract[[s]], style="report", use.pander=use.pander)
    }
    if (MUS.step > 2) {
#      cat("\n\tevaluating...\n")
      # Copy book values into a new column audit values
      # Evaluate the sample, cache and print it
      if (!inclui_total) {
        audited[[s]] <- extract[[s]]$sample
        audited.high[[s]] <- extract[[s]]$high.values
        evaluation[[s]] <- MUS.evaluation(extract[[s]], audited[[s]], audited.high[[s]], print.advice=FALSE, tainting.order="decreasing", experimental=FALSE)
      }
      print(evaluation[[s]], print.misstatements=FALSE, style="report", use.pander=use.pander)
      cat("\n")
      resultados[s] <- evaluation[[s]]$acceptable
      mle <- evaluation[[s]]$MLE.final
      tot <- evaluation[[s]]$book.value - ifelse(is.data.frame(evaluation[[s]]$filled.high.values), sum(evaluation[[s]]$filled.high.values$book.value), 0)
      erro.provavel[s] <- mle / tot
    }

    if (MUS.step > 3) {

      cat("\n\\newpage\n")
      cat("\n## Gr&aacute;ficos\n")
      op<-par(mfrow=c(3,2))
      fsample <- evaluation[[s]]$filled.sample
      pop <- evaluation[[s]]$data
      comb.fsample <- rbind(data.frame(g=rgb(0,0.7,0.1,0.4), v=fsample$book.value), data.frame(g=rgb(0,0.1,0.7,0.4), v=fsample$audit.value))

      tsoma.rubrica <- c(by(pop$book.value, pop$id.rubrica, sum))
      tqtd.rubrica <- c(by(pop$book.value, pop$id.rubrica, length))
      trubricas <- data.frame(id=as.numeric(names(tsoma.rubrica)),
        nome=uniq.rubrica[as.numeric(names(tsoma.rubrica))],
        qty=as.numeric(tqtd.rubrica),
        value=as.numeric(tsoma.rubrica)
      )

      orubricas <- trubricas
      trubricas$pct <- Vectorize(MUS:::.percent)(trubricas$value / sum(trubricas$value))
      trubricas$value <- Vectorize(MUS:::.value)(trubricas$value)
      comb.pop <- rbind(data.frame(g=rgb(0,0.7,0.1,0.4), v=pop$book.value), data.frame(g=rgb(0,0.1,0.7,0.4), v=pop$audit.value))

      colnames(trubricas) <- c("Id", paste0(c(gettext("Description", domain=dm), rep("&nbsp;",5)), collapse=""),
        gettext("Items", domain=dm), gettext("Value", domain=dm), "%")

      pop.grid <- theme(
        panel.background = element_rect(fill = "powderblue",
                                      colour = "powderblue",
                                      size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white")
        )

      h1 <- ggplot(fsample, aes(x=book.value)) +
              geom_histogram(bins = 6, color="white", fill=rgb(0,0.7,0.1,0.4)) +
              scale_x_continuous(labels=format_si()) +
              ggtitle("Amostra") +
              xlab("valor informado") + ylab("qtd")
      print(h1)
      h2 <- ggplot(fsample, aes(x=audit.value)) +
              geom_histogram(bins = 6, color="white", fill=rgb(0,0.1,0.7,0.4)) +
              scale_x_continuous(labels=format_si()) +
              ggtitle("Amostra") +
              xlab("valor auditado") + ylab("qtd")
      print(h2)

      h3 <- ggplot(fsample[fsample$book.value != fsample$audit.value,], aes(x=(book.value - audit.value))) +
              geom_histogram(bins = 6, color="white", fill=rgb(0.7,0.1,0.1,0.4)) +
              scale_x_continuous(labels=format_si()) +
              ggtitle("Amostra") +
              xlab("distor\u{E7}\u{F5}es") + ylab("qtd")
      print(h3)
      h4 <- ggplot(pop, aes(x=book.value)) +
              geom_histogram(bins = 6, color="white", fill=rgb(0,0.7,0.1,0.4)) +
              pop.grid +
#              scale_x_log10(labels=format_si()) +
              scale_x_continuous(limits = quantile(pop$book.value, c(0.01, 0.99)), labels=format_si()) +
              ggtitle("Popula\u{E7}\u{E3}o") +
              xlab("valor informado") + ylab("qtd")
      suppressWarnings(print(h4))
      merge_histograms <- (length(unique(fsample$stratum)) > 1)
      if (merge_histograms) {
          h5 <- ggplot(fsample, aes(x=as.factor(stratum), y=book.value)) +
              geom_boxplot(fill=rgb(0,0.7,0.1,0.4), outlier.color=NA) +
              scale_y_continuous(limits = quantile(fsample$book.value, c(0.01, 0.99)), labels=format_si()) +
              ggtitle("Amostra") +
              xlab("estrato") + ylab("valor informado")
        suppressWarnings(print(h5))
          h6 <- ggplot(pop, aes(x=as.factor(stratum), y=book.value)) +
                  geom_boxplot(fill=rgb(0,0.7,0.1,0.4), outlier.color=NA) +
                  pop.grid +
                  scale_y_continuous(limits = quantile(pop$book.value, c(0.01, 0.99)), labels=format_si()) +
                  ggtitle("Popula\u{E7}\u{E3}o") +
                  xlab("estrato") + ylab("valor informado")
        suppressWarnings(print(h6))
      } else {
        if (nrow(trubricas) <= 20) {
            g1 <- ggplot(fsample, aes(x=as.factor(id.rubrica), y=book.value)) +
                    geom_boxplot(fill=rgb(0,0.7,0.1,0.4), outlier.color=NA) +
                    scale_y_continuous(limits = quantile(fsample$book.value, c(0.01, 0.99)), labels=format_si()) +
                    ggtitle("Amostra") +
                    xlab("rubrica") + ylab("valor informado")
            suppressWarnings(print(g1))
            g2 <- ggplot(pop, aes(x=as.factor(id.rubrica), y=book.value)) +
                    geom_boxplot(fill=rgb(0,0.7,0.1,0.4), outlier.color=NA) +
                    pop.grid +
                    scale_y_continuous(limits = quantile(pop$book.value, c(0.01, 0.99)), labels=format_si()) +
                    ggtitle("Popula\u{E7}\u{E3}o") +
                    xlab("rubrica") + ylab("valor informado")
            suppressWarnings(print(g2))
        } else {
          g1 <- ggplot(orubricas[order(-orubricas$value),], aes(x=1:nrow(orubricas), y=cumsum(value)/evaluation[[s]]$book.value)) +
                    geom_point(color=rgb(0,0.7,0.1,1), size=2.5) +
                    scale_y_continuous(labels=format_pct()) +
                    ggtitle("Amostra") +
                    xlab("rubricas") + ylab("valor informado acum.")
          print(g1)
          g2 <- ggplot(rubricas[order(-rubricas$value),], aes(x=1:nrow(rubricas), y=cumsum(value)/evaluation[[s]]$book.value)) +
                    geom_point(color=rgb(0,0.7,0.1,1), size=2.5) +
                    pop.grid +
                    scale_y_continuous(labels=format_pct()) +
                    ggtitle("Popula\u{E7}\u{E3}o") +
                    xlab("rubricas") + ylab("valor informado acum.")
          print(g2)
        }
      }

      par(op)
      cat("\n")

      if (nrow(trubricas)>9) {
        cat("\n\\newpage\n")
      }
      MUS:::.title("Rubricas", use.pander=TRUE, level=2)
      pandoc.table(trubricas, justify="clrrr")
      if (inclui_total & s==1) {
        MUS:::.title("Anexos", use.pander=TRUE, level=2)
        cat("\n- dados.csv")
        cat("\n- script.R")
        cat("\n- diagnostico.txt\n\n")
      }
    }

  }
}

selected <- dados$id[dados$selected==1]
highvalues <- dados$id[dados$selected==2]
#resultados

if ((MUS.step > 4) & file.exists('work.pdf')) {
  sink("diagnostico.txt")
  cat("Informações da Sessão\n\n")
  print(sessionInfo())
  cat("\n\nVersão do R\n\n")
  print(version)
  sink()
  write.csv(dados, file="dados.csv")
  pdftk("work.pdf", "attach_files dados.csv example.R diagnostico.txt", "report.pdf" )
  unlink(c("work.pdf", "dados.csv", "diagnostico.txt", "example.Rmd", "example.R", "logo.png"))
}

#moment.bound(c(rep(0, 96), -.16, .04, .18, .47))
#moment.bound(c(rep(0, 95), -75, -25, 25, 40, 60, 75)/100)
#moment.bound(c(rep(0, 96), 75, -60, -40, -25, 99)/100)
#moment.bound(c(rep(0, 93), -50, -50,50,50,50,50,50,50)/100)
