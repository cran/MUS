print.MUS.evaluation.result <- function(x, error.rate="auto",
	print.misstatements=TRUE, print.planning=FALSE, print.extraction=FALSE, print.error.as.pct=TRUE, print.advice=TRUE,
	style="default", use.pander=FALSE, ...){
	# Checking parameter
	dm <- "R-MUS"
	bindtextdomain(dm)
	if (class(x)!="MUS.evaluation.result") stop("x has to be an object from type MUS.evaluation.result. Use function MUS.evaluate to create such an object.")
	x$error.as.pct <- print.error.as.pct
	.title(gettext("Evaluation Results", domain=dm), use.pander=use.pander)
	res <- list()
	if (print.extraction) {
		print.MUS.extraction.result(x, print.planning=print.planning, use.pander=use.pander)
	}
	if(sum(x$Results.Total$Number.of.Errors)==0) {
		cat("\n-", gettext("No misstatements found. Thus, the projected misstatememt is 0.", domain=dm))
	} else {
		sample.misstatements <- x$filled.sample[,x$col.name.book.values] - x$filled.sample[,x$col.name.audit.values]
		res$sample.book.value <- sum(x$filled.sample[,x$col.name.book.values])
		res$audited.over.qty <- x$Results.Total$Number.of.Errors["overstatements"]
		res$audited.under.qty <- x$Results.Total$Number.of.Errors["understatements"]
		res$sample.over.qty <- x$Results.Sample$Number.of.Errors["overstatements"]
		res$sample.under.qty <- x$Results.Sample$Number.of.Errors["understatements"]

		res$sample.over.value <- sum(sample.misstatements[sample.misstatements>0])
		res$sample.under.value <- sum(sample.misstatements[sample.misstatements<0])
		res$sample.over.rate <- .percent(res$sample.over.value / res$sample.book.value)
		res$sample.under.rate <- .percent(res$sample.under.value / res$sample.book.value)

		res$audited.over.uel <- x$Results.Total$Net.upper.error.limit["overstatements"]
		res$audited.under.uel <- x$Results.Total$Net.upper.error.limit["understatements"]
		res$sample.over.uel <- x$Results.Sample$Net.upper.error.limit["overstatements"]
		res$sample.under.uel <- x$Results.Sample$Net.upper.error.limit["understatements"]

		res$sample.miss.qty <- max(x$Results.Sample$Number.of.Errors)
		res$sample.miss.value <- sum(sample.misstatements)
		res$sample.miss.rate <- .percent(res$sample.miss.value/res$sample.book.value)
		population.value <- x$book.value
		if (class(x$filled.high.values)=="data.frame") {
			high.misstatements <- x$filled.high.values[,x$col.name.book.values] - x$filled.high.values[,x$col.name.audit.values]
			res$high.book.value <- sum(x$filled.high.values[,x$col.name.book.values])
			res$high.miss.qty <- sum(high.misstatements != 0)
			res$high.miss.value <- sum(high.misstatements)
			#population.value <- x$book.value - sum(x$filled.high.values[,x$col.name.book.values])
			res$high.over.value <- sum(high.misstatements[high.misstatements>0])
			res$high.under.value <- sum(high.misstatements[high.misstatements<0])
			res$high.over.qty <- x$Results.High.values$Number.of.Errors["overstatements"]
			res$high.under.qty <- x$Results.High.values$Number.of.Errors["understatements"]
			res$audited.over.value <- res$sample.over.value + res$high.over.value
			res$audited.under.value <- res$sample.under.value + res$high.under.value
		} else {
			#population.value <- x$book.value
			res$high.book.value <- 0
			res$high.miss.qty <- 0
			res$high.miss.value <- 0
			res$audited.over.value <- res$sample.over.value
			res$audited.under.value <- res$sample.under.value
		}
		res$audited.miss.qty <- res$sample.miss.qty + res$high.miss.qty
		res$audited.miss.value <- res$sample.miss.value + res$high.miss.value
		res$audited.book.value <- res$sample.book.value + res$high.book.value
		res$audited.over.rate <- .percent(res$audited.over.value / res$audited.book.value)
		res$audited.under.rate <- .percent(res$audited.under.value / res$audited.book.value)

		res$high.miss.rate <- ifelse(res$high.book.value>0, .percent(res$high.miss.value/res$high.book.value), "-")
		res$audited.miss.rate <- .percent(res$audited.miss.value/res$audited.book.value)
		res$most.likely.error.value <- x$Results.Total$Net.most.likely.error[1]
		res$most.likely.error.rate <- .percent(res$most.likely.error.value / population.value)
		res$tainting.order <- x$tainting.order
		res$UEL.lowrate.value <- x$UEL.low.error.rate
		res$UEL.lowrate.rate <- .percent(res$UEL.lowrate.value / population.value)
		res$UEL.highrate.value <- x$UEL.high.error.rate
		res$UEL.highrate.rate <- .percent(res$UEL.highrate.value / population.value)

		res$MLE.lowrate.value <- x$MLE.low.error.rate
		res$MLE.lowrate.rate <- .percent(res$MLE.lowrate.value / population.value)
		res$MLE.highrate.value <- x$MLE.high.error.rate
		res$MLE.highrate.rate <- .percent(res$MLE.highrate.value / population.value)
		res$MLE.final.value <- x$MLE.final
		res$MLE.final.rate <- .percent(res$MLE.final.value / population.value)

		tbl <- matrix(nrow=11, ncol=4)
		tbl[1,] = c(gettext("Audited Misstatements", domain=dm), res$audited.miss.qty , .value(res$audited.miss.value), res$audited.miss.rate)
		tbl[2,] = c(gettext("Audited Overstatements", domain=dm), res$audited.over.qty , .value(res$audited.over.value), res$audited.over.rate)
		tbl[3,] = c(gettext("Audited Understatements", domain=dm), res$audited.under.qty , .value(res$audited.under.value), res$audited.under.rate)
		tbl[4,] = c(gettext("Sample Misstatements", domain=dm), res$sample.miss.qty , .value(res$sample.miss.value), res$sample.miss.rate)
		tbl[5,] = c(gettext("High Value Misstatements", domain=dm), res$high.miss.qty , .value(res$high.miss.value), res$high.miss.rate)
		tbl[7,] = c(gettext("UEL (Low Error Rate)", domain=dm), "-" , .value(res$UEL.lowrate.value), res$UEL.lowrate.rate)
		tbl[8,] = c(gettext("UEL (High Error Rate)", domain=dm), "-" , .value(res$UEL.highrate.value), res$UEL.highrate.rate)
		if (res$sample.miss.qty > 20) {
			tbl[6,] = c(gettext("Upper Error Limit (Final)", domain=dm), "-" , .value(res$UEL.highrate.value), res$UEL.highrate.rate)
		} else {
			tbl[6,] = c(gettext("Upper Error Limit (Final)", domain=dm), "-" , .value(res$UEL.lowrate.value), res$UEL.lowrate.rate)
		}
#		tbl[9,] = c(gettext("Most Likely Error", domain=dm), "-" , .value(res$most.likely.error.value), res$most.likely.error.rate)
		tbl[9,] = c(gettext("Most Likely Error", domain=dm), "-" , .value(res$MLE.final.value), res$MLE.final.rate)
		tbl[10,] = c(gettext("MLE (Low Error Rate)", domain=dm), "-" , .value(res$MLE.lowrate.value), res$MLE.lowrate.rate)
		tbl[11,] = c(gettext("MLE (High Error Rate)", domain=dm), "-" , .value(res$MLE.highrate.value), res$MLE.highrate.rate)
#		tbl[2,] <- Vectorize(.italic)(tbl[2,])
#		tbl[3,] <- Vectorize(.italic)(tbl[3,])
#		tbl[7,] <- Vectorize(.italic)(tbl[7,])
#		tbl[8,] <- Vectorize(.italic)(tbl[8,])
		tbl[6,] <- Vectorize(.bold)(tbl[6,])
		tbl[9,] <- Vectorize(.bold)(tbl[9,])
		tbl[1,] <- Vectorize(.bold)(tbl[1,])
		colnames(tbl) <- c(paste0(c(gettext("Description", domain=dm), rep("&nbsp;",6)), collapse=""),
			gettext("Items", domain=dm), gettext("Value", domain=dm), "%")
		x$tbl <- tbl
		if (style=="report") {
			pander::pandoc.table(x$tbl, digits=2, justify="lrrr", split.tables=Inf, keep.trailing.zeros=TRUE)
		} else {
			cat("\n-", .f(gettext("Number of Overstatements", domain=dm)), res$sample.over.qty)
			cat("\n-", .f(gettext("Number of Understatements", domain=dm)), res$sample.under.qty)
			cat("\n-", .f(gettext("Sample Misstatement Amount", domain=dm)), res$sample.miss.value, "(", res$sample.miss.rate , ")")
			cat("\n-", .f(gettext("High Values Misstatement Amount", domain=dm)), res$high.miss.value, "(", res$high.miss.rate, ")")
			cat("\n-", .f(gettext("Audited Misstatement Amount", domain=dm)), res$audited.miss.value, "(", res$audited.miss.rate, ")")
#			cat("\n-", .f(gettext("Most Likely Error", domain=dm)), .write.UEL(x, res$most.likely.error.value))
			cat("\n-", .f(gettext("Most Likely Error", domain=dm)), .write.UEL(x, res$most.likely.error.value))
			if (res$tainting.order != "decreasing") {
				cat("\n-", gettext("Tainting Order", domain=dm), res$tainting.order)
			}
			if (error.rate=="low" || error.rate=="both" || res$sample.miss.qty<20) {
				cat("\n-", .f(gettext("Upper Error Limit (Low Error Rate)", domain=dm)), .write.UEL(x, x$UEL.low.error.rate),
					.is.acceptable(x$acceptable.low.error.rate))
				if (res$sample.over.qty>0 && res$sample.under.qty>0) {
					cat("\n-", .f(gettext("Upper Error Limit (Overstatements)", domain=dm)), .write.UEL(x, round(res$sample.over.uel)))
					cat("\n-", .f(gettext("Upper Error Limit (Understatements)", domain=dm)), .write.UEL(x, round(res$sample.under.uel)))
				}
				cat("\n-", .f(gettext("UEL Acceptable (Low Error Rate)", domain=dm)), .msg.acceptable(x$acceptable.low.error.rate))
			}
			if (error.rate=="high" || error.rate=="both" || max(x$Results.Sample$Number.of.Errors)>=20) {
				cat("\n-", .f(gettext("Upper Error Limit (High Error Rate)", domain=dm)), .write.UEL(x, x$high.error.rate$upper.error.limit), .is.acceptable(x$acceptable.high.error.rate))
				cat("\n-", .f(gettext("UEL Acceptable (High Error Rate)", domain=dm)), .msg.acceptable(x$acceptable.high.error.rate))
			}
			if ("moment.bound" %in% names(x)) {
				cat("\n-", .f(gettext("Upper Error Limit (Moment Bound)", domain=dm)), .write.UEL(x, x$moment.bound), .is.acceptable(x$acceptable.moment.bound))
			}
			if ("binomial.bound" %in% names(x)) {
				cat("\n-", .f(gettext("Upper Error Limit (Binomial Bound)", domain=dm)), .write.UEL(x, x$binomial.bound), .is.acceptable(x$acceptable.binomial.bound))
			}
			if ("multinomial.bound" %in% names(x)) {
				cat("\n-", .f(gettext("Upper Error Limit (Multinomial Bound)", domain=dm)), .write.UEL(x, x$multinomial.bound), .is.acceptable(x$acceptable.multinomial.bound))
			}

		}

	}
	if (print.misstatements && sum(x$Results.Total$Number.of.Errors) > 0) {
		cat(paste0("\n\n", gettext("Factual Misstatements", domain=dm), "\n"))
		if (is.data.frame(x$filled.sample) | is.matrix(x$filled.sample)) {
			factual <- subset(x$filled.sample, with(x, filled.sample[,col.name.audit.values]!=filled.sample[,col.name.book.values]))
			if(nrow(factual)>0) {
				print(factual)
			}
		}
		if (is.data.frame(x$filled.high.values) | is.matrix(x$filled.high.values)) {
			factual <- subset(x$filled.high.values, with(x, filled.high.values[,col.name.audit.values]!=filled.high.values[,col.name.book.values]))
			if(nrow(factual)>0) {
				print(factual)
			}
		}
	}


	if (print.advice) {
		advised <- FALSE
		dm <- "R-MUS"
		if(!x$acceptable) {
			advised <- .write.advice.title(advised, use.pander=use.pander)
			if (x$combined) {
				if (x$qty.accepted > 0) {
					cat("\n*", gettext("Some strata are acceptable.", domain=dm))
				} else {
					cat("\n*", gettext("No strata are acceptable.", domain=dm))
				}
				cat("\n*", gettext("You have to get further audit evidence or extend the sample.", domain=dm))
				cat("\n*", gettext("You have to book the MLE if it is material.", domain=dm))
			} else {
				cat("\n*", gettext("Stratum results are not acceptable.", domain=dm))
				cat("\n*", gettext("You have to get further audit evidence or extend the sample.", domain=dm))
				cat("\n*", gettext("You have to book the MLE if it is material.", domain=dm))
			}
		} else {
			if (x$combined) {
				cat("\n*", gettext("All strata results are acceptable.", domain=dm))
			} else {
				cat("\n*", gettext("Stratum results are acceptable.", domain=dm))
			}
			cat("\n*", gettext("Audit evidence is sufficient.", domain=dm))
		}
		if ((error.rate=="high" || error.rate=="both") && max(x$Results.Sample$Number.of.Errors) < 20) {
			advised <- .write.advice.title(advised, use.pander=use.pander)
			cat("\n*", gettext("You had less than 20 errors in the sample. Low Error Rate evaluation recommended.", domain=dm))
		}
		if ((error.rate=="low" || error.rate=="both") && max(x$Results.Sample$Number.of.Errors)>=20) {
			advised <- .write.advice.title(advised, use.pander=use.pander)
			cat("\n*", gettext("You had at least 20 errors in the sample. High Error Rate evaluation recommended.", domain=dm))
		}
		if (x$Results.Total$Number.of.Errors["understatements"]>0) {
			advised <- .write.advice.title(advised, use.pander=use.pander)
			cat("\n*", gettext("Please be aware that MUS is not designed to detect understatements, thus they can only be used as an indicator.", domain=dm))
		}
	}
	cat("\n")
	# Check book values for NAs, zeros or negative values and repeat warning from MUS.planning
	if (any(is.infinite(with(x$data, get(x$col.name.book.values))))) warning("There are missing or infinite values (NA, NaN or Inf) as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(x$data, get(x$col.name.book.values))==0)) warning("There are zeros as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(x$data, get(x$col.name.book.values))<0)) warning("There are negative values as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
}

.write.advice.title <- function(already.printed=FALSE, use.pander=use.pander) {
	dm <- "R-MUS"
	if (!already.printed) {
		cat("\n")
		.title(gettext("Recommendations", domain=dm), use.pander=use.pander)
		already.printed <- TRUE
	}
	already.printed
}

.is.acceptable <- function(x) {
	ifelse(x, "*", "")
}
.msg.acceptable <- function(x) {
	ifelse(x, gettext("Yes"), gettext("No"))
}

.write.UEL <- function(x, y, digits=2, format="f", ...) {
	#population.value <- x$book.value - ifelse(is.data.frame(x$filled.high.values), sum(x$filled.high.values[,x$col.name.book.values]), 0)
	population.value <- x$book.value
	ifelse(x$error.as.pct, paste0(formatC(100 * y / population.value, format=format, digits=digits, ...), "%"), y)
}

.percent <- function(x, digits = 2, format = "f", ...) {
  # paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  ifelse(is.numeric(x), formatC(100 * x, format = format, digits = digits, ...), "-")
}
.value <- function(x, digits=2, big.mark=NULL, decimal.mark=getOption("OutDec"), ...) {
  # paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  big.mark = ifelse(is.null(big.mark), ifelse(decimal.mark==".", ",", "."), big.mark)
  ifelse(is.numeric(x), format(round(x, digits), nsmall=digits, big.mark=big.mark, decimal.mark=decimal.mark, ...), "-")
}
.title <- function(x, use.pander=FALSE, level=2) {
    if (use.pander && requireNamespace("pander", quietly = TRUE)) {
		pander::pandoc.header(x, level=level)
	} else {
		cat(paste0("\n", x, "\n"))
	}
}

.italic <- function(x) {
	paste0("_",x,"_")
}

.bold <- function(x) {
	paste0("__",x,"__")
}

.f <- function(x) {
	sprintf("%-40s", x)
}