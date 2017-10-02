print.MUS.evaluation.result <- function(x, error.rate="auto",
	print.misstatements=TRUE, print.planning=FALSE, print.extraction=FALSE, print.error.as.pct=TRUE, print.advice=TRUE,
	style="default", use.pander=FALSE, ...){
	# Checking parameter
	if (class(x)!="MUS.evaluation.result") stop("x has to be an object from type MUS.evaluation.result. Use function MUS.evaluate to create such an object.")
	x$error.as.pct <- print.error.as.pct
	mus.title("Evaluation Results", use.pander=use.pander)
	res <- list()
	if (print.extraction) {
		print.MUS.extraction.result(x, print.planning=print.planning, use.pander=use.pander)
	}
	if(sum(x$Results.Total$Number.of.Errors)==0) {
		cat("\n- No misstatements found. Thus, the projected misstatememt is 0.")
	} else {
		population.value <- x$book.value - sum(x$filled.high.values[,x$col.name.book.values])
		sample.misstatements <- x$filled.sample[,x$col.name.book.values] - x$filled.sample[,x$col.name.audit.values]
		res$sample.book.value <- sum(x$filled.sample[,x$col.name.book.values])
		res$sample.over.qty <- x$Results.Total$Number.of.Errors["overstatements"]
		res$sample.under.qty <- x$Results.Total$Number.of.Errors["understatements"]
		res$sample.over.value <- sum(sample.misstatements[sample.misstatements>0])
		res$sample.under.value <- sum(sample.misstatements[sample.misstatements<0])
		res$sample.over.rate <- percent(res$sample.over.value / res$sample.book.value)
		res$sample.under.rate <- percent(res$sample.under.value / res$sample.book.value)
		res$sample.over.uel <- x$Results.Total$Net.upper.error.limit["overstatements"]
		res$sample.under.uel <- x$Results.Total$Net.upper.error.limit["understatements"]
		res$sample.miss.qty <- max(x$Results.Sample$Number.of.Errors)
		res$sample.miss.value <- sum(sample.misstatements)
		res$sample.miss.rate <- percent(res$sample.miss.value/res$sample.book.value)

		res$high.book.value <- sum(x$filled.high.values[,x$col.name.book.values])
		res$high.miss.qty <- sum(x$filled.high.values[,x$col.name.book.values] != x$filled.high.values[,x$col.name.audit.values])
		res$high.miss.value <- sum(x$filled.high.values[,x$col.name.book.values] - x$filled.high.values[,x$col.name.audit.values])
		res$high.miss.rate <-percent(res$high.miss.value/res$high.book.value)
		res$audited.miss.qty <- res$sample.miss.qty + res$high.miss.qty
		res$audited.miss.value <- res$sample.miss.value + res$high.miss.value
		res$audited.book.value <- res$sample.book.value + res$high.book.value
		res$audited.miss.rate <-percent(res$audited.miss.value/res$audited.book.value)
		res$most.likely.error.value <- x$Results.Total$Net.most.likely.error[1]
		res$most.likely.error.rate <- percent(res$most.likely.error.value / population.value)
		res$tainting.order <- x$tainting.order
		res$UEL.lowrate.value <- x$UEL.low.error.rate
		res$UEL.lowrate.rate <- percent(res$UEL.lowrate.value / population.value)
		res$UEL.highrate.value <- x$UEL.high.error.rate
		res$UEL.highrate.rate <- percent(res$UEL.highrate.value / population.value)

		tbl <- matrix(nrow=8, ncol=4)
		tbl[1,] = c("Sample Misstatements", res$sample.miss.qty , value(res$sample.miss.value), res$sample.miss.rate)
		tbl[2,] = c("Sample Overstatements", res$sample.over.qty , value(res$sample.over.value), res$sample.over.rate)
		tbl[3,] = c("Sample Understatements", res$sample.under.qty , value(res$sample.under.value), res$sample.under.rate)
		tbl[4,] = c("High Value Misstatements", res$high.miss.qty , value(res$high.miss.value), res$high.miss.rate)
		tbl[5,] = c("Audited Misstatements", res$audited.miss.qty , value(res$audited.miss.value), res$audited.miss.rate)
		tbl[6,] = c("Most Likely Error", "-" , value(res$most.likely.error.value), bold(res$most.likely.error.rate))
		if (res$sample.miss.qty > 20) {
			tbl[7,] = c("Upper Error Limit (Low Error Rate)", "-" , value(res$UEL.lowrate.value), res$UEL.lowrate.rate)
			tbl[8,] = c("Upper Error Limit (High Error Rate)", "*" , value(res$UEL.highrate.value), bold(res$UEL.highrate.rate))
		} else {
			tbl[7,] = c("Upper Error Limit (Low Error Rate)", "*", value(res$UEL.lowrate.value), bold(res$UEL.lowrate.rate))
			tbl[8,] = c("Upper Error Limit (High Error Rate)", "-" , value(res$UEL.highrate.value), res$UEL.highrate.rate)
		}
		colnames(tbl) <- c(paste0(c("Description", rep("&nbsp;",6)), collapse=""), "Items", "Value", "%")
		x$tbl <- rbind(x$tbl, tbl)
		if (style=="report") {
			pandoc.table(x$tbl, digits=2, justify="lrrr", split.tables=Inf, keep.trailing.zeros=TRUE)
		} else {
			cat("\n- Number of Overstatements:\t\t\t", res$sample.over.qty)
			cat("\n- Number of Understatements:\t\t\t", res$sample.under.qty)
			cat("\n- Sample Misstatement Amount:\t\t\t", res$sample.miss.value, "(", res$sample.miss.rate , ")")
			cat("\n- High Values Misstatement Amount:\t\t", res$high.miss.value, "(", res$high.miss.rate, ")")
			cat("\n- Audited Misstatement Amount:\t\t\t", res$audited.miss.value, "(", res$audited.miss.rate, ")")
			cat("\n- Most Likely Error:\t\t\t\t", print.UEL(x, res$most.likely.error.value))
			if (res$tainting.order != "decreasing") {
				cat("\n- Tainting Order:\t\t\t\t", res$tainting.order)
			}
			if (error.rate=="low" || error.rate=="both" || res$sample.miss.qty<20) {
				cat("\n- Upper Error Limit (Low Error Rate):\t\t", print.UEL(x, x$UEL.low.error.rate),
					is.acceptable(x$acceptable.low.error.rate))
				if (res$sample.over.qty>0 && res$sample.under.qty>0) {
					cat("\n- Upper Error Limit (Overstatements):\t\t", print.UEL(x, round(res$sample.over.uel)))
					cat("\n- Upper Error Limit (Understatements):\t\t", print.UEL(x, round(res$sample.under.uel)))
				}
				cat("\n- UEL Acceptable (Low Error Rate):\t\t", text.acceptable(x$acceptable.low.error.rate))
			}
			if (error.rate=="high" || error.rate=="both" || max(x$Results.Sample$Number.of.Errors)>=20) {
				cat("\n- Upper Error Limit (High Error Rate):\t\t", print.UEL(x, x$high.error.rate$upper.error.limit), is.acceptable(x$acceptable.high.error.rate))
				cat("\n- UEL Acceptable (High Error Rate):\t\t", text.acceptable(x$acceptable.high.error.rate))
			}
			if ("moment.bound" %in% names(x)) {
				cat("\n- Upper Error Limit (Moment Bound):\t\t", print.UEL(x, x$moment.bound), is.acceptable(x$acceptable.moment.bound))
			}
			if ("binomial.bound" %in% names(x)) {
				cat("\n- Upper Error Limit (Binomial Bound):\t\t", print.UEL(x, x$binomial.bound), is.acceptable(x$acceptable.binomial.bound))
			}
			if ("multinomial.bound" %in% names(x)) {
				cat("\n- Upper Error Limit (Multinomial Bound):\t", print.UEL(x, x$multinomial.bound), is.acceptable(x$acceptable.multinomial.bound))
			}

		}

	}
	if (print.misstatements && sum(x$Results.Total$Number.of.Errors) > 0) {
		cat("\n\nFactual Misstatements:\n")
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
		if(!x$acceptable) {
			advised <- print.advice.title(advised, use.pander=use.pander)
			cat("\n* You have to get further audit evidence or extend the sample.")
			cat("\n* You have to book the MLE if it is material.")
		} else {
			cat("\n* Audit evidence is sufficient. Results are acceptable.")
		}
		if ((error.rate=="high" || error.rate=="both") && max(x$Results.Sample$Number.of.Errors) < 20) {
			advised <- print.advice.title(advised, use.pander=use.pander)
			message("\n* You had less than 20 errors in the sample. Low Error Rate evaluation recommended.")
		}
		if ((error.rate=="low" || error.rate=="both") && max(x$Results.Sample$Number.of.Errors)>=20) {
			advised <- print.advice.title(advised, use.pander=use.pander)
			cat("\n* You had at least 20 errors in the sample. High Error Rate evaluation recommended.")
		}
		if (x$Results.Total$Number.of.Errors["understatements"]>0) {
			advised <- print.advice.title(advised, use.pander=use.pander)
			cat("\n* Please be aware that MUS is not designed to detect understatements, thus they can only be used as an indicator.")
		}
	}
	cat("\n")
	# Check book values for NAs, zeros or negative values and repeat warning from MUS.planning
	if (any(is.infinite(with(x$data, get(x$col.name.book.values))))) warning("There are missing or infinite values (NA, NaN or Inf) as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(x$data, get(x$col.name.book.values))==0)) warning("There are zeros as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(x$data, get(x$col.name.book.values))<0)) warning("There are negative values as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
}

print.advice.title <- function(already.printed=FALSE, use.pander=use.pander) {
	if (!already.printed) {
		cat("\n")
		mus.title("Recommendations", use.pander=use.pander)
		already.printed <- TRUE
	}
	already.printed
}

is.acceptable <- function(x) {
	ifelse(x, "*", "")
}
text.acceptable <- function(x) {
	ifelse(x, "Yes", "No")
}

print.UEL <- function(x, y, digits=2, format="f", ...) {
	population.value <- x$book.value - sum(x$filled.high.values[,x$col.name.book.values])
	ifelse(x$error.as.pct, paste0(formatC(100 * y / population.value, format=format, digits=digits, ...), "%"), y)
}

percent <- function(x, digits = 2, format = "f", ...) {
  # paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  formatC(100 * x, format = format, digits = digits, ...)
}
value <- function(x, digits=2, big.mark=NULL, decimal.mark=getOption("OutDec"), ...) {
  # paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  if (is.null(big.mark)) {
	  big.mark = ifelse(decimal.mark==".", ",", ".")
  }
  format(round(x, digits), nsmall=digits, big.mark=big.mark, decimal.mark=decimal.mark, ...)
}
mus.title <- function(x, use.pander=FALSE, level=2) {
	if (use.pander && require("pander")) {
		pandoc.header(x, level=level)
	} else {
		cat(paste0("\n", x, "\n"))
	}
}

bold <- function(x) {
	paste0("**",x,"**")
}