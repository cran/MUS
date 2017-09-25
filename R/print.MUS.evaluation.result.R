print.MUS.evaluation.result <- function(x, error.rate="both",
	print.misstatements=TRUE, print.planning=FALSE, print.extraction=FALSE, print.error.as.pct=TRUE, print.advice=TRUE, ...){
	# Checking parameter
	if (class(x)!="MUS.evaluation.result") stop("x has to be an object from type MUS.evaluation.result. Use function MUS.evaluate to create such an object.")
	x$error.as.pct <- print.error.as.pct
	cat("\nEvaluation Results\n")
	if(sum(x$Results.Total$Number.of.Errors)==0) {
		cat("\n- No misstatements found. Thus, the projected misstatememt is 0.")
	} else {
		if (error.rate=="low" || error.rate=="both" || (error.rate=="auto" && max(x$Results.Sample$Number.of.Errors)<20)) {
			population.value <- x$book.value - sum(x$filled.high.values[,x$col.name.book.values])
			cat("\n- Number of Missstatements:\t\t\t", x$Results.Total$Number.of.Errors["overstatements"],"overstatements,",
				x$Results.Total$Number.of.Errors["understatements"], "understatements")
			cat("\n- Sample Misstatement Rate (Amount):\t\t",
				percent((1-sum(x$filled.sample[,x$col.name.audit.values]) / sum(x$filled.sample[,x$col.name.book.values]))), " \t(",
				sum(x$filled.sample[,x$col.name.book.values] - x$filled.sample[,x$col.name.audit.values]), ")")
			cat("\n- High Values Misstatement Rate (Amount):\t",
				percent((1-sum(x$filled.high.values[,x$col.name.audit.values]) / sum(x$filled.high.values[,x$col.name.book.values]))), " \t(",
				sum(x$filled.high.values[,x$col.name.book.values] - x$filled.high.values[,x$col.name.audit.values]), ")")
			cat("\n- Audited Misstatement Rate (Amount):\t\t",
				percent((sum(x$filled.high.values[,x$col.name.book.values])+sum(x$filled.sample[,x$col.name.book.values])) /
					 (sum(x$filled.high.values[,x$col.name.audit.values])+sum(x$filled.sample[,x$col.name.audit.values]))-1), " \t(",
				sum(x$filled.sample[,x$col.name.book.values] - x$filled.sample[,x$col.name.audit.values]) +
				sum(x$filled.high.values[,x$col.name.book.values] - x$filled.high.values[,x$col.name.audit.values]), ")")

			cat("\n- Most Likely Error:\t\t\t\t", print.UEL(x, x$Results.Total$Net.most.likely.error[1]))
			cat("\n- Tainting Order:\t\t\t\t", toupper(x$tainting.order))
			cat("\n- Upper Error Limit (Low Error Rate):\t\t", print.UEL(x, x$UEL.low.error.rate), "\t", is.acceptable(x$acceptable.low.error.rate))
			if (x$Results.Total$Number.of.Errors["overstatements"]>0 && x$Results.Total$Number.of.Errors["understatements"]>0) {
				cat("\n- Upper Error Limit (Overstatements):\t\t", print.UEL(x, round(x$Results.Total$Net.upper.error.limit["overstatements"])))
				cat("\n- Upper Error Limit (Understatements):\t\t", print.UEL(x, round(x$Results.Total$Net.upper.error.limit["understatements"])), "\n")
			}
			if (print.extraction) {
				print.MUS.extraction.result(x, print.planning=print.planning)
			}
		}
		if (error.rate=="high" || error.rate=="both" || (error.rate=="auto" && max(x$Results.Sample$Number.of.Errors)<20)) {
			cat("\n- Upper Error Limit (High Error Rate):\t\t", print.UEL(x, x$high.error.rate$upper.error.limit), "\t", is.acceptable(x$acceptable.high.error.rate))
		}
		cat("\n- Upper Error Limit (Moment Bound):\t\t", print.UEL(x, x$moment.bound), "\t", is.acceptable(x$acceptable.moment.bound))
		cat("\n- Upper Error Limit (Binomial Bound):\t\t", print.UEL(x, x$binomial.bound), "\t", is.acceptable(x$acceptable.binomial.bound))
		cat("\n- Upper Error Limit (Multinomial Bound):\t", print.UEL(x, x$multinomial.bound), "\t", is.acceptable(x$acceptable.multinomial.bound))

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
			advised <- print.advice.title(advised)
			cat("\n* You have to get further audit evidence or extend the sample (currently not supported by this package).")
			cat("\n* You have to book the MLE if it is material.")
		}
		if ((error.rate=="high" || error.rate=="both") && max(x$Results.Sample$Number.of.Errors) < 20) {
			advised <- print.advice.title(advised)
			message("\n* You had less than 20 errors in the sample. Low Error Rate evaluation recommended.")
		}
		if ((error.rate=="low" || error.rate=="both") && max(x$Results.Sample$Number.of.Errors)>=20) {
			advised <- print.advice.title(advised)
			cat("\n* You had at least 20 errors in the sample. High Error Rate evaluation recommended.")
		}
		if (x$Results.Total$Number.of.Errors["understatements"]>0) {
			advised <- print.advice.title(advised)
			cat("\n* Please be aware that MUS is not designed to detect understatements, thus they can only be used as an indicator.")
		}
	}

	# Check book values for NAs, zeros or negative values and repeat warning from MUS.planning
	if (any(is.infinite(with(x$data, get(x$col.name.book.values))))) warning("There are missing or infinite values (NA, NaN or Inf) as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(x$data, get(x$col.name.book.values))==0)) warning("There are zeros as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(x$data, get(x$col.name.book.values))<0)) warning("There are negative values as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
}

print.advice.title <- function(already.printed=FALSE) {
	if (!already.printed) {
		cat("\n\nRecommendations\n")
		already.printed <- TRUE
	}
	already.printed
}

is.acceptable <- function(x) {
	ifelse(x, "Acceptable", "Not Acceptable")
}

print.UEL <- function(x, y, digits=2, format="f", ...) {
	population.value <- x$book.value - sum(x$filled.high.values[,x$col.name.book.values])
	ifelse(x$error.as.pct, paste0(formatC(100 * y / population.value, format=format, digits=digits, ...), "%"), y)
}

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
