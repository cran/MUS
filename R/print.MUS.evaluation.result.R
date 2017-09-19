print.MUS.evaluation.result <- function(x, error.rate="both",
	print.misstatements=TRUE, print.planning=FALSE, print.extraction=FALSE, print.advice=TRUE, ...){
	# Checking parameter
	if (class(x)!="MUS.evaluation.result") stop("x has to be an object from type MUS.evaluation.result. Use function MUS.evaluate to create such an object.")

	if (error.rate=="low" || error.rate=="both" || (error.rate=="auto" && max(x$Results.Sample$Number.of.Errors)<20)) {
		cat("\nResults for Low Error Rate Evaluation:\n\n")
		if(x$acceptable) {
			cat(strwrap("The sample provides a reasonable basis to conclude that the population is free of material misstatements."), sep="\n")
		} else {
			cat(strwrap("The sample provides NO reasonable basis to conclude that the population is free of material misstatements."), sep="\n")
		}
		if(sum(x$Results.Total$Number.of.Errors)==0) {
			cat("\n- No misstatements found. Thus, the projected misstatememt is 0.")
		} else {
			cat("\n- Most Likely Error:\t\t\t\t", x$Results.Total$Net.most.likely.error[1])
			cat("\n- # of Overstatements:\t\t\t\t", x$Results.Total$Number.of.Errors["overstatements"])
			cat("\n- # of Understatements:\t\t\t\t", x$Results.Total$Number.of.Errors["understatements"])
		}
		cat("\n- Upper Error Limit:\t\t\t\t", round(max(x$Results.Total$Net.upper.error.limit*c(1,-1))))
		cat("\n- Upper Error Limit (Overstatements):\t\t", round(x$Results.Total$Net.upper.error.limit["overstatements"]))
		cat("\n- Upper Error Limit (Understatements):\t\t", round(x$Results.Total$Net.upper.error.limit["understatements"]), "\n")

		if (print.advice) {
			if(!x$acceptable) {
				cat("\n** You have to get further audit evidence or extend the sample (currently not supported by this package).")
				cat("\n** You have to book the MLE if it is material.")
			}
			if (max(x$Results.Sample$Number.of.Errors)>=20) {
				cat("\n** You had at least 20 errors in the sample. High Error Rate evaluation recommended.")
			}

			cat("\n** Please be aware that MUS is not designed to detect understatements, thus they can only be used as an indicator.")
		}

		if (print.extraction) {
			print.MUS.extraction.result(x, print.planning=print.planning)
		}

		if(print.misstatements && sum(x$Results.Total$Number.of.Errors) > 0) {
			cat("\nFactual Misstatements:\n")
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
	}
	if (error.rate=="high" || error.rate=="both" || (error.rate=="auto" && max(x$Results.Sample$Number.of.Errors)>=20)) {
		cat("\n\nResults for High Error Rate Evaluation:\n\n")
		if(x$high.error.rate$acceptable) {
			cat(strwrap("The sample provides a reasonable basis to conclude that the population is free of material misstatements."), sep="\n")
		} else {
			cat(strwrap("The sample provides NO reasonable basis to conclude that the population is free of material misstatements."), sep="\n")
		}
		if(sum(x$Results.Total$Number.of.Errors)==0) {
			cat("\n- No misstatements found. Thus, the projected misstatememt is 0.")
		} else {
			cat("\n- Most Likely Error:\t\t\t\t", x$high.error.rate$most.likely.error)
			cat("\n- Upper Error Limit:\t\t\t\t", x$high.error.rate$upper.error.limit, "\n")
		}
		if (print.advice) {
			if(!x$acceptable) {
				cat("\n** You have to get further audit evidence or extend the sample (currently not supported by this package).")
				cat("\n** You have to book the MLE if it is material.")
			}
			if (max(x$Results.Sample$Number.of.Errors) < 20) {
				message("\n** You had less than 20 errors in the sample. Low Error Rate evaluation recommended.")
			}
		}
	}
	# Check book values for NAs, zeros or negative values and repeat warning from MUS.planning
	if (any(is.infinite(with(x$data, get(x$col.name.book.values))))) warning("There are missing or infinite values (NA, NaN or Inf) as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(x$data, get(x$col.name.book.values))==0)) warning("There are zeros as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(x$data, get(x$col.name.book.values))<0)) warning("There are negative values as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
}
