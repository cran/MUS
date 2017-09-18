print.MUS.evaluation.result <- function(x, ...){
	# Checking parameter
	if (class(x)!="MUS.evaluation.result") stop("x has to be an object from type MUS.evaluation.result. Use function MUS.evaluate to create such an object.")

	cat("MONETARY UNIT SAMPLING\n\n")
	if(x$acceptable) {
		cat(strwrap("The sample provides a reasonable basis to conclude that the population is free of material misstatements (given the parameters below)."), sep="\n")
	} else {
		cat(strwrap("The sample provides NO reasonable basis to conclude that the population is free of material misstatements (given the parameters below).\nYou have to get further audit evidence or extend the sample (currently not supported by this package)."), sep="\n")
	}
	cat("\n")
	cat(strwrap(paste0("The conclusion is based on a calculated Upper Error Limit of ",
			   round(x$Results.Total$Net.upper.error.limit["overstatements"]),
			   " for overstatements and ",
			   round(x$Results.Total$Net.upper.error.limit["understatements"]),
			   " for understatements (please be aware that MUS is not designed to detect understatements, thus they can only be used as an indicator).")), sep="\n")

	cat("\nMost important parameters:\n- Confidence Level:\t\t\t\t")
	cat(x$confidence.level)
	cat("\n- Tolerable Error (Materiality):\t\t")
	cat(x$tolerable.error)
	cat("\n- Population gross value:\t\t\t")
	cat(x$book.value)
	cat("\n- Expected Error in population:\t\t\t")
	cat(x$expected.error)
	cat("\n- Sample size:\t\t\t\t\t")
	cat(x$Results.Total$Total.number.of.items.examined)
	cat("\n- Threshold for individual significant items:\t")
	cat(round(x$High.value.threshold))

	cat("\n\nResults for high error rate evaluation:\n\n")
	if(x$high.error.rate$acceptable) {
		cat(strwrap("The sample provides a reasonable basis to conclude that the population is free of material misstatements (given the parameters below)."), sep="\n")
	} else {
		cat(strwrap("The sample provides NO reasonable basis to conclude that the population is free of material misstatements (given the parameters below).\nYou have to get further audit evidence or extend the sample (currently not supported by this package)."), sep="\n")
	}
	cat("\n")
	cat(strwrap(paste0("The conclusion is based on a calculated Upper Error Limit of ",
			   round(x$high.error.rate$upper.error.limit), ".")), sep="\n")

	cat("\n\nProjected Misstatement:\n")
	if(sum(x$Results.Total$Number.of.Errors)==0) {
		cat("No misstatements found. Thus, the projected misstatememt is 0.\n")
	} else {
		cat(strwrap(paste0("Based on ",
				   x$Results.Total$Number.of.Errors["overstatements"],
				   " overstatement differences and ",
				   x$Results.Total$Number.of.Errors["understatements"],
				   " understatement differences the netted Most Likely Error is ",
				   round(x$Results.Total$Net.most.likely.error[1]),
				" Monetary Units. You have to book the MLE if it is material.")), sep="\n")
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

	# Check book values for NAs, zeros or negative values and repeat warning from MUS.planning
	if (any(is.infinite(with(x$data, get(x$col.name.book.values))))) warning("There are missing or infinite values (NA, NaN or Inf) as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(x$data, get(x$col.name.book.values))==0)) warning("There are zeros as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(x$data, get(x$col.name.book.values))<0)) warning("There are negative values as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
}
