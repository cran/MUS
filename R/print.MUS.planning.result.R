print.MUS.planning.result <- function(x, print.title=TRUE, style="default", use.pander=FALSE, ...){
	# Checking parameter
	if (class(x)!="MUS.extraction.result" && class(x)!="MUS.evaluation.result" && class(x)!="MUS.planning.result") {
		stop("x has to be an object from type MUS.extraction.result or MUS.evaluation.result or MUS.planning.result.")
	}
	dm <- "R-MUS"
	bindtextdomain(dm)
	if (print.title) {
		.title(gettext("Planning Parameters", domain=dm), use.pander=use.pander)
	}
	tbl <- matrix(nrow=4, ncol=4)
	tbl[1,] = c(gettext("Expected Error", domain=dm), "-", .value(x$expected.error), .percent(x$expected.error / x$book.value))
	tbl[2,] = c(gettext("Tolerable Error (Materiality)", domain=dm), "-", .value(x$tolerable.error), .percent(x$tolerable.error / x$book.value))
	tbl[3,] = c(gettext("Confidence Level", domain=dm), "-", x$confidence.level, .percent(x$confidence.level))
	tbl[4,] = c(gettext("High Value Threshold", domain=dm), "-", .value(x$High.value.threshold),
		ifelse(is.numeric(x$High.value.threshold), .percent(x$High.value.threshold / x$book.value), "-"))
	colnames(tbl) <- c(paste0(c(gettext("Description", domain=dm), rep("&nbsp;",6)), collapse=""),
		gettext("Items", domain=dm), gettext("Value", domain=dm), "%")
	x$tbl <- tbl
	if (style=="report") {
		pander::pandoc.table(x$tbl, digits=2, justify="lrrr", split.tables=Inf, keep.trailing.zeros=TRUE)
	} else {
		cat("\n-", .f(gettext("Confidence Level", domain=dm)), x$confidence.level)
		cat("\n-", .f(gettext("Population size", domain=dm)), nrow(x$data))
		cat("\n-", .f(gettext("Population amount", domain=dm)), x$book.value)
		cat("\n-", .f(gettext("Expected Error in population", domain=dm)), x$expected.error)
		cat("\n-", .f(gettext("Expected Error Rate", domain=dm)), .percent(x$expected.error / x$book.value))
		cat("\n-", .f(gettext("Tolerable Error (Materiality)", domain=dm)), x$tolerable.error)
		cat("\n-", .f(gettext("Tolerable Error Rate", domain=dm)), .percent(x$tolerable.error / x$book.value))
		cat("\n-", .f(gettext("Sample size", domain=dm)), x$n)
		cat("\n-", .f(gettext("High Value Threshold", domain=dm)), x$High.value.threshold, "\n")
	}
}
