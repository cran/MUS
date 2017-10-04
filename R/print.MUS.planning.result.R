print.MUS.planning.result <- function(x, print.title=TRUE, style="default", use.pander=FALSE, ...){
	# Checking parameter
	if (class(x)!="MUS.extraction.result" && class(x)!="MUS.evaluation.result" && class(x)!="MUS.planning.result") {
		stop("x has to be an object from type MUS.extraction.result or MUS.evaluation.result or MUS.planning.result.")
	}
	if (print.title) {
		mus.title("Planning Parameters", use.pander=use.pander)

	}
	tbl <- matrix(nrow=4, ncol=4)
	tbl[1,] = c("Expected Error", "-", mus.value(x$expected.error), mus.percent(x$expected.error / x$book.value))
	tbl[2,] = c("Tolerable Error (Materiality)", "-", mus.value(x$tolerable.error), mus.percent(x$tolerable.error / x$book.value))
	tbl[3,] = c("Confidence Level", "-", x$confidence.level, mus.percent(x$confidence.level))
	tbl[4,] = c("High Value Threshold", "-", mus.value(x$High.value.threshold),
		ifelse(is.numeric(x$High.value.threshold), mus.percent(x$High.value.threshold / x$book.value), "-"))
	colnames(tbl) <- c(paste0(c("Description", rep("&nbsp;",6)), collapse=""), "Items", "Value", "%")
	x$tbl <- tbl
	if (style=="report") {
		pandoc.table(x$tbl, digits=2, justify="lrrr", split.tables=Inf, keep.trailing.zeros=TRUE)
	} else {
		cat("\n- Confidence Level:\t\t\t\t", x$confidence.level)
		cat("\n- Population size:\t\t\t\t", nrow(x$data))
		cat("\n- Population amount:\t\t\t\t", x$book.value)
		cat("\n- Expected Error in population:\t\t\t", x$expected.error)
		cat("\n- Expected Error Rate:\t\t\t\t", mus.percent(x$expected.error / x$book.value))
		cat("\n- Tolerable Error (Materiality):\t\t", x$tolerable.error)
		cat("\n- Tolerable Error Rate:\t\t\t\t", mus.percent(x$tolerable.error / x$book.value))
		cat("\n- Sample size:\t\t\t\t\t", x$n)
		cat("\n- High Value Threshold:\t\t\t\t", round(x$High.value.threshold), "\n")
	}
}
