print.MUS.planning.result <- function(x, print.title=TRUE, ...){
	# Checking parameter
	if (class(x)!="MUS.extraction.result" && class(x)!="MUS.evaluation.result" && class(x)!="MUS.planning.result") {
		stop("x has to be an object from type MUS.extraction.result or MUS.evaluation.result or MUS.planning.result.")
	}
	if (print.title) {
		cat("\nPlanning Parameters\n")
	}
	cat("\n- Confidence Level:\t\t\t\t", x$confidence.level)
	cat("\n- Population size:\t\t\t\t", nrow(x$data))
	cat("\n- Population amount:\t\t\t\t", x$book.value)
	cat("\n- Expected Error in population:\t\t\t", x$expected.error)
	cat("\n- Expected Error Rate:\t\t\t\t", percent(x$expected.error / x$book.value))
	cat("\n- Tolerable Error (Materiality):\t\t", x$tolerable.error)
	cat("\n- Tolerable Error Rate:\t\t\t\t", percent(x$tolerable.error / x$book.value))
	cat("\n- Sample size:\t\t\t\t\t", x$n)
	cat("\n- High Value Threshold:\t\t\t\t", round(x$High.value.threshold), "\n")
}
