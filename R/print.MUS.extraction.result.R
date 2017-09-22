print.MUS.extraction.result <- function(x, print.title=TRUE, print.planning=FALSE, ...){
	# Checking parameter
	if (class(x)!="MUS.extraction.result" && class(x)!="MUS.evaluation.result" && class(x)!="MUS.planning.result") {
		stop("x has to be an object from type MUS.extraction.result or MUS.evaluation.result or MUS.planning.result.")
	}
	cat("\nExtraction Parameters\n")
	if (print.planning) {
		print.MUS.planning.result(x)
	}
	cat("\n- Sample items:\t\t\t\t\t", nrow(x$sample))
	cat("\n- Sample amount:\t\t\t\t", round(sum(x$sample[,x$col.name.book.values])))
	cat("\n- Sample coverage:\t\t\t\t", round(sum(x$sample[,x$col.name.book.values]) * 100 / x$book.value, 2), "%")

	cat("\n- High Value items:\t\t\t\t", nrow(x$high.values))
	cat("\n- High Value amount:\t\t\t\t", round(sum(x$high.values[,x$col.name.book.values])))
	cat("\n- High Value coverage:\t\t\t\t", round(sum(x$high.values[,x$col.name.book.values]) * 100 / x$book.value, 2), "%")

	cat("\n- Audited items:\t\t\t\t", round(nrow(x$high.values) + nrow(x$sample)))
	cat("\n- Audited amount:\t\t\t\t", round(sum(x$high.values[,x$col.name.book.values]) +
		sum(x$sample[,x$col.name.book.values])))
	cat("\n- Audited coverage:\t\t\t\t", round((sum(x$high.values[,x$col.name.book.values]) +
		sum(x$sample[,x$col.name.book.values])) * 100 / x$book.value, 2), "%\n")
}
