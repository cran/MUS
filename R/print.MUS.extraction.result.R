print.MUS.extraction.result <- function(x, print.title=TRUE, print.planning=FALSE, style="default", use.pander=FALSE, ...){
	# Checking parameter
	if (class(x)!="MUS.extraction.result" && class(x)!="MUS.evaluation.result" && class(x)!="MUS.planning.result") {
		stop("x has to be an object from type MUS.extraction.result or MUS.evaluation.result or MUS.planning.result.")
	}
	mus.title("Extraction Parameters", use.pander=use.pander)
	if (print.planning) {
		print.MUS.planning.result(x, use.pander=use.pander)
	}

	tbl <- matrix(nrow=4, ncol=4)
	tbl[1,] = c("Sample", nrow(x$sample), mus.value(sum(x$sample[,x$col.name.book.values])),
		mus.percent(sum(x$sample[,x$col.name.book.values]) / x$book.value)	)
	tbl[2,] = c("High Values", nrow(x$high.values), mus.value(sum(x$high.values[,x$col.name.book.values])),
		mus.percent(sum(x$high.values[,x$col.name.book.values]) / x$book.value)	)
	tbl[3,] = c("Audited", nrow(x$high.values) + nrow(x$sample),
		mus.value(sum(x$high.values[,x$col.name.book.values]) + sum(x$sample[,x$col.name.book.values])),
		mus.percent((sum(x$high.values[,x$col.name.book.values]) + sum(x$sample[,x$col.name.book.values])) / x$book.value)	)
	tbl[4,] = c("Population", nrow(x$data), mus.value(sum(x$data[,x$col.name.book.values])),
		mus.percent(sum(x$data[,x$col.name.book.values]) / x$book.value)	)

	colnames(tbl) <- c(paste0(c("Description", rep("&nbsp;",6)), collapse=""), "Items", "Value", "%")
	x$tbl <- rbind(x$tbl, tbl)
	if (style=="report") {
		pandoc.table(x$tbl, digits=2, justify="lrrr", split.tables=Inf, keep.trailing.zeros=TRUE)
	} else {
		cat("\n- Sample items:\t\t\t\t\t", nrow(x$sample))
		cat("\n- Sample amount:\t\t\t\t", round(sum(x$sample[,x$col.name.book.values])))
		cat("\n- Sample coverage:\t\t\t\t", mus.percent(sum(x$sample[,x$col.name.book.values]) / x$book.value))

		cat("\n- High Value items:\t\t\t\t", nrow(x$high.values))
		cat("\n- High Value amount:\t\t\t\t", round(sum(x$high.values[,x$col.name.book.values])))
		cat("\n- High Value coverage:\t\t\t\t", mus.percent(sum(x$high.values[,x$col.name.book.values]) / x$book.value))

		cat("\n- Audited items:\t\t\t\t", round(nrow(x$high.values) + nrow(x$sample)))
		cat("\n- Audited amount:\t\t\t\t", round(sum(x$high.values[,x$col.name.book.values]) +
			sum(x$sample[,x$col.name.book.values])))
		cat("\n- Audited coverage:\t\t\t\t", mus.percent((sum(x$high.values[,x$col.name.book.values]) +
			sum(x$sample[,x$col.name.book.values])) / x$book.value), "\n")
	}
}
