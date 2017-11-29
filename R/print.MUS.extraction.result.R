print.MUS.extraction.result <- function(x, print.title=TRUE, print.planning=FALSE, style="default", use.pander=FALSE, ...){
	# Checking parameter
	if (class(x)!="MUS.extraction.result" && class(x)!="MUS.evaluation.result" && class(x)!="MUS.planning.result") {
		stop("x has to be an object from type MUS.extraction.result or MUS.evaluation.result or MUS.planning.result.")
	}
	dm <- "R-MUS"
	bindtextdomain(dm)
	.title(gettext("Extraction Parameters", domain=dm), use.pander=use.pander)
	if (print.planning) {
		print.MUS.planning.result(x, use.pander=use.pander)
	}

	tbl <- matrix(nrow=4, ncol=4)
	tbl[1,] = c(gettext("Sample", domain=dm), nrow(x$sample), .value(sum(x$sample[,x$col.name.book.values])),
		.percent(sum(x$sample[,x$col.name.book.values]) / x$book.value)	)
	tbl[2,] = c(gettext("High Values", domain=dm), nrow(x$high.values), .value(sum(x$high.values[,x$col.name.book.values])),
		.percent(sum(x$high.values[,x$col.name.book.values]) / x$book.value)	)
	tbl[3,] = c(gettext("Audited", domain=dm), nrow(x$high.values) + nrow(x$sample),
		.value(sum(x$high.values[,x$col.name.book.values]) + sum(x$sample[,x$col.name.book.values])),
		.percent((sum(x$high.values[,x$col.name.book.values]) + sum(x$sample[,x$col.name.book.values])) / x$book.value)	)
	tbl[4,] = c(gettext("Population", domain=dm), nrow(x$data), .value(sum(x$data[,x$col.name.book.values])),
		.percent(sum(x$data[,x$col.name.book.values]) / x$book.value)	)

	colnames(tbl) <- c(paste0(c(gettext("Description", domain=dm), rep("&nbsp;",6)), collapse=""),
		gettext("Items", domain=dm), gettext("Value", domain=dm), "%")

	x$tbl <- tbl

	if (style=="report") {
		pander::pandoc.table(x$tbl, digits=2, justify="lrrr", split.tables=Inf, keep.trailing.zeros=TRUE)
	} else {
		cat("\n-", .f(gettext("Sample items", domain=dm)), nrow(x$sample))
		cat("\n-", .f(gettext("Sample amount", domain=dm)), round(sum(x$sample[,x$col.name.book.values])))
		cat("\n-", .f(gettext("Sample coverage", domain=dm)), .percent(sum(x$sample[,x$col.name.book.values]) / x$book.value))

		cat("\n-", .f(gettext("High Value items", domain=dm)), nrow(x$high.values))
		cat("\n-", .f(gettext("High Value amount", domain=dm)), round(sum(x$high.values[,x$col.name.book.values])))
		cat("\n-", .f(gettext("High Value coverage", domain=dm)), .percent(sum(x$high.values[,x$col.name.book.values]) / x$book.value))

		cat("\n-", .f(gettext("Audited items", domain=dm)), round(nrow(x$high.values) + nrow(x$sample)))
		cat("\n-", .f(gettext("Audited amount", domain=dm)), round(sum(x$high.values[,x$col.name.book.values]) +
			sum(x$sample[,x$col.name.book.values])))
		cat("\n-", .f(gettext("Audited coverage", domain=dm)), .percent((sum(x$high.values[,x$col.name.book.values]) +
			sum(x$sample[,x$col.name.book.values])) / x$book.value), "\n")
	}
}
