
MUS.extend <- function(extract, new_plan=NULL, additional.n=NULL) {
	if (class(extract)!="MUS.extraction.result") stop("extract has to be an object from type MUS.extraction.result. Use function MUS.extraction to create such an object.")
	if (class(new_plan)!="MUS.planning.result" && !is.null(new_plan)) stop("new_plan has to be an object from type MUS.planning.result. Use function MUS.planning to create such an
 object or NULL.")
	if (class(additional.n)!="numeric" && !is.null(additional.n)) stop("additional.n must be numeric or NULL.")
	# rebuild plan from extract object
 	if (is.null(additional.n)) {
		additional.n <- 0
	}
	
	if (is.null(new_plan)) {
		n.final = extract$n + additional.n
		interval <- extract$book.value / n.final # calculate sampling interval
		tol.taint <- extract$expected.error / extract$book.value * n.final # calculate tolerable taintings (maximal number of full overstatements that will be acceptable in the sample)

		new_plan <- list(
			data=extract$data, 
			col.name.book.values=extract$col.name.book.values, 
			confidence.level=extract$confidence.level, 
			tolerable.error=extract$tolerable.error, 
			expected.error=extract$expected.error, 
			book.value=extract$book.value, 
			n=n.final, 
			High.value.threshold=interval, 
			tolerable.taintings=tol.taint, 
			combined=extract$combined)
		class(new_plan) <- "MUS.planning.result"	
	} else {
	    additional.n <- new_plan$n - extract$n
	}
	if (additional.n < 1) {
		extract$additional.sample <- extract$sample[FALSE, ]
		return(extract)
	}
	total.n <- (additional.n + extract$n)
	colunas <- colnames(extract$sample.population)
	sample.cols <- colnames(extract$sample)

	# split data into high values and population from which will be sampled
	if (is.data.frame(extract$high.values)) {
		old.high.values <- extract$high.values
	} else {
		old.high.values <- extract$sample.population[FALSE, ]
	}
	old.high.values$MUS.total <- rep(0, nrow(old.high.values))
	old.high.values$MUS.hit <- rep(0, nrow(old.high.values))
	# old.sample.population <- extract$sample.population
    old.sample <- extract$sample
	old.audited <- rbind(old.sample[, sample.cols], old.high.values[, sample.cols])
	# create a brand new sample with the new n
	new_extract <- MUS.extraction(new_plan, start.point=NULL, extract$seed, extract$obey.n.as.min, extract$combined)	
	if (is.data.frame(new_extract$high.values)) {
		new.high.values <- new_extract$high.values
	} else {
		new.high.values <- new_extract$sample.population[FALSE, ]
	}
	new.high.values$MUS.total <- rep(0, nrow(new.high.values))
	new.high.values$MUS.hit <- rep(0, nrow(new.high.values))

	new.sample <- rownames(new_extract$sample)
	new.n <- length(new.sample)
	selected <- rownames(old.audited)[!rownames(old.audited) %in% rownames(new.high.values)]
	new.basedraw <- new.sample[!new.sample %in% selected]
	# final sample is original sample+high.values that are not on the new high.values
	# extended by randomly selected elements from the new sample
	# this allows us to reuse the extraction method as is
	newSize <- pmax(0, new.n - length(selected))
	if (newSize > 0) {
			adding <- sample(new.basedraw, newSize)
			final.sample <- c(selected, adding)
	} else {
			final.sample <- c(selected)
			adding <- c()
	}	

	if(!"MUS.total" %in% colnames(new_extract$sample.population)) {
		new_extract$sample.population$MUS.total <- rep(0, nrow(new_extract$sample.population))
	}
	if(!"MUS.hit" %in% colnames(new_extract$sample.population )) {
		new_extract$sample.population$MUS.hit <- rep(0, nrow(new_extract$sample.population))
	}
	new_extract$sample <- new_extract$sample.population[rownames(new_extract$sample.population) %in% final.sample, sample.cols]
	new_extract$sample.population <- new_extract$sample.population[, colunas]
	# calculate revised sampling interval (used for evaluation of the sample population)
	new_extract$interval <- sum(new_extract$sample.population[,new_extract$col.name.book.values])/nrow(new_extract$sample)
	new_extract$extensions <- extract$extensions + 1
	new_extract$n.qty <- c(extract$n.qty, additional.n)
	# return all results, parameters and planning object as list for further processing
	return(new_extract)
}
