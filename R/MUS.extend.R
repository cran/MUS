
MUS.extend <- function(extract, additional.n, new_plan=NULL){
	if (class(extract)!="MUS.extraction.result") stop("extract has to be an object from type MUS.extraction.result. Use function MUS.extraction to create such an object.")
	if (additional.n < 1) {
		extract$additional.sample <- extract$sample[FALSE, ]
		return(extract)
	}
	total.n <- (additional.n + extract$n)
	extract$n <- total.n
	colunas <- colnames(extract$sample.population)
	sample.cols <- colnames(extract$sample)

	# rebuild plan from extract object
	if (is.null(new_plan)) {
		new_plan <- list(
			data=extract$data, 
			col.name.book.values=extract$col.name.book.values, 
			confidence.level=extract$confidence.level, 
			tolerable.error=extract$tolerable.error, 
			expected.error=extract$expected.error, 
			book.value=extract$book.value, 
			n=extract$n, 
			High.value.threshold=extract$High.value.threshold, 
			tolerable.taintings=extract$tolerable.taintings, 
			combined=extract$combined)
		class(new_plan) <- "MUS.planning.result"	
	}
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
	new_extract <- MUS.extraction(new_plan, extract$start.point, extract$seed, extract$obey.n.as.min, extract$combined)	
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
	adding <- sample(new.basedraw, new.n - length(selected))
	final.sample <- c(selected, adding)
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
