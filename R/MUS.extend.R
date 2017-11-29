
MUS.extend <- function(extract, additional.n){
	if (class(extract)!="MUS.extraction.result") stop("extract has to be an object from type MUS.extraction.result. Use function MUS.extraction to create such an object.")
	if (additional.n < 1) {
		extract$additional.sample <- extract$sample[FALSE, ]
		return(extract)
	}
	seed <- extract$seed
	obey.n.as.min <- extract$obey.n.as.min
	start.point <- extract$start.point
	# set seed according to user input
	if (!is.null(seed)) set.seed(seed)
	interval <- extract$sampling.interval

	# split data into high values and population from which will be sampled
	high.values <- extract$high.values
	sample.population <- extract$sample.population
    sample <- extract$sample
    remaining <- (! rownames(sample.population) %in% rownames(sample))
    sample.population <- sample.population[remaining, ]

	if (obey.n.as.min) {
		# calculate and replace interval by perfect sampling interval (if high value threshold is used, there might be less samples drawn than specified - cannot happen with perfect sampling interval)
		interval <- sum(sample.population[,extract$col.name.book.values])/additional.n
		oldinterval <- interval-1
		# If the interval is not equal to the old interval, there might be other items with book value between old and new interval. Move them to high interval and recalculate interval.
		while(oldinterval!=interval){
			sample.population <- subset(sample.population, with(sample.population, get(extract$col.name.book.values))<interval)
			oldinterval <- interval
			interval <- sum(sample.population[,extract$col.name.book.values])/additional.n
		}
	}

	# fixed interval selection method
	# if no start point is provided, set a random one
	if (is.null(start.point)) start.point <- runif(n=1, min=0, max=interval)

	# calculate the units to be sampled, one in each interval
	samplingunits <- round(start.point+0:(additional.n)*round(interval, digits=2))

	# cut samplingunits if they are above the maximum book value
	samplingunits <- subset(samplingunits, samplingunits<=sum(sample.population[, extract$col.name.book.values]))

	# draw sample and add MUS.hit
	extract$additional.sample <- sample.population[findInterval(samplingunits, c(0,sample.population$MUS.total)),]
	extract$additional.sample$MUS.hit <- samplingunits

	extract$sample <- rbind(extract$sample, extract$additional.sample)

	# calculate revised sampling interval (used for evaluation of the sample population)
	extract$interval <- sum(extract$sample.population[,extract$col.name.book.values])/nrow(extract$sample)
	extract$extensions <- extract$extensions + 1
	extract$n.qty <- c(extract$n.qty, additional.n)
	# return all results, parameters and planning object as list for further processing

	return(extract)
}
