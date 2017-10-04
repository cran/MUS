MUS.extraction <- function(plan, start.point=NULL, seed=NULL, obey.n.as.min=FALSE, combined=FALSE){
	# check parameters plan, seed and obey.n.as.min
	if (class(plan)!="MUS.planning.result") stop("plan has to be an object from type MUS.planning.result. Use function MUS.planning to create such an object.")
	if (!is.null(seed)) if(!is.numeric(seed) | round(seed)!=seed | length(seed)!=1 | seed<0) stop("seed has to be an integer value greater or equal than 0.")
	if (!is.logical(obey.n.as.min) | is.na(obey.n.as.min)) stop("obey.n.as.min has to be TRUE or FALSE.")

	# set seed according to user input
	if (!is.null(seed)) set.seed(seed)

	# split data into high values and population from which will be sampled
	high.values <- subset(plan$data, with(plan$data, get(plan$col.name.book.values))>=plan$High.value.threshold)
	sample.population <- subset(plan$data, with(plan$data, get(plan$col.name.book.values))<plan$High.value.threshold)

	# take high value threshold as sampling interval (some statistical software does it like this)
	interval <- plan$High.value.threshold
	if (obey.n.as.min) {
		# calculate and replace interval by perfect sampling interval (if high value threshold is used, there might be less samples drawn than specified - cannot happen with perfect sampling interval)
		oldinterval <- interval
		interval <- sum(sample.population[,plan$col.name.book.values])/(plan$n-nrow(high.values))
		# If the interval is not equal to the old interval, there might be other items with book value between old and new interval. Move them to high interval and recalculate interval.
		while(oldinterval!=interval){
			high.values <- subset(plan$data, with(plan$data, get(plan$col.name.book.values))>=plan$High.value.threshold)
			sample.population <- subset(plan$data, with(plan$data, get(plan$col.name.book.values))<plan$High.value.threshold)
			oldinterval <- interval
			interval <- sum(sample.population[,plan$col.name.book.values])/(plan$n-nrow(high.values))
		}
	}

	# check parameter start.point
	if (!is.null(start.point)) if (!is.numeric(start.point) | length(start.point)!=1 | start.point<0 | start.point>interval) stop("start.point has to be a numeric value between 0 and possible recalculated interval length (both inclusive).")

	# fixed interval selection method
	# if no start point is provided, set a random one
	if (is.null(start.point)) start.point <- runif(n=1, min=0, max=interval)

	# calculate the units to be sampled, one in each interval
	samplingunits <- round(start.point+0:plan$n*round(interval, digits=2))

	# add running sum
	sample.population <- cbind(sample.population, MUS.total=cumsum(sample.population[, plan$col.name.book.values]))

	# cut samplingunits if they are above the maximum book value
	samplingunits <- subset(samplingunits, samplingunits<=sum(sample.population[, plan$col.name.book.values]))

	# draw sample and add MUS.hit
	sample <- cbind(sample.population[findInterval(samplingunits, c(0,sample.population$MUS.total)),], MUS.hit=samplingunits)

	# calculate revised sampling interval (used for evaluation of the sample population)
	interval <- sum(sample.population[,plan$col.name.book.values])/nrow(sample)

	# return all results, parameters and planning object as list for further processing
	result <- c(plan, list(start.point=start.point, seed=seed, obey.n.as.min=obey.n.as.min, high.values=high.values, sample.population=sample.population, sampling.interval=interval, sample=sample, combined=combined))
	class(result) <- "MUS.extraction.result"
	return(result)
}

