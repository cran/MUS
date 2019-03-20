# calculate necessary observations for a given number of wrong units, additional alpha=1-confidence level
# this function does not work for very small account values, because
# the hyper-geometric distribution is discrete and uniroot-function fails in this case
.calculate.n.hyper <- function(num.errors, alpha, tolerable.error, account.value) {
	# maximal error rate which would be acceptable if found in the sample
	max.error.rate <- tolerable.error/account.value

	# function that sums up the probabilities for each error and calculate the deivance to the given confidence level
	calculate.deviance <- function (n.stichprobe) {
		phyper(q=num.errors, m=round(max.error.rate*account.value), n=correct.mu, k=n.stichprobe)-alpha
	}

	# number of correct monetary units in the population
	correct.mu <- round((1-max.error.rate)*account.value)

	# search the zero point on the deviance and return the ceiled value
	return(ceiling(uniroot(f=calculate.deviance, interval=c(0, min(correct.mu+num.errors, account.value)))$root))
	# maximal possible sample size is the number of correct items added through the allowed errors
	# - every sample size greater than that has to be exactly 0 because it is impossible to happen.
	# However, if account value is larger, this is the maximal possible sampling size.
}

MUS.factor <- function(confidence.level, pct.ratio) {
# calculate MUS Factor
# Based on Technical Notes on the AICPA Audit Guide Audit Sampling, Trevor Stewart, AICPA, 2012
  erro = -1
  resp = erro
  max_iter=1000
  solved=0.000001
  if (confidence.level <= 0 || confidence.level >= 1 || pct.ratio < 0 || pct.ratio >= 1) {
    stop("Parameters must be between 0 and 1.")
  } else {
    F = qgamma(confidence.level, 1, 1)
    if (pct.ratio == 0) {
      resp = F
    } else {
      F1 = 0
      i = 0
      while ((abs(F1-F)>solved) && (i<=max_iter)) {
        F1 = F
        F = qgamma(confidence.level, 1 + pct.ratio * F1, 1)
        i = i + 1
      }
      resp = ifelse((abs(F1-F)<=solved), F, erro)
    }
  }
  resp
}

MUS.calc.n.conservative <- function(confidence.level, tolerable.error, expected.error, book.value) {
# calculate n consevatively, as per AICPA audit guide
  pct.ratio = expected.error / tolerable.error
  conf.factor = ceiling(MUS.factor(confidence.level, pct.ratio)*100)/100
  ceiling(conf.factor / tolerable.error * book.value)
}

MUS.planning <- function(data, col.name.book.values="book.value", confidence.level=.95, tolerable.error, expected.error,
	n.min=0, errors.as.pct=FALSE, conservative=FALSE, combined=FALSE){
	# check parameters data and col.name.book.values
	if (!is.data.frame(data) | is.matrix(data)) stop("Data needs to be a data frame or a matrix but it is not.")
	if (!is.character(col.name.book.values) | length(col.name.book.values)!=1 | !is.element(col.name.book.values, names(data))) stop("The data frame requires at least a column with the book values and the name of this column has to be provided by parameter col.name.book.values (default book.value).")

	# check book values for NAs, zeros or negative values
	if (any(is.infinite(with(data, get(col.name.book.values))))) warning("There are missing or infinite values (NA, NaN or Inf) as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(data, get(col.name.book.values))==0)) warning("There are zeros as book values in your data. Those elements have no chance for selection. You have to audit them separately.")
	if (any(with(data, get(col.name.book.values))<0)) warning("There are negative values as book values in your data. Those elements have no chance for selection. You have to audit them separately.")

	# calculate gross book value from dataset
	book.value <- sum(pmax(with(data, get(col.name.book.values)), 0))

	# calculate number of items in the dataset
	num.items <- length(with(data, get(col.name.book.values)))

	# check other parameters
	if (errors.as.pct && is.numeric(tolerable.error) && is.numeric(expected.error)) {
		tolerable.error = tolerable.error * book.value;
		expected.error = expected.error * book.value;
	}
	if (!is.numeric(confidence.level) | length(confidence.level)!=1 | confidence.level<=0 | confidence.level>=1) stop("Confidence level has to be a numeric value between 0 and 1 (both exclusive).")
	if (!is.numeric(tolerable.error) | length(tolerable.error)!=1 | tolerable.error<=0) stop("Tolerable Error has to be a numeric value between 0 and book value (both exclusive).")
	if (!is.numeric(expected.error) | length(expected.error)!=1 | expected.error<0) stop("Expected error has to be a numeric value greater or equal to 0.")
	if (!is.numeric(n.min) | length(n.min)!=1 | n.min<0 | n.min>=num.items) stop("Minimum number of sample size has to be a numeric value between 0 and the number of items in the population (last exclusive). If the minimum sample size is equal or larger than the number of items in the population, sampling is not suitable because every item has to be tested anyway.")
	too.large <- (tolerable.error/book.value)*(1-confidence.level)*sqrt(tolerable.error-expected.error) < 0.07
	if (too.large) {
		warning("Combination of parameters leads to impractically large sample.")
	}
	if (tolerable.error>=book.value) {
		warning("Tolerable Error has to be a numeric value between 0 and book value (both exclusive). If the tolerable error is equal larger than book value, no sampling is necessary. However, Planning will be proceeded.")
		n.optimal <- 0
	} else if (try(.calculate.n.hyper(0, alpha=1-confidence.level, tolerable.error=tolerable.error, account.value=book.value))<0) {
		stop("Undefined situation: If 0 errors in the sample occur, the sample size needs to be positive!")
	} else if (.calculate.n.hyper(num.items, alpha=1-confidence.level, tolerable.error=tolerable.error, account.value=book.value)*expected.error/book.value-num.items>0) {
		warning("MUS makes no sense for your sampling problem - your sample size needs to be bigger than the number of items in your population.")
		if (expected.error>=tolerable.error) message("Just for information: If the expected error is equal or larger than te tolerable error, MUS is not applicable.")
		n.optimal <- num.items
	} else {
		# search point of intersection between the lines of tolerable and expected errors
		i=0; while(.calculate.n.hyper(i, alpha=1-confidence.level, tolerable.error=tolerable.error, account.value=book.value)*expected.error/book.value > i){i=i+1;} # stop if a number becomes negative (meaning intersection have to be between i-1 und i)
		ni <- .calculate.n.hyper(num.errors=i-1, alpha=(1-confidence.level), tolerable.error=tolerable.error, account.value=book.value)
		nip1 <- .calculate.n.hyper(num.errors=i, alpha=(1-confidence.level), tolerable.error=tolerable.error, account.value=book.value)
		n.optimal <- ceiling((ni/(nip1-ni)-(i-1))/(1/(nip1-ni)-expected.error/book.value)) # linear interpolation between both points

		# check if n.optimal is a plausible sample size
		if (n.optimal>num.items) {
			warning("MUS makes no sense for your sampling problem - your sample size needs to be bigger than the number of items in your population.")
			n.optimal <- num.items # audit everything in this case
		} else if (n.optimal==nip1+1) {
			n.optimal=n.optimal-1 # due to rounding n.optimal can become nip1+1, in this case just subtract 1 to get nip1 as upper limit again
		} else {
			if (n.optimal < 0) {print(i); print(ni); print(nip1); print(n.optimal); stop("n.optimal is not plausible (negative), internal error, please report as bug.")}
			if (n.optimal < ni) {print(i); print(ni); print(nip1); print(n.optimal); stop("n.optimal is not plausible (smaller than lower bound ni), internal error, please report as bug.")}
			if (n.optimal > nip1) {print(i); print(ni); print(nip1); print(n.optimal); stop("n.optimal is not plausible (greater than upper bound nip1), internal error, please report as bug.")}
		}
	}
	n.final <- max(n.optimal, n.min) # take greater value of optimal n or predefined minimum sample size

	if (conservative) {
		n.final = max(n.final, MUS.calc.n.conservative(confidence.level, tolerable.error, expected.error, book.value))
	}

	interval <- book.value/n.final # calculate sampling interval
	tol.taint <- expected.error/book.value*n.final # calculate tolerable taintings (maximal number of full overstatements that will be acceptable in the sample)

	# return all results, parameters and data as list for further processing
	result <- list(data=data, col.name.book.values=col.name.book.values, confidence.level=confidence.level, tolerable.error=tolerable.error, expected.error=expected.error, book.value=book.value, n=n.final, High.value.threshold=interval, tolerable.taintings=tol.taint, combined=combined)
	class(result) <- "MUS.planning.result"
	return(result)
}
