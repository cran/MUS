MUS.moment.bound <- function(x, confidence.level=0.95, as.pct=FALSE, include.high.values=TRUE) {
    # Dworking & Grimlund, 1984
    # data = c(rep(0, 96), -.16, .04, .18, .47)
    if (!class(x)=="MUS.evaluation.result" && !is.vector(x)) stop("x has to be a vector or an object of type MUS.evaluation.result. Use function MUS.evaluate to create such an object.")

    if (class(x)=="MUS.evaluation.result") {
        data <- c(1 - x$filled.sample[,x$col.name.audit.values] / x$filled.sample[,x$col.name.book.values])
        if (include.high.values && is.data.frame(x$filled.high.values)) {
            data <- c(data, (1 - x$filled.high.values[,x$col.name.audit.values] / x$filled.high.values[,x$col.name.book.values]))
        }
        confidence.level <- x$confidence.level
        mult <- ifelse(as.pct, 100, x$Results.Total$Net.most.likely.error[1])
    } else {
        data <- x
        mult <- 0
    }

    taintings = data[data!=0]
    n <- length(taintings)
    N <- length(data)

    hypothetical.tainting = 0.81*(1-0.667*tanh(10*mean(taintings)))*(1+0.667*tanh(n/10))
    TN <- sapply(1:3, function(j) (hypothetical.tainting^j+sum(taintings^j))/5)
    RN <- c( (1+n)/(2+N), (1+n)/(2+N)*(2+n)/(3+N), (1+n)/(2+N)*(2+n)/(3+N)*(3+n)/(4+N) )
    UN <- c(  RN[1]*TN[1], (RN[1]*TN[2] + (N-1)*RN[2]*TN[1]^2)/N, ( RN[1]*TN[3] + 3*(N-1)*RN[2]*TN[1]*TN[2] + (N-1)*(N-2)*RN[3]*TN[1]^3)/N^2 )
    UC <- c( 0, UN[2]-UN[1]^2, UN[3]-3*UN[1]*UN[2]+2*UN[1]^3 )

    A <- (4*UC[2]^3/UC[3]^2)
    B <- (0.5*UC[3]/UC[2])
    G <- (UN[1]-2*UC[2]^2/UC[3])
    Z <- qnorm(confidence.level)
    CB <- G + A*B*(1+Z/sqrt(9*A)-1/(9*A))^3
    ifelse(mult == 0, CB*100, (1 + CB) * (mult))
}

MUS.binomial.bound <- function(x, scope="qty", as.pct=FALSE, include.high.values=TRUE, confidence.level=0.95) {
    if (!class(x)=="MUS.evaluation.result" && !is.vector(x)) stop("x has to be a vector or an object of type MUS.evaluation.result. Use function MUS.evaluate to create such an object.")
    if (class(x)=="MUS.evaluation.result") {
        if (scope == "value") {
            if (include.high.values && is.data.frame(x$filled.high.values) ) {
                misstatement <- sum(x$filled.sample[,x$col.name.book.values] - x$filled.sample[,x$col.name.audit.values]) +
                    sum(x$filled.high.values[,x$col.name.book.values] - x$filled.high.values[,x$col.name.audit.values])
                audited.value <- sum(x$filled.sample[,x$col.name.book.values]) + sum(x$filled.high.values[,x$col.name.book.values])
                book.value <- length(x$filled.sample[,x$col.name.book.values]) + length(x$filled.high.values[,x$col.name.book.values])
            } else {
                misstatement <- sum(x$filled.sample[,x$col.name.book.values] - x$filled.sample[,x$col.name.audit.values])
                audited.value <- sum(x$filled.sample[,x$col.name.book.values])
                book.value <- length(x$filled.sample[,x$col.name.book.values])
            }
            misstatement <- ceiling(misstatement / audited.value * book.value )
        } else {
            if (include.high.values && is.data.frame(x$filled.high.values) ) {
                misstatement <- sum(x$filled.sample[,x$col.name.book.values] != x$filled.sample[,x$col.name.audit.values]) +
                    sum(x$filled.high.values[,x$col.name.book.values] != x$filled.high.values[,x$col.name.audit.values])
                book.value <- length(x$filled.sample[,x$col.name.book.values]) + length(x$filled.high.values[,x$col.name.book.values])
            } else {
                misstatement <- sum(x$filled.sample[,x$col.name.book.values] != x$filled.sample[,x$col.name.audit.values])
                book.value <- length(x$filled.sample[,x$col.name.book.values])
            }
        }
        materiality <- x$tolerable.error / x$book.value
        confidence.level = x$confidence.level
        mult <- ifelse(as.pct, 100, x$book.value)
    } else {
        # x are taintings
        mult <- 100
        book.value <- length(x)
        misstatement <- sum(x)
    }

    if (requireNamespace("DescTools", quietly = TRUE)) {
        bc <- DescTools::BinomCI(misstatement, book.value, conf.level=1-(1-confidence.level) * 2, method = "clopper-pearson")
        ifelse(as.pct, (bc[3] * mult), round(bc[3] * mult))
    } else {
        NULL
    }
}

MUS.multinomial.bound <- function(x, as.pct=FALSE, include.high.values=TRUE) {
	if (class(x)!="MUS.evaluation.result") stop("x has to be an object from type MUS.evaluation.result. Use function MUS.evaluate to create such an object.")
    res <- NA
    if (requireNamespace("DescTools", quietly = TRUE)) {
        if (include.high.values && is.data.frame(x$filled.high.values) ) {
            misstatement <- ceiling(c(1 - x$filled.sample[,x$col.name.audit.values] / x$filled.sample[,x$col.name.book.values],
                1 - x$filled.high.values[,x$col.name.audit.values] / x$filled.high.values[,x$col.name.book.values]))
        } else {
            misstatement <- ceiling(c(1 - x$filled.sample[,x$col.name.audit.values] / x$filled.sample[,x$col.name.book.values]))
        }

        observed <- aggregate(data.frame(count = misstatement), list(value = misstatement), length)
        res <- DescTools::MultinomCI(observed$count, conf.level=1-(1-x$confidence.level) * 2, method="sisonglaz")
        mult <- ifelse(as.pct, 100, x$book.value)
        ifelse(as.pct, ((1-res[observed$value==0][2])) * mult, round(((1-res[observed$value==0][2])) * mult))
    } else {
        NULL
    }
}

MUS.combined.high.error.rate <- function(evaluation, interval.type="one-sided"){
	filled.sample <- evaluation$filled.sample
	filled.high.values <- evaluation$filled.high.values
	col.name.audit.values <- evaluation$col.name.audit.values
	col.name.book.values <- evaluation$col.name.book.values
	confidence.level <- evaluation$confidence.level

	ratios <- 1 - filled.sample[,col.name.audit.values]/filled.sample[,col.name.book.values]
	qty_errors <- sum(ratios!=0)
	ratios_mean <- mean(ratios)
	ratios_sd <- sd(ratios)
    if (is.data.frame(filled.high.values)) {
        N <- nrow(evaluation$data) - nrow(filled.high.values)
        Y <- sum(evaluation$data[, col.name.book.values]) - sum(filled.high.values[, col.name.book.values])
        high.values.error <- sum(filled.high.values[, col.name.book.values]-filled.high.values[, col.name.audit.values])
    } else {
        N <- nrow(evaluation$data)
        Y <- sum(evaluation$data[, col.name.book.values])
        high.values.error <- 0
    }
	R <- ifelse(interval.type == "two-sided",  1 - (1 - confidence.level) / 2, confidence.level)
	U <- qt(R, qty_errors - 1)

	most.likely.error <- ratios_mean * Y
	precision <- U * Y * ratios_sd / sqrt(nrow(filled.sample))
	upper.error.limit <- most.likely.error + precision * sign(most.likely.error) + high.values.error
	upper.error.limit
}
