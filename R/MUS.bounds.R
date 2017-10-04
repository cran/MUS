moment.bound <- function(x, confidence.level=0.95, as.percentage=FALSE, include.high.values=TRUE) {
    # Dworking & Grimlund, 1984
    # data = c(rep(0, 96), -.16, .04, .18, .47)
    if (!class(x)=="MUS.evaluation.result" && !is.vector(x)) stop("x has to be a vector or an object of type MUS.evaluation.result. Use function MUS.evaluate to create such an object.")

    if (class(x)=="MUS.evaluation.result") {
        if (include.high.values) {
            data <- c(1 - x$filled.high.values[,x$col.name.audit.values] / x$filled.high.values[,x$col.name.book.values])
            data <- c(data, (1 - x$filled.sample[,x$col.name.audit.values] / x$filled.sample[,x$col.name.book.values]))
        } else {
            data <- c(1 - x$filled.sample[,x$col.name.audit.values] / x$filled.sample[,x$col.name.book.values])
        }
        confidence.level <- x$confidence.level
        mult <- ifelse(as.percentage, 100, x$Results.Total$Net.most.likely.error[1])
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

binomial.bound <- function(x, target="qty", as.percentage=FALSE, include.high.values=TRUE, confidence.level=0.95) {
    if (!class(x)=="MUS.evaluation.result" && !is.vector(x)) stop("x has to be a vector or an object of type MUS.evaluation.result. Use function MUS.evaluate to create such an object.")
    if (class(x)=="MUS.evaluation.result") {
        if (target == "value") {
            if (include.high.values) {
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
            if (include.high.values) {
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
        mult <- ifelse(as.percentage, 100, x$book.value)
    } else {
        # x are taintings
        mult <- 100
        book.value <- length(x)
        misstatement <- sum(x)
    }
#    bt <- binom.test(misstatement, book.value, materiality, alternative="less", conf.level=x$confidence.level)
    bc <- BinomCI(misstatement, book.value, conf.level=1-(1-confidence.level) * 2, method = "clopper-pearson")
    #bt$conf.int[2]
    ifelse(as.percentage, (bc[3] * mult), round(bc[3] * mult))
}

multinomial.bound <- function(x, as.percentage=FALSE, include.high.values=TRUE) {
    res <- NA
    if (require("DescTools")) {
        if (include.high.values) {
            misstatement <- ceiling(c(1 - x$filled.sample[,x$col.name.audit.values] / x$filled.sample[,x$col.name.book.values],
                1 - x$filled.high.values[,x$col.name.audit.values] / x$filled.high.values[,x$col.name.book.values]))
        } else {
            misstatement <- ceiling(c(1 - x$filled.sample[,x$col.name.audit.values] / x$filled.sample[,x$col.name.book.values]))
        }

        observed <- aggregate(data.frame(count = misstatement), list(value = misstatement), length)
        res <- MultinomCI(observed$count, conf.level=1-(1-x$confidence.level) * 2, method="sisonglaz")
    }
    mult <- ifelse(as.percentage, 100, x$book.value)
    ifelse(as.percentage, ((1-res[observed$value==0][2])) * mult, round(((1-res[observed$value==0][2])) * mult))
}
