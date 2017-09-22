moment.bound <- function(x, confidence.level=0.95) {
    # Dworking & Grimlund, 1984
    # data = c(rep(0, 96), -.16, .04, .18, .47)
    if (!class(x)=="MUS.evaluation.result" && !is.vector(x)) stop("x has to be a vector or an object of type MUS.evaluation.result. Use function MUS.evaluate to create such an object.")

    if (class(x)=="MUS.evaluation.result") {
        data <- (x$filled.sample[,x$col.name.book.values] - x$filled.sample[,x$col.name.audit.values]) / x$filled.sample[,x$col.name.book.values]
        confidence.level <- x$confidence.level
        mult <- x$Results.Total$Net.most.likely.error[1]
    } else {
        data <- x
        mult <- 1
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
    CB*mult
}

combine.evaluations <- function(lx) {
    if (!is.list(lx) && length(lx)<1) {
        stop("lx must be a list with one or more MUS.evaluation.result objects.")
    }
    s <- 1
    x <- lx[[s]]
    if (length(lx)>1) {
        for (s in 2:length(lx)) {
            y <- lx[[s]]
            x$sample <- rbind(x$sample, y$sample)
            x$filled.sample <- rbind(x$filled.sample, y$filled.sample)
            x$filled.high.values <- rbind(x$filled.high.values, y$filled.high.values)
            x$book.value <- x$book.value + y$book.value
        }
    }
    x
}
