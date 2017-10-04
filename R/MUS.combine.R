combine.evaluations <- function(lx) {
    if (!is.list(lx) || length(lx)<1) {
        stop("lx must be a list with one or more MUS.evaluation.result objects.")
    }
    s <- 1
    x <- lx[[s]]
    if (length(lx)>1) {
        x$High.value.threshold <- "-"
        x$Strata <- length(lx)
        x$qty.rejected <- ifelse(x$acceptable, 0, 1)
        x$qty.accepted <- ifelse(x$acceptable, 1, 0)
        for (s in 2:length(lx)) {
            y <- lx[[s]]
            x$data <- rbind(x$data, y$data)
            x$sample <- rbind(x$sample, y$sample)
            if ("filled.sample" %in% names(x)) {
                x$filled.sample <- rbind(x$filled.sample, y$filled.sample)
            }
            if ("high.values" %in% names(x) && class(y$high.values)=="data.frame") {
                if (class(x$high.values)=="data.frame") {
                    x$high.values <- rbind(x$high.values, y$high.values)
                } else {
                    x$high.values <- y$high.values
                }
            }
            if ("filled.high.values" %in% names(x) && class(y$filled.high.values)=="data.frame") {
                if (class(x$filled.high.values)=="data.frame") {
                    x$filled.high.values <- rbind(x$filled.high.values, y$filled.high.values)
                } else {
                    x$filled.high.values <- y$filled.high.values
                }
            }
            if ("sample.population" %in% names(x)) {
                x$sample.population <- rbind(x$sample.population, y$sample.population)
            }
            if ("Results.Total" %in% names(x)) {
                for (j in 1:2) {
                    x$Results.Total$Number.of.Errors[j] <-
                        x$Results.Total$Number.of.Errors[j] +
                        y$Results.Total$Number.of.Errors[j]

                    x$Results.Total$Net.upper.error.limit[j] <-
                        x$Results.Total$Net.upper.error.limit[j] +
                        y$Results.Total$Net.upper.error.limit[j]

                    x$Results.Total$Gross.upper.error.limit[j] <-
                        x$Results.Total$Gross.upper.error.limit[j] +
                        y$Results.Total$Gross.upper.error.limit[j]

                    x$Results.Sample$Number.of.Errors[j] <-
                        x$Results.Sample$Number.of.Errors[j] +
                        y$Results.Sample$Number.of.Errors[j]

                    x$Results.Sample$Net.upper.error.limit[j] <-
                        x$Results.Sample$Net.upper.error.limit[j] +
                        y$Results.Sample$Net.upper.error.limit[j]

                    x$Results.Sample$Gross.upper.error.limit[j] <-
                        x$Results.Sample$Gross.upper.error.limit[j] +
                        y$Results.Sample$Gross.upper.error.limit[j]

                    x$Results.High.values$Number.of.Errors[j] <-
                        x$Results.High.values$Number.of.Errors[j] +
                        y$Results.High.values$Number.of.Errors[j]

                    x$Results.High.values$Gross.Value.of.Errors[j] <-
                        x$Results.High.values$Gross.Value.of.Errors[j] +
                        y$Results.High.values$Gross.Value.of.Errors[j]

                    x$Results.Sample$Precision.Gap.widening[j] <-
                        x$Results.Sample$Precision.Gap.widening[j] +
                        y$Results.Sample$Precision.Gap.widening[j]

                    x$Results.Sample$Total.Precision[j] <-
                        x$Results.Sample$Total.Precision[j] +
                        y$Results.Sample$Total.Precision[j]

                    x$Results.Total$Net.most.likely.error[j] <-
                        x$Results.Total$Net.most.likely.error[j] +
                        y$Results.Total$Net.most.likely.error[j]
                }

                x$Results.Total$Total.number.of.items.examined <-
                    x$Results.Total$Total.number.of.items.examined +
                    y$Results.Total$Total.number.of.items.examined

                x$Results.Sample$Sample.Size <-
                    x$Results.Sample$Sample.Size +
                    y$Results.Sample$Sample.Size

                x$Results.Sample$Basic.Precision <-
                    x$Results.Sample$Basic.Precision +
                    y$Results.Sample$Basic.Precision

                x$Results.High.values$Number.of.high.value.items <-
                    x$Results.High.values$Number.of.high.value.items +
                    y$Results.High.values$Number.of.high.value.items

                x$Results.High.values$Net.Value.of.Errors <-
                    x$Results.High.values$Net.Value.of.Errors +
                    y$Results.High.values$Net.Value.of.Errors

                x$UEL.low.error.rate <- x$UEL.low.error.rate + y$UEL.low.error.rate
                x$UEL.high.error.rate <- x$UEL.high.error.rate + y$UEL.high.error.rate

                if ("acceptable.low.error.rate" %in% names(x)) {
                    x$acceptable.low.error.rate <-
                        ifelse(y$acceptable.low.error.rate, x$acceptable.low.error.rate, y$acceptable.low.error.rate)
                }
                if ("acceptable.high.error.rate" %in% names(x)) {
                    x$acceptable.high.error.rate <-
                        ifelse(y$acceptable.high.error.rate, x$acceptable.high.error.rate, y$acceptable.high.error.rate)
                }
                if ("acceptable" %in% names(x)) {
                    x$acceptable <- ifelse(y$acceptable, x$acceptable, y$acceptable)
                    x$qty.rejected <- x$qty.rejected + ifelse(y$acceptable, 0, 1)
                    x$qty.accepted <- x$qty.accepted + ifelse(y$acceptable, 1, 0)
                }

            }

            x$book.value <- x$book.value + y$book.value
            x$tolerable.error <- x$tolerable.error + y$tolerable.error
            x$expected.error <- x$expected.error + y$expected.error
            x$n <- x$n + y$n
            x$n.min <- x$n.min + y$n.min
        }
        x$combined <- TRUE
    }
    x
}
