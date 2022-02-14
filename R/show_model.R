# Copied from the R souce at https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.R
# Modified

#' Show a compact model summary
#'
#' Prints a modified version of the defaults \code{lm} model summary. The model summary statistics
#' are shown on top, followed by the (sorted) table of coefficients. The function call and
#' residual information is omitted.
#'
#' The function is based on the \code{print.summary.lm} function in \code{base}. Function parameters not documented
#' here are described in \code{print.summary.lm}.
#'
#' @param n_coefs Number of coefficients to display (NA=all).
#' @param order_coefs Function used to order coefficients. Default sorts by decending magnitude.
#' @param show_coefs If \code{FALSE}, skip the coefficient table.
#' @param show_fit If \code{FALSE}, skip the model fit statistics (R^2, F, etc.)
#'
#' @export
show_model <-
  function (x, digits = max(3L, getOption("digits") - 3L),
            symbolic.cor = x$symbolic.cor,
            signif.stars = getOption("show.signif.stars"),
            n_coefs = NA,
            order_coefs = abs,
            show_coefs = TRUE,
            show_fit = TRUE, ...)
  {
    x <- summary.lm(x)
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    if (show_fit) {
      cat("Residual standard error:",
          format(signif(x$sigma, digits)), "on", rdf, "degrees of freedom")
      cat("\n")
      if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
      if (!is.null(x$fstatistic)) {
        cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
        cat(",\tAdjusted R-squared: ",formatC(x$adj.r.squared, digits = digits),
            "\nF-statistic:", formatC(x$fstatistic[1L], digits = digits),
            "on", x$fstatistic[2L], "and",
            x$fstatistic[3L], "DF,  p-value:",
            format.pval(pf(x$fstatistic[1L], x$fstatistic[2L],
                           x$fstatistic[3L], lower.tail = FALSE),
                        digits = digits))
        cat("\n\n")
      }
    }

    if (show_coefs) {
      if (length(x$aliased) == 0L) {
        cat("No Coefficients\n")
      } else {
        coefs <- x$coefficients
        if(!is.null(aliased <- x$aliased) && any(aliased)) {
          cn <- names(aliased)
          coefs <- matrix(NA, length(aliased), 4, dimnames=list(cn, colnames(coefs)))
          coefs[!aliased, ] <- x$coefficients
        }

        ordr <- order(order_coefs(coefs[ ,1]), decreasing=TRUE)
        coefs <- coefs[ordr, ]

        if (!is.na(n_coefs)) {
          coefs <- coefs[1:n_coefs, ]
        }
        printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
                     na.print = "NA", ...)
      }
      ##
    }

    cat("\n")#- not in S
    invisible(x)
  }


