
align_numbers <- function(x, integer_digits=5, decimal_digits=5) {
  use_na <- is.na(x)
  x[use_na] <- 0.0

  magnitude <- log10(ifelse(abs(x) < 1, 1, abs(x)))
  use_sci <- magnitude + 1 > integer_digits
  x <- round(x, decimal_digits)

  sign_char <- ifelse(x < 0, "-", " ")

  integer_char <- ifelse(use_sci, "", formatC(as.integer(abs(trunc(x)))))

  dec_part <- abs(x - trunc(x))
  decs <- format(dec_part, scientific=FALSE, nsmall=decimal_digits, drop0trailing=TRUE)
  decimal_char <- substr(decs, 2, nchar(decs))

  num_char <- ifelse(use_sci,
                     prettyNum(x, drop0trailing=TRUE),
                     paste0(sign_char, ifelse(integer_char=="0", "", integer_char), decimal_char))
  parts <- stringr::str_split_fixed(num_char, "\\.", 2)
  parts[use_na,1] <- "NA"
  paste0(format(parts[ ,1], justify="right"), ".", format(parts[ ,2], justify="left"))
}

pillar_numbers <- function(x, width=12) {
  as.character(format(pillar::pillar_shaft(x), width=width))
}

#' Show effect sizes.
#'
#' Prints the effect sizes for a vector of coefficients from a linear model.
#'
#' @param coefs A named numeric vector of coefficients from a \code{lm} model.
#' @param model A linear model produced by \code{lm}.
#' @param scaling Scale all non-intercept coefficients by this factor (default=1).
#' @param n Show up to \code{n} effects.
#' @param no_scale Character vector of effects to not scale (default=\code{"(Intercept)"}).
#' @param ordered By default (\code{ordered="none"}), the effects are ordered as they appear in the input.
#'   Setting \code{order="abs"} orders by absolute value (magnitude), and \code{order="signed"} shows in
#'   numerical order (largest to smallest, respecting sign).
#' @param colsep Separator between effect names and value (default=\code{"   "}).
#' @param intercept When \code{TRUE} (default), show the intercept as an effect.
#' @param integer_digits Max number of digits to display left of the decimal point. Larger numbers
#'   are displayed using scientific notation.
#' @param decimal_digits Effects are rounded to this many digits, and training zeros are not shown.
#'
#' @author Paul Jensen, \email{software@jensenlab.net}
#'
#' @export
show_effects <- function(...) UseMethod("show_effects")

#' @describeIn show_effects Base method for named numeric vectors
#' @export
show_effects.default <- function(coefs, scaling=1, n=length(coefs), no_scale="(Intercept)", ordered="none", colsep="   ", aligner=align_numbers, ...) {
  stopifnot(ordered %in% c("none", "abs", "signed"))
  coefs[!(names(coefs) %in% no_scale)] <- coefs[!(names(coefs) %in% no_scale)] * scaling
  if (ordered == "none") {
    orderfun <- function(x) length(x):1
  } else if (ordered == "abs") {
    orderfun <- abs
  } else {
    orderfun <- function(x) x
  }

  coefs <- coefs[order(orderfun(coefs), decreasing=TRUE)]
  strs <- paste0(
    format(names(coefs), justify="right"),
    colsep,
    aligner(coefs, ...),
    "\n"
  )
  for (i in 1:n) {
    cat(strs[i])
  }
}

#' @describeIn show_effects Show effects from a linear model.
#' @export
show_effects.lm <- function(model, intercept=TRUE, ...) {
  coefs <- coefficients(model)
  if (!intercept) {
    coefs <- coefs[names(coefs) != "(Intercept)"]
  }
  show_effects(coefs, ...)
}
