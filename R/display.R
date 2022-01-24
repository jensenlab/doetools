
align_numbers <- function(x, integer_digits=5, decimal_digits=5) {
  use_sci <- log10(ifelse(abs(x) < 1, 1, abs(x))) >= integer_digits
  x <- round(x, decimal_digits)
  ints <- stringr::str_replace(format(trunc(x)), "^ *0$", " ")
  dec_part <- abs(x - trunc(x))
  decs <- format(dec_part, scientific=FALSE, nsmall=decimal_digits)
  numstrs <- stringr::str_replace_all(paste0(ints, substr(decs, 2, nchar(decs))), c("0+$"="", "^ +"=""))
  parts <- stringr::str_split_fixed(ifelse(use_sci, format(x), numstrs), "\\.", 2)
  paste0(format(parts[ ,1], justify="right"), ".", format(parts[ ,2], justify="left"))
}

#' Show effect sizes.
#'
#' Prints the effect sizes for a vector of coefficients from a linear model.
#'
#' @param coefs A named numeric vector of coefficients from a `lm` model.
#' @param model A linear model produced by `lm`.
#' @param scaling Scale all non-intercept coefficients by this factor (default=1).
#' @param no_scale Character vector of effects to not scale (default=`"(Intercept)"`).
#' @param ordered By default (`ordered="none"`), the effects are ordered as they appear in the input.
#'   Setting `order="abs"` orders by absolute value (magnitude), and `order="signed"` shows in
#'   numerical order (largest to smallest, respecting sign).
#' @param colsep Separator between effect names and value (default=`"   "`).
#' @param intercept When `TRUE` (default), show the intercept as an effect.
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
show_effects.default <- function(coefs, scaling=1, no_scale="(Intercept)", ordered="none", colsep="   ", ...) {
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
    align_numbers(coefs, ...),
    "\n"
  )
  for (s in strs) {
    cat(s)
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
