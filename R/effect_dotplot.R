
#' Get effects from a linear model.
#'
#' @param model A linear model object (`lm` or `rsm`).
#' @param scaling Multiply all model coefficients by this factor.
#' @param no_scale Character vector of effects to not scale (default=`"(Intercept)"`).
#' @param intercept When `FALSE` (default), exclude the intercept.
#'
#' @author Paul Jensen, \email{software@jensenlab.net}
#'
#' @export
get_effects <- function(model, scaling=1, no_scale="(Intercept)", intercept=FALSE) {
  effs <- coefficients(model)
  effs[!(names(effs) %in% no_scale)] <- effs[!(names(effs) %in% no_scale)] * scaling
  if (!intercept) {
    effs[!(names(effs) %in% no_scale)]
  }
  effs
}

#' Create a dot plot of effects.
#'
#' @param model Linear model.
#' @param scaling Multiply all model coefficients by this factor.
#' @param limits Numeric vector `c(lower, upper)` Effects outsize this range are moved to the range boundaries.
#' @param ... Optional arguments for `ggplot2::geom_dotplot`.
#'
#' @author Paul Jensen, \email{software@jensenlab.net}
#'
#' @export
effect_dotplot <- function(model, scaling=1, limits=NULL, ...) {
  odr <- attr(model$terms, "order")
  order_labels <- rep("3+", max(odr))
  order_labels[1] <- "1 (main)"
  order_labels[2] <- "2"
  df <- data.frame(
    effect = scaling*coefficients(model)[attr(model$terms, "term.labels")],
    order = order_labels[odr]
  )

  if (!is.null(limits)) {
    df$effect[df$effect < limits[1]] <- limits[1]
    df$effect[df$effect > limits[2]] <- limits[2]
  }
  (ggplot2::ggplot(df, ggplot2::aes(x=effect, fill=order))
    + ggplot2::geom_dotplot(...)
    + ggpubr::theme_pubr()
    + ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     legend.position = "bottom")
    + ggplot2::xlab("effect size")
    + ggplot2::ylab(""))
}
