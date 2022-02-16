
#' Calculate dispersion for a replicated experiment.
#'
#' Collects replicate runs and calculates the location (mean)
#' and dispersion (\eqn{\ln s^2}{ln(s^2)}), where \eqn{s^2}{s^2}
#' is the sample variance and \eqn{\ln}{ln} is the natural logarithm.
#'
#' Returns a data frame with new column \code{lns2}. The column with
#' the response is replaced with the mean response across the replicates.
#'
#' @param data Data frame (or similar)
#' @param factors Character vector of factor names
#' @param response Name of the response column
#' @param df If \code{TRUE}, the output is coerced with \code{as.data.frame}. Otherwise a \code{tibble} is returned.
#'
#' @export
add_dispersion <- function(data, factors, response="response", df=TRUE) {
  data %<>%
    dplyr::group_by(.dots={{factors}}) %>%
    dplyr::summarize(location__ = mean(.data[[response]]), lns2 = log(var(.data[[response]]))) %>%
    dplyr::ungroup()
  data[[response]] <- data$location__
  data$location__ <- NULL
  if (df) {
    as.data.frame(data)
  } else {
    df  # return a tibble
  }
}
