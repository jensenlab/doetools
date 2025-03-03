% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show_model.R
\name{show_model}
\alias{show_model}
\title{Show a compact model summary}
\usage{
show_model(
  x,
  digits = max(3L, getOption("digits") - 3L),
  symbolic.cor = x$symbolic.cor,
  signif.stars = getOption("show.signif.stars"),
  n_coefs = NA,
  order_coefs = abs,
  show_coefs = TRUE,
  show_fit = TRUE,
  ...
)
}
\arguments{
\item{n_coefs}{Number of coefficients to display (NA=all).}

\item{order_coefs}{Function used to order coefficients. Default sorts by decending magnitude.}

\item{show_coefs}{If \code{FALSE}, skip the coefficient table.}

\item{show_fit}{If \code{FALSE}, skip the model fit statistics (R^2, F, etc.)}
}
\description{
Prints a modified version of the defaults \code{lm} model summary. The model summary statistics
are shown on top, followed by the (sorted) table of coefficients. The function call and
residual information is omitted.
}
\details{
The function is based on the \code{print.summary.lm} function in \code{base}. Function parameters not documented
here are described in \code{print.summary.lm}.
}
