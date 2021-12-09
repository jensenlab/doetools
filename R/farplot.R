
#' Plot factors and a response.
#'
#' Shows a continuous response variable and the corresponding treatments (factor levels) for a designed
#' experiment or a linear model.
#'
#' \code{farplot} has three methods for displaying factors:
#' \itemize{
#'   \item{\bold{sign} shows a \code{-}, \code{0}, or \code{+} character based on the sign of the factor.
#'     This can be used for 2- and 3-level factorials. \code{R} factors are assigned a sign based on their
#'     character representation: \code{"-"}, and \code{"-1"} are negative, \code{"0"} is zero, and \code{"+"},
#'     \code{"1"}, and \code{"+1"} are positive.}
#'   \item{\bold{continuous} factors are shown as circles with sizes proportional to the factor level.
#'     Colors distinguish negative, zero, and positive values, with red, gray, and black defaults
#'     (the same as for signs).}
#'   \item{\bold{factors} show non-numeric data (factors or strings) by abbreviating the factor level.}
#' }
#' By default, \code{farplot} guesses the factor based on the class and contents of each factor.
#' \itemize{
#'   \item{A \code{numeric} or \code{integer} vector is displayed as \bold{continuous} unless the vector
#'     only contains the values -1, 0, or 1, in which case it is displayed as a \bold{sign}.}
#'   \item{All other types (e.g. \code{factor}, \code{character}) are displayed as a \bold{factor}.}
#' }
#'
#' @param data Data frame with factors and the response.
#' @param response Name of response column (default=\code{"response"}).
#' @param factors Names of factor to plot \code{NULL} selects all columns except \code{response}.
#' @param factor_type A character vector of the display type (\code{"sign"}, \code{"continuous"},
#'   or \code{"factor"}) for each factor. By default, the function \code{guess_factor_type} finds
#'   factor types using the above rules and the classes of the columns in \code{factors}.
#' @param factor_colors A 3-item vector specifying the colors for negative, zero, and positive symbols.
#'   Default: negative=red, zero=gray, positive=black.
#' @param show_prediction If true (default), make and display predictions for \code{lm} and \code{rsm}
#'   objects.
#' @param color_signs If true (the default), the \code{-}, \code{0}, and \code{+} signs are colored
#'   using \code{factor_colors}.
#' @param label_chars Number of characters to use when abbreviating factor names. Numbers are added to
#'   make the factors unique. For example, if \code{label_chars=2} (the default), \code{"March"} and
#'   \code{"May"} are abbreviated \code{"Ma1"} and \code{"Ma2"}.
#' @param factor_size Default size, in \code{char}, of the factor symbols. Reduce this if symbols
#'   overlap. Default: 1.0.
#' @param zero_size For continuous factors, zeros are displayed at this size. Setting this to zero make
#'   no symbol. Note that zeros for sign factors are displayed as the character \code{0} and are
#'   unaffected by this parameter. Default: 0.1.
#' @param size_transform The symbol for a factor is drawn with \code{size=size_transform(level)} where
#'   \code{level} is the factor level. By default this is \code{sqrt} so the area of the symbol
#'   is proportional to the factor level. \code{NULL} applies no transform.
#' @param normalize The levels of continuous factors can be normalized by row (\code{"row"}) or
#'   across all continuous factors in the dataframe (\code{"all"}). Default: \code{"all"}.
#' @param order_response If true (default), sort the responses by \code{stat}.
#' @param stack_replicates If true (default), responses for replicates are plotted above the same
#'   horizontal position, i.e. only unique treatments are shown on the horizontal.
#' @param response_pch Plotting character for the response. Default: 19 (filled circle).
#' @param response_size Size, in \code{char}, of the response. Default: 1.0.
#' @param response color Color of the response character. Default: \code{"orange"}.
#' @param stat Statistic used to order responses. Default: \code{\link{mean}}.
#' @param show_stat If true, shows the \code{stat} value for the responses.
#'   By default, the stat is shown if at least one treatment is replicated and the responses
#'   are stacked.
#' @param stat_pch Plotting character for the stat. Default: 95 (horizontal line).
#' @param stat_size Size, in \code{char}, of the stat. Default: 0.5.
#' @param stat_color Color of the stat character. Default: \code{"black"}.
#' @param prediction_pch Plotting character for the prediction Default: 19 (filled circle).
#' @param prediction_size Size, in \code{char}, of the prediction. Default: 0.5.
#' @param prediction_color Color of the prediction character. Default: \code{"gray"}.
#' @param extension Axis extension factor; see \code{\link{dataViewport}}.
#'
#' @author Paul Jensen, \email{software@jensenlab.net}
#'
#' @export
farplot <- function(...) UseMethod("farplot")

#' @describeIn farplot Base method for dataframes of factors
#' @export
farplot.data.frame <- function(data, response, factors=NULL, ...) {
  if (is.character(response)) {
    response_name <- response
    response <- data[[response_name]]
    data[[response_name]] <- NULL
  } else {
    response_name <- "response"
  }

  if (!is.null(factors)) {
    data <- data[ ,factors, drop=FALSE]
  }

  make_farplot(factors=data, response=response, response_name=response_name, ...)
}

get_response_name <- function(model) {
  as.character(attr(model$terms, "variables")[[attr(model$terms, "response")+1]])
}

guess_factor_type <- function(factors, exclude=character(0)) {
  factors <- factors[ ,setdiff(names(factors), exclude), drop=FALSE]
  in_types <- vapply(factors, class, "")
  out_types <- character(length(in_types))
  for (i in 1:length(out_types)) {
    if (in_types[i] %in% c("numeric", "integer")) {
      if (all(factors[[i]] %in% c(-1,0,1))) {
        out_types[i] <- "sign"
      } else {
        out_types[i] <- "continuous"
      }
    } else {
      out_types[i] <- "factor"
    }
  }
  names(out_types) <- names(factors)
  out_types
}

#' @describeIn farplot The `data` argument can be used to provide the original factors; otherwise the data will be pulled from the original call.
#' @export
farplot.lm <- function(model, data=NULL, factors=NULL, show_prediction=TRUE, ...) {
  response_name <- get_response_name(model)
  if (is.null(data)) {
    # try to get it from the call
    data <- eval(model$call$data)
  }
  response <- data[[response_name]]
  data[[response_name]] <- NULL
  if (show_prediction) {
    prediction <- predict(model, data)
  } else {
    prediction <- NULL
  }

  if (!is.null(factors)) {
    factor_names <- factors
  } else {
    # the dataframe from the original call might include columns that were
    # not terms in the model; remove these
    factor_names <- dimnames(attr(model$terms,"factors"))[[2]]
  }
  factors <- data[ ,factor_names, drop=FALSE]

  make_farplot(factors, response, prediction, response_name=response_name, ...)
}

#' @describeIn farplot For response surface models created with `rsm::rsm`
#' @export
farplot.rsm <- function(model, factors=NULL, show_prediction=TRUE, ...) {
  response_name <- get_response_name(model)
  data <- model$data
  response <- data[[response_name]]
  data[[response_name]] <- NULL
  if (show_prediction) {
    prediction <- predict(model, data)
  } else {
    prediction <- NULL
  }

  if (!is.null(factors)) {
    factor_names <- factors
  } else {
    # the dataframe from the original call might include columns that were
    # not terms in the model; remove these
    candidates <- c(dimnames(attr(model$terms, "factors"))[[2]], unname(model$newlabs))
    factor_names <- intersect(names(model$data), candidates)
  }
  factors <- data[ ,factor_names, drop=FALSE]

  make_farplot(data, response, prediction, response_name=response_name, ...)
}

sign_ <- function(x) {
  if (is.numeric(x)) {
    return(sign(x))
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  # assume x is a character vector
  X <- integer(length(x))
  for (i in 1:length(x)) {
    X[i] <- switch(x[i], "-1"=-1L, "-"=-1L, "0"=0L, "+"=1L, "1"=1L, "+1"=1L)
  }
  sign(X)
}

make_farplot <- function(factors, response, prediction=NULL,
                         factor_type=guess_factor_type(factors),
                         response_name="response",

                         factor_colors=c("red", "gray", "black"),
                         color_signs=TRUE,
                         label_chars=2,
                         factor_size=1.0,
                         zero_size=0.1,
                         size_transform=sqrt,
                         normalize="all",

                         order_response=TRUE,
                         response_color="orange",
                         response_size=1.0,
                         response_pch=19,

                         stack_replicates=TRUE,
                         stat=mean,
                         show_stat=any(vapply(response, length, 0) > 1),
                         stat_pch=95,
                         stat_size=0.5,
                         stat_color="black",

                         prediction_pch=19,
                         prediction_size=0.5,
                         prediction_color="gray",

                         extension=0.05) {
  if (stack_replicates) {
    df <- dplyr::group_data(dplyr::group_by_all(factors))
    factors <- as.data.frame(df)
    factors$.rows <- NULL
    .rows <- df$.rows
    response <- lapply(.rows, function(x) response[x])
    if (!is.null(prediction)) {
      prediction <- vapply(.rows, function(x) prediction[x[1]], 0.0)
    }
  } else {
    response <- as.list(response)
  }

  nr <- length(response)
  nf <- length(factors)

  if (length(factor_type) != nf) {
    if (length(factor_type) == 1) {
      factor_type <- rep(factor_type, nf)
    } else {
    stop("length(factor_type) does not match the number of factors")
    }
  }
  if (is.null(names(factor_type))) {
    names(factor_type) <- names(factors)
  } else {
    # order the factor type as they appear the DF
    factor_type <- factor_type[names(factors)]
  }

  if (is.null(size_transform)) {
    size_transform <- function(x) x
  }

  factor_name_width <- max(stringWidth(names(factors)))
  left_margin <- max(factor_name_width + unit(0.5, "line"), unit(4, "line"))
  left_margin_lines <- convertWidth(left_margin, "line", valueOnly=TRUE) + 1

  response_stat <- vapply(response, stat, 0.0)
  if (order_response) {
    idxs <- order(response_stat)
    response <- response[idxs]
    response_stat <- response_stat[idxs]
    factors <- factors[idxs, , drop=FALSE]
    if (!is.null(prediction)) {
      prediction <- prediction[idxs]
    }
  }

  response_x <- unlist(lapply(1:nr, function(i) rep(i, length(response[[i]]))))
  response_y <- unlist(response)
  alldata_x <- response_x
  alldata_y <- response_y
  if (!is.null(prediction)) {
    alldata_x <- c(alldata_x, 1:nr)
    alldata_y <- c(alldata_y, prediction)
  }

  grid.newpage()
  pushViewport(plotViewport(c(nf+1,left_margin_lines,1,1)))
  pushViewport(dataViewport(alldata_x, alldata_y, name="plotRegion", extension=extension))

  # main plotting region and responses
  grid.rect()
  grid.points(response_x, response_y, name="dataSymbols", pch=response_pch, size=unit(response_size, "char"), gp=gpar(col=response_color))
  if (!is.null(prediction)) {
    grid.points(1:nr, prediction, name="predictionSymbols", pch=prediction_pch, size=unit(prediction_size, "char"), gp=gpar(col=prediction_color))
  }
  if (show_stat) {
    grid.points(1:nr, response_stat, name="statSymbols", pch=stat_pch, size=unit(stat_size, "char"), gp=gpar(col=stat_color))
  }
  grid.yaxis()
  grid.text(response_name, x=unit(-3, "line"), rot=90)

  pushViewport(viewport(x=0, y=0, just=c("left","top"),
                        xscale=extendrange(1:nr, f=extension), height=unit(nf, "line")))
  grid.text(names(factors), y=unit(nf+0.5-(1L:nf), "line"), x=unit(-0.5, "line"), just="right")

  global_max <- -Inf
  for (i in 1:nf) {
    if (factor_type[i] == "continuous") {
      global_max <- max(global_max, abs(factors[[i]]))
    }
  }
  # avoid divide by zero when scaling
  if (abs(global_max) < 1e-5) {
    global_max <- 1.0
  }

  scale_codes <- function(codes) {
    scaling <- ifelse(normalize == "all", global_max, max(abs(codes)))
    size_transform(abs(codes)/scaling)
  }


  for (i in 1:nf) {
    if (factor_type[i] == "sign") {
      signs <- sign_(factors[ ,i])
      codes <- rep("0", nr)
      codes[signs < 0] <- "-"
      codes[signs > 0] <- "+"
      if (color_signs) {
        sign_colors <- factor_colors
      } else {
        sign_colors <- rep("black", 3)
      }
      names(sign_colors) <- c("-", "0", "+")
      grid.text(codes,
                x=unit(1:nr, "native"),
                y=unit(nf+0.5-i, "line"),
                just="center",
                gp=gpar(fontface="bold", col=sign_colors[codes]))
    } else if (factor_type[i] == "factor") {
      f <- as.factor(factors[ ,i])
      labels <- vapply(levels(f), function(x) substr(x, 1, label_chars), "")
      labels <- paste0(labels, ifelse(duplicated(labels) | duplicated(labels, fromLast=TRUE), 1:length(labels), ""))
      grid.text(labels,
                x=unit(1:nr, "native"),
                y=unit(nf+0.5-i, "line"),
                just="center")
    } else if (factor_type[i] == "continuous") {
      f <- as.numeric(factors[ ,i])
      sf <- scale_codes(f)
      sf[sf == 0] <- size_transform(zero_size)
      colors <- rep(factor_colors[2], nr)
      colors[sign(f) < 0] <- factor_colors[1]
      colors[sign(f) > 0] <- factor_colors[3]
      fills <- colors
      fills[f == 0] <- "white"
      grid.points(
        x=unit(1:nr, "native"),
        y=unit(rep(nf+0.5-i, nr), "line"),
        size=sf * unit(factor_size, "char"),
        pch=21,
        gp=gpar(col=colors, fill=fills)
      )
    } else {
      stop("invalid factor_type")
    }
  }
}


# data(cement)
# model_cement <- rsm(y ~ SO(x1,x2,x3) + Block, data=cement)
# farplot(model_cement)
#
# data(chem)
# model_chem <- lm(y ~ A + B + C + D, data=chem)
# farplot(model_chem)
#
# data(cont)
# farplot(cont, response="lns2")
#
# data(cpipe)
# model_cpipe <- lm(y ~ A+B+P+Q+WP, data=cpipe)
# farplot(model_cpipe)
#
# data(eptaxr)
# model_eptaxr <- lm(y ~ A+B+C+D+E, data=eptaxr)
# farplot(model_eptaxr, stack_replicates=TRUE)
