
#' Plot factors and a response.
#'
#' @param data Data frame.
#' @param response Name of response column. `NULL` uses the last column.
#' @param factors Names of factor columns. `NULL` selects all columns except `response`.
#' @param size Default size, in `char`, or the response symbols. Reduce this if symbols overlap.
#' @param pch Symbol type for response.
#' @param factor_colors Colors for negative and positive factors.
#' @param normalize If `"all"` (default), factor sizes are normalized to the largest single factor code.
#'   Otherwise, each row is normalized separately.
#' @param size_tranform Function applied to convert factor codes to symbol sizes. Default is `sqrt`.
#'   `NULL` applies no transform.
#' @param extension Axis extension factor; see `dataViewport`.
#'
#' @export
farplot <- function(data, response=NULL, factors=NULL,
                    size=1.0, pch=19,
                    factor_colors=c("red","black"),
                    normalize="all",
                    size_transform=sqrt,
                    extension=0.05) {

  if (!is.null(response)) {
    responses <- data[[response]]
  } else {
    responses <- data[[-1]]
    response <- "response"  # for y-axis label
  }
  nr <- length(responses)

  if (is.null(factors)) {
    factors <- setdiff(names(data), response)
  }
  nf <- length(factors)

  if (is.null(size_transform)) {
    size_transform <- function(x) x
  }

  factor_name_width <- max(stringWidth(factors))
  left_margin <- max(factor_name_width + unit(0.5, "line"), unit(4, "line"))
  left_margin_lines <- convertWidth(left_margin, "line", valueOnly=TRUE) + 1

  idxs <- order(responses)
  responses <- responses[idxs]

  grid.newpage()

  pushViewport(plotViewport(c(nf+1,left_margin_lines,1,1)))
  pushViewport(dataViewport(1:nr, responses, name="plotRegion", extension=extension))

  # main plotting region and responses
  grid.rect()
  grid.points(1:nr, responses, name="dataSymbols", pch=pch, size=unit(size, "char"))
  grid.yaxis()
  grid.text(response, x=unit(-3, "line"), rot=90)

  pushViewport(viewport(x=0, y=0, just=c("left","top"),
                        xscale=extendrange(1:nr, f=extension), height=unit(nf, "line")))
  grid.text(factors, y=unit(nf+0.5-(1L:nf), "line"), x=unit(-0.5, "line"), just="right")

  global_max <- -Inf
  for (fact in factors) {
    if (is.numeric(data[[fact]])) {
      global_max <- max(global_max, abs(data[[fact]]))
    }
  }

  scale_codes <- function(codes) {
    scaling <- ifelse(normalize == "all", global_max, max(abs(codes)))
    size_transform(abs(codes)/scaling)
  }

  for (i in 1:nf) {
    fact <- factors[i]
    codes <- data[[fact]][idxs]
    if (!is.numeric(codes)) {
      grid.text(as.character(as.integer(as.factor(codes))),
                x=unit(1:nr, "native"),
                y=unit(nf+0.5-i, "line"),
                just="center")
    } else {
      # negative points
      locs <- which(codes < 0)
      grid.points(
        x=unit(locs, "native"),
        y=unit(rep_len(nf+0.5-i, length(locs)), "line"),
        size=scale_codes(codes[locs]) * unit(size, "char"),
        pch=21,
        gp=gpar(col=factor_colors[1], fill=factor_colors[1])
      )

      # positive points
      locs <- which(codes > 0)
      grid.points(
        x=unit(locs, "native"),
        y=unit(rep_len(nf+0.5-i, length(locs)), "line"),
        size=scale_codes(codes[locs]) * unit(size, "char"),
        pch=21,
        gp=gpar(col=factor_colors[2], fill=factor_colors[2])
      )
    }
  }
}

