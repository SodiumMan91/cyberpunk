cyberpunk <- c(
  "#E200F7",
  "#0FF0FC",
  "#450EFF",
  "#D6014D",
  "#02B59F",
  "#ccccff",
  "#ccff00",
  "#FFFFFF"
)

#' @title The Cyberpunk palette
#' @description The Cyberpunk palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname cpunk
#' @export
#' @examples
#' library(scales)
#' show_col(cpunk_pal()(5))
#' @importFrom scales manual_pal
#' @importFrom grDevices colorRampPalette

cpunk_pal <- function(n,
                         type = c("discrete", "continuous"),
                         reverse = FALSE){
  cpunk <- cyberpunk

  if (reverse == TRUE) {
    cpunk <- rev(cpunk)
  }

  if (missing(n)) {
    n <- length(cpunk)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(cpunk)) {
    stop(paste0("Palette does not have ", n, " colors, maximum is ", length(cpunk), "!"))
  }

  cpunk <- switch(type,
                     continuous = grDevices::colorRampPalette(cpunk)(n),
                     discrete = cpunk[1:n])

  cpunk <- scales::manual_pal(cpunk)

  return(cpunk)
}


#' @title scale_color_cpunk
#' @rdname cpunk_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_cpunk()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_cpunk <- function(n, type = "discrete",
                                 reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("color", "cpunk", cpunk_pal(), ...)
  } else {
    ggplot2::scale_color_gradientn(colors = cpunk_pal(n = n, type = type,
                                                         reverse = reverse)(8))
  }
}

#' @title scale_colour_cpunk
#' @rdname cpunk_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_cpunk()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_cpunk <- scale_color_cpunk

#' @title scale_fill_cpunk
#' @rdname cpunk_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'      geom_histogram(aes(fill = class), col = "black", size = 0.1) +
#'      scale_fill_cpunk()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_cpunk <- function(n = NULL, type = "discrete", reverse = FALSE, ...) {
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "cpunk", cpunk_pal(type = "discrete", reverse = reverse), ...)
  } else {
    if (is.null(n)) n <- 256  # Default number of colors for continuous scale
    ggplot2::scale_fill_gradientn(colors = cpunk_pal(type = type, reverse = reverse)(n), ...)
  }
}
