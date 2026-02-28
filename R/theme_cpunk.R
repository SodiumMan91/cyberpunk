#' @title The Cyberpunk Theme
#'
#' @param base_size base font size, all other sizes scale from this. Default: 11
#' @param text.font text font, Default: "mono"
#' @param title.font title font, Default: "mono"
#' @param legend.font legend font, Default: "mono"
#' @param legend.position legend position, Default: "bottom"
#' @param ticks add axis ticks, Default: FALSE
#' @examples
#' ggplot(airquality, aes(x = Day, y = Temp, group = as.factor(Month),
#'         color = as.factor(Month))) + geom_point(size = 2.5) + theme_cpunk()
#'
#' # Scale everything up
#' ggplot(airquality, aes(x = Day, y = Temp, group = as.factor(Month),
#'         color = as.factor(Month))) + geom_point(size = 2.5) + theme_cpunk(base_size = 14)
#'
#' @rdname theme_cpunk
#' @seealso [ggplot2::theme]
#' @importFrom ggplot2 theme element_text element_rect element_blank element_line theme_minimal margin
#' @importFrom grid unit
#' @return ggplot2 theme object
#' @export

theme_cpunk <- function(
    base_size       = 11,
    text.font       = "mono",
    title.font      = "mono",
    legend.font     = "mono",
    legend.position = "bottom",
    ticks           = FALSE) {

  # All sizes derived from base_size — change one number, everything scales
  title.size        <- base_size * 1.6
  subtitle.size     <- base_size * 1.1
  axis.title.size   <- base_size * 1.2
  axis.text.size    <- base_size * 0.9
  legend.title.size <- base_size * 0.95
  legend.text.size  <- base_size * 0.85
  strip.size        <- base_size * 1.1

  cpunk <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(

      # Backgrounds
      panel.background  = element_rect(fill = "black", color = NA),
      plot.background   = element_rect(fill = "black", color = NA),
      legend.background = element_rect(fill = "black", color = NA),

      # Grid
      panel.grid.major  = element_line(color = "gray30"),
      panel.grid.minor  = element_line(color = "gray30"),

      # Title — aligned to full plot width so long titles don't get clipped
      plot.title = element_text(
        family   = title.font,
        color    = "#ffffff",
        size     = title.size,
        face     = "bold",
        margin   = ggplot2::margin(b = base_size * 0.8),
        hjust    = 0       # left-align; change to 0.5 for centered
      ),
      plot.title.position = "plot",   # key fix: title spans full figure width

      # Subtitle
      plot.subtitle = element_text(
        family = text.font,
        color  = "#ffffff",
        size   = subtitle.size,
        margin = ggplot2::margin(b = base_size * 0.5)
      ),
      plot.subtitle.position = "plot",

      # Caption
      plot.caption = element_text(
        family = text.font,
        color  = "gray60",
        size   = base_size * 0.75,
        hjust  = 1
      ),
      plot.caption.position = "plot",

      # Axis titles & text
      axis.title = element_text(
        family = text.font,
        color  = "#ffffff",
        size   = axis.title.size,
        face   = "bold"
      ),
      axis.text = element_text(
        family = text.font,
        color  = "#ffffff",
        size   = axis.text.size,
        face   = "bold"
      ),

      # Facet strips
      strip.text = element_text(
        family = title.font,
        color  = "#ffffff",
        size   = strip.size,
        face   = "bold"
      ),
      strip.background = element_rect(fill = "grey20", color = "black"),

      # Legend
      legend.text = element_text(
        family = legend.font,
        color  = "#ffffff",
        size   = legend.text.size,
        face   = "bold"
      ),
      legend.title = element_text(
        family = legend.font,
        color  = "#ffffff",
        size   = legend.title.size,
        face   = "bold"
      ),
      legend.position = legend.position,

      # Breathing room
      plot.margin = grid::unit(c(1, 1, 1, 1), "cm")
    )

  # Geom defaults — magenta as the default single-color geom color
  ggplot2::update_geom_defaults("point",    list(color = "#E200F7", stroke = 1))
  ggplot2::update_geom_defaults("line",     list(color = "#E200F7"))
  ggplot2::update_geom_defaults("bar",      list(color = "#E200F7", fill = "#E200F7"))
  ggplot2::update_geom_defaults("col",      list(color = "#E200F7"))
  ggplot2::update_geom_defaults("density",  list(color = "#E200F7"))
  ggplot2::update_geom_defaults("boxplot",  list(color = "#E200F7"))
  ggplot2::update_geom_defaults("violin",   list(color = "#E200F7"))
  ggplot2::update_geom_defaults("smooth",   list(color = "#E200F7"))
  ggplot2::update_geom_defaults("area",     list(color = "#E200F7"))
  ggplot2::update_geom_defaults("tile",     list(color = "#E200F7"))
  ggplot2::update_geom_defaults("text",     list(color = "#E200F7"))
  ggplot2::update_geom_defaults("errorbar", list(color = "#E200F7"))
  ggplot2::update_geom_defaults("ribbon",   list(color = "#E200F7"))
  ggplot2::update_geom_defaults("polygon",  list(color = "#E200F7"))
  ggplot2::update_geom_defaults("path",     list(color = "#E200F7"))
  ggplot2::update_geom_defaults("step",     list(color = "#E200F7"))
  ggplot2::update_geom_defaults("abline",   list(color = "#E200F7"))
  ggplot2::update_geom_defaults("vline",    list(color = "#E200F7"))
  ggplot2::update_geom_defaults("hline",    list(color = "#E200F7"))

  # Ticks
  if (ticks == FALSE) {
    cpunk <- cpunk + ggplot2::theme(
      axis.ticks   = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    )
  } else {
    cpunk <- cpunk + ggplot2::theme(
      axis.ticks        = element_line(linewidth = 0.15, color = "white"),
      axis.ticks.x      = element_line(linewidth = 0.15, color = "white"),
      axis.ticks.y      = element_line(linewidth = 0.15, color = "white"),
      axis.ticks.length = grid::unit(4, "pt")
    )
  }

  return(cpunk)
}
