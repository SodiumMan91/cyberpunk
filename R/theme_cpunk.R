
#' @title The Cyberpunk Theme#'
#'
#' @param text.font text font, Default: mono
#' @param title.font title font, Default: mono
#' @param legend.font legend font, Default: mono
#' @param title.size title font size, Default: 18
#' @param text.size text font size, Default: 14
#' @param subtitle.size subtitle font size, Default: 12
#' @param axis.title.size axis title font size, Default: 14
#' @param axis.text.size axis text font size, Default: 10
#' @param legend.title.size legend title font size, Default: 10
#' @param legend.text.size legend text font size, Default: 9
#' @param title.color title color, Default = "#ffffff"
#' @param subtitle.color subtitle color, Default = "#ffffff"
#' @param text.color text color, Default = "#ffffff"
#' @param axis.title.color axis title color, Default = "#ffffff"
#' @param axis.text.color axis text color, Default = "#ffffff"
#' @param legend.title.color legend title color, Default = "#ffffff"
#' @param legend.text.color legend text color, Default = "#ffffff"
#' @param legend.position legend position, Default: "bottom"
#' @param ticks add axis ticks, Default: FALSE
#' @examples
#' ggplot(airquality, aes(x = Day, y = Temp, group = as.factor(Month),
#'         color = as.factor(Month))) + geom_point(size = 2.5) +   theme_cpunk()
#' @rdname theme_cpunk
#' @seealso [ggplot2::theme]
#' @importFrom ggplot2 theme element_text element_rect element_blank element_line
#' theme_minimal
#' @importFrom grid unit
#' @return ggplot2 theme object
#' @export


theme_cpunk <- function(
    text.font = "mono",
    title.font = "mono",
    legend.font = "mono",
    title.size = 18,
    text.size = 14,
    subtitle.size = 12,
    axis.title.size = 14,
    axis.text.size = 10,
    legend.title.size = 10,
    legend.text.size = 9,
    title.color = "#ffffff",
    subtitle.color = "#ffffff",
    text.color = "#ffffff",
    axis.title.color = "#ffffff",
    axis.text.color = "#ffffff",
    legend.title.color = "#ffffff",
    legend.text.color = "#ffffff",
    legend.position = "bottom",
    ticks = FALSE) {
  cpunk <- ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "gray30"),
        panel.grid.minor = element_line(color = "gray20"),
        plot.title = element_text(family = title.font, color = title.color, size = title.size, face = "bold"),
        plot.subtitle = element_text(color = subtitle.color, size = subtitle.size),
        axis.title = element_text(family = text.font, color = axis.title.color, size = axis.title.size, face = "bold"),
        axis.text = element_text(family = text.font, color = axis.text.color, size = axis.text.size, face = "bold"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(family = legend.font, color = legend.text.color, size = legend.text.size, face = "bold"),
        legend.title = element_text(family = legend.font, color = legend.title.color, size = legend.title.size, face = "bold"),
        legend.position = legend.position
      )

    scale_color_cpunk()
    scale_fill_cpunk()
    ggplot2::update_geom_defaults("point", list(color = "#E200F7", stroke = 1))
    ggplot2::update_geom_defaults("line", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("bar", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("col", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("density", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("boxplot", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("violin", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("smooth", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("area", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("tile", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("text", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("errorbar", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("ribbon", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("polygon", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("path", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("step", list(color = "#E200F7"))
    ggplot2::update_geom_defaults("abline", list(color = "#E200F7"))

    if (ticks == FALSE) {
      cpunk <- cpunk + theme(axis.ticks = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.ticks.y = element_blank())
    } else {
      cpunk <- cpunk + theme(axis.ticks = element_line(linewidth = 0.15, color = "white"),
                             axis.ticks.x = element_line(linewidth = 0.15, color = "white"),
                             axis.ticks.y = element_line(linewidth = 0.15, color = "white"),
                             axis.ticks.length = grid::unit(4, "pt"))
    }

    return(cpunk)
}
