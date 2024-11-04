#' Flat Violin Plot Geom for ggplot2
#'
#' This function adds a custom flat violin plot geom to ggplot2. Unlike the traditional
#' violin plot, this version displays only one side of the density plot, making it
#' ideal for raincloud plots and other compact visualizations. This geom was inspired by a solution
#' to issues raised on social media and utilizes modified code from ggplot2's `geom_violin`.
#'
#' @source https://github.com/gabrifc/raincloud-shiny
#'
#' @references Original issue raised on Twitter: \url{https://twitter.com/EamonCaddigan/status/646759751242620928}
#' ggplot2's `geom_violin` source: \url{https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r}
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()}. If specified, it
#' overrides the default mapping at the plot level.
#' @param data A data frame. If specified, it overrides the default data at the plot level.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param trim Logical, whether to trim the tails of the violins to the range of the data.
#' @param scale If "area" (default), all violins have the same area; if "count," areas are scaled proportionally
#' to the number of observations; if "width," all violins have the same maximum width.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics rather than combining with them.
#' @param ... Additional arguments passed to the layer.
#'
#' @return A ggplot2 layer for flat violin plots, useful in creating raincloud plots.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_flat_violin(trim = FALSE)
#'
#' @export
geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @export
GeomFlatViolin <- ggproto("GeomFlatViolin", Geom,
                          setup_data = function(data, params) {
                            data$width <- data$width %||% params$width %||% (resolution(data$x, FALSE) * 0.9)
                            data %>%
                              group_by(group) %>%
                              mutate(ymin = min(y),
                                     ymax = max(y),
                                     xmin = x,
                                     xmax = x + width / 2)
                          },

                          draw_group = function(data, panel_scales, coord) {
                            data <- transform(data, xminv = x,
                                              xmaxv = x + violinwidth * (xmax - x))
                            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                                             plyr::arrange(transform(data, x = xmaxv), -y))
                            newdata <- rbind(newdata, newdata[1,])
                            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
                          },

                          draw_key = draw_key_polygon,

                          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                                            alpha = NA, linetype = "solid"),
                          required_aes = c("x", "y")
)
