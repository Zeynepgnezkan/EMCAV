#' GeomFlatViolin
#'
#' A custom ggplot2 geom that creates flat violins for visualizing the distribution of a variable.
#'
#' This geometry allows for a flat violin representation, which is particularly useful for
#' visualizing the density of the data while maintaining a clear view of the underlying data points.
#'
#'
#' @return A ggproto object that defines the flat violin geometry.
#'
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
