#' @import ggplot2
#' @importFrom dplyr "%>%"
NULL

modify_aes <- function(mapping, ...) {
  if (is.null(mapping)) mapping <- aes()
  utils::modifyList(mapping, ...)
}

GeomPointHollow <- ggproto(
  "GeomPointHollow", GeomPoint,
  default_aes = modify_aes(GeomPoint$default_aes, aes(shape = 1, size = 1))
)

GeomMeanLine <- ggproto(
  "GeomMeanLine", GeomCrossbar,

  setup_data = function(data, params) {
    data_entries <- data %>%
      dplyr::select(-y) %>%
      dplyr::distinct()
    data_summary <- data %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(y = mean(y)) %>%
      dplyr::mutate(ymin = y, ymax = y, width = params$width) %>%
      dplyr::left_join(data_entries, by = "group")
    GeomCrossbar$setup_data(data_summary, params)
  },

  required_aes = c("x", "y")
)

GeomCI <- ggproto(
  "GeomCI", GeomTile,

  setup_data = function(data, params) {
    data_entries <- data %>%
      dplyr::mutate(fill = NULL) %>%
      dplyr::select(-y) %>%
      dplyr::distinct()
    data_summary <- data %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(mean_y = mean(y),
                       sem_y = sd(y) / sqrt(n())) %>%
      dplyr::mutate(y = mean_y, height = sem_y * 1.96 * 2,
                    width = params$width) %>%
      dplyr::left_join(data_entries, by = "group")
    GeomTile$setup_data(data_summary, params)
  },

  default_aes = modify_aes(GeomTile$default_aes,
                           aes(alpha = 0.5, fill = "white", size = 0.5,
                               colour = "darkgrey"))
)

GeomMeanBar <- ggproto(
  "GeomMeanBar", GeomCol,

  setup_data = function(data, params) {
    data_entries <- data %>%
      dplyr::mutate(colour = NULL) %>%
      dplyr::select(-y) %>%
      dplyr::distinct()
    data_summary <- data %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(y = mean(y)) %>%
      dplyr::mutate(ymin = y, ymax = y, width = params$width) %>%
      dplyr::left_join(data_entries, by = "group")
    GeomCol$setup_data(data_summary, params)
  },

  default_aes = modify_aes(GeomCol$default_aes, aes(alpha = 0.3))

)

GeomViolinBkg <- ggproto(
  "GeomViolinBkg", GeomViolin,

  draw_group = function(self, data, ...) {
    data <- data %>% dplyr::mutate(colour = "darkgrey", fill = "transparent")
    GeomViolin$draw_group(data, ...)
  }

)

#' Pirate plots
#'
#' A pirate plot
#' (\url{https://cran.r-project.org/web/packages/yarrr/vignettes/pirateplot.html})
#' is a way of displaying data where a continuous dependent variable is a
#' function of a categorical independent variable, in a more informative way
#' than the traditional barplot. \code{geom_pirate} plots the raw data as points
#' (using \link[ggplot2]{geom_jitter}), along with layers showing descriptive
#' and inferential statistics -- bars indicating means (using
#' \link[ggplot2]{geom_col}), horizontal line indicating means (using
#' \link[ggplot2]{geom_crossbar}), boxes indicating 95\% confidence intervals
#' assuming a normal sampling distribution (using \link[ggplot2]{geom_rect}),
#' and violins indicating the density (using \link[ggplot2]{geom_violin}).
#'
#' @export
#'
#' @inheritParams ggplot2::geom_violin
#' @param points logical indicating whether to show points
#' @param bars logical indicating whether to show mean bars
#' @param lines logical indicating whether to show mean lines
#' @param cis logical indicating whether to show confidence intervals boxes
#' @param violins logical indicating whether to show violins
#' @param point_width Amount of horizontal jitter added to the locations of the
#'   points. Defaults to 10\% of the resolution of the data.
#' @param bar_width Width of mean bars. Defaults to 90\% of the resolution of
#'   the data.
#' @param line_width Width of mean lines. Defaults to 90\% of the resolution of
#'   the data.
#' @param ci_width Width of confidence interval boxes. Defaults to 80\% of the
#'   resolution of the data.
#' @param violin_width Width of violins. Defaults to 70\% of the resolution of
#'   the data.
#' @param fatten A multiplicative factor used to increase the size of the mean
#'   lines.
#' @param show.legend logical indicating whether this layer be included in the
#'   legends? NA includes if any aesthetics are mapped. FALSE (the default)
#'   never includes, and TRUE always includes.
#'
#' @examples
#' ggplot(mpg, aes(x = class, y = displ)) +
#'   geom_pirate()
#'
#' ggplot(mpg, aes(x = class, y = displ)) +
#'   geom_pirate(aes(colour = class, fill = class))
geom_pirate <- function(mapping = NULL, data = NULL,
                        ...,
                        points = TRUE,
                        bars = TRUE,
                        lines = TRUE,
                        cis = TRUE,
                        violins = TRUE,
                        point_width = 0.1,
                        bar_width = 0.9,
                        line_width = 0.9,
                        ci_width = 0.8,
                        violin_width = 0.7,
                        trim = TRUE,         # geom_violin
                        scale = "area",      # geom_violin
                        fatten = 2.5,        # geom_crossbar
                        na.rm = FALSE,
                        show.legend = FALSE,
                        inherit.aes = TRUE) {

  layers <- c()

  if (bars) {
    bars_layer <- layer(
      data = data,
      mapping = mapping,
      stat = "identity",
      geom = GeomMeanBar,
      position = "stack",
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        width = bar_width,
        na.rm = na.rm,
        ...
      )
    )
    layers <- c(layers, bars_layer)
  }

  if (violins) {
    violin_layer <- layer(
      data = data,
      mapping = mapping,
      stat = "ydensity",
      geom = GeomViolinBkg,
      position = "dodge",
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        width = violin_width,
        trim = trim,
        scale = scale,
        na.rm = na.rm,
        ...
      )
    )
    layers <- c(layers, violin_layer)
  }

  if (cis) {
    cis_layer <- layer(
      data = data,
      mapping = mapping,
      stat = "identity",
      geom = GeomCI,
      position = "identity",
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        width = ci_width,
        na.rm = na.rm,
        ...
      )
    )
    layers <- c(layers, cis_layer)
  }

  if (lines) {
    lines_layer <- layer(
      data = data,
      mapping = mapping,
      stat = "identity",
      geom = GeomMeanLine,
      position = "identity",
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        width = line_width,
        fatten = fatten,
        na.rm = na.rm,
        ...
      )
    )
    layers <- c(layers, lines_layer)
  }

  if (points) {
    points_layer <- layer(
      data = data,
      mapping = mapping,
      stat = "identity",
      geom = GeomPointHollow,
      position = position_jitter(width = point_width, height = 0),
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        ...
      )
    )
    layers <- c(layers, points_layer)
  }

  return(layers)

}
