#' @import ggplot2
#' @importFrom dplyr "%>%"
NULL

GeomMeanLine <- ggproto(
  "GeomMeanLine", GeomCrossbar,

  setup_data = function(data, params) {
    data_entries <- data %>%
      dplyr::select(-y) %>%
      dplyr::distinct()
    data_summary <- data %>%
      dplyr::group_by(PANEL, group) %>%
      dplyr::summarise(y = mean(y, na.rm = TRUE)) %>%
      dplyr::mutate(ymin = y, ymax = y, width = params$width) %>%
      dplyr::left_join(data_entries, by = c("PANEL", "group"))
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
      dplyr::group_by(PANEL, group) %>%
      dplyr::summarise(mean_y = mean(y, na.rm = TRUE),
                       sem_y = sd(y, na.rm = TRUE) / sqrt(n())) %>%
      dplyr::mutate(y = mean_y, height = sem_y * 1.96 * 2,
                    width = params$width) %>%
      dplyr::left_join(data_entries, by = c("PANEL", "group"))
    GeomTile$setup_data(data_summary, params)
  }
)

GeomMeanBar <- ggproto(
  "GeomMeanBar", GeomCol,

  setup_data = function(data, params) {
    data_entries <- data %>%
      dplyr::mutate(colour = NULL) %>%
      dplyr::select(-y) %>%
      dplyr::distinct()
    data_summary <- data %>%
      dplyr::group_by(PANEL, group) %>%
      dplyr::summarise(y = mean(y, na.rm = TRUE)) %>%
      dplyr::mutate(ymin = y, ymax = y, width = params$width) %>%
      dplyr::left_join(data_entries, by = c("PANEL", "group"))
    GeomCol$setup_data(data_summary, params)
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
#' @param width_points Amount of horizontal jitter added to the locations of the
#'   points. Defaults to 10\% of the resolution of the data.
#' @param width_bars Width of mean bars. Defaults to 90\% of the resolution of
#'   the data.
#' @param width_lines Width of mean lines. Defaults to 90\% of the resolution of
#'   the data.
#' @param width_cis Width of confidence interval boxes. Defaults to 80\% of the
#'   resolution of the data.
#' @param width_violins Width of violins. Defaults to 70\% of the resolution of
#'   the data.
#' @param alpha_points Opacity of the points. Defaults to 1 (not transparent at
#'   all).
#' @param alpha_bars Opacity of the mean bars. Defaults to 0.3.
#' @param alpha_cis Opacity of the confidence interval boxes. Defaults to 0.5.
#' @param alpha_violins Opacity of the violins. Defaults to 0 (completely
#'   transparent).
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
                        width_points = 0.1,
                        width_bars = 0.9,
                        width_lines = 0.9,
                        width_cis = 0.8,
                        width_violins = 0.7,
                        alpha_points = 1,
                        alpha_bars = 0.3,
                        alpha_cis = 0.5,
                        alpha_violins = 0,
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
        width = width_bars,
        alpha = alpha_bars,
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
      geom = GeomViolin,
      position = "dodge",
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        width = width_violins,
        alpha = alpha_violins,
        colour = "darkgrey",
        fill = "white",
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
        width = width_cis,
        alpha = alpha_cis,
        colour = "darkgrey",
        fill = "white",
        size = 0.5,
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
        width = width_lines,
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
      geom = GeomPoint,
      position = position_jitter(width = width_points, height = 0),
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        alpha = alpha_points,
        shape = 1,
        size = 1,
        na.rm = na.rm,
        ...
      )
    )
    layers <- c(layers, points_layer)
  }

  return(layers)

}
