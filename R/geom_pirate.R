#' @import ggplot2
#' @importFrom dplyr "%>%"
NULL

StatCI <- ggproto(
  "StatCI", Stat,

  compute_group = function(data, scales) {
    data %>%
      dplyr::summarise(mean_y = mean(y, na.rm = TRUE),
                       sem_y = sd(y, na.rm = TRUE) / sqrt(n())) %>%
      dplyr::mutate(y = mean_y, height = sem_y * 1.96 * 2)
  },

  finish_layer = function(data, params) {
    data %>% dplyr::mutate(colour = "darkgrey")
  },

  required_aes = c("x", "y")
)

StatYdensityPirate <- ggproto(
  "StatYdensityPirate", StatYdensity,

  finish_layer = function(data, params) {
    data %>% dplyr::mutate(colour = "darkgrey")
  }
)

GeomCrossbarPirate <- ggproto(
  "GeomCrossbarPirate", GeomCrossbar,
  draw_key = draw_key_path
)

add_modify_aes <- function(mapping, ...) {
  if (is.null(mapping)) return(NULL)
  utils::modifyList(mapping, ...)
}


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
#'   geom_pirate(aes(colour = class))
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

  if ("colour" %in% names(mapping)) {
    group <- as.name(mapping$colour)
  } else {
    group <- NULL
  }

  if (bars) {
    bars_layer <- layer(
      data = data,
      mapping = add_modify_aes(mapping, aes_(colour = NULL, fill = group)),
      stat = "summary",
      geom = GeomCol,
      position = position_dodge(width = 0.9),
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        fun.y = "mean",
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
      stat = StatYdensityPirate,
      geom = GeomViolin,
      position = position_dodge(width = 0.9),
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        width = width_violins,
        alpha = alpha_violins,
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
      stat = StatCI,
      geom = "tile",
      position = position_dodge(width = 0.9),
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        width = width_cis,
        alpha = alpha_cis,
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
      stat = "summary",
      geom = GeomCrossbarPirate,
      position = position_dodge(width = 0.9),
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        fun.y = "mean",
        fun.ymax = "mean",
        fun.ymin = "mean",
        width = width_lines,
        fatten = fatten,
        na.rm = na.rm,
        ...
      )
    )
    layers <- c(layers, lines_layer)
  }

  if ("colour" %in% names(mapping)) {
    points_position <- position_jitterdodge(jitter.width = width_points,
                                            jitter.height = 0,
                                            dodge.width = 0.9)
  } else {
    points_position <- position_jitter(width = width_points, height = 0)
  }
  if (points) {
    points_layer <- layer(
      data = data,
      mapping = mapping,
      stat = "identity",
      geom = GeomPoint,
      position = points_position,
      show.legend = show.legend,
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
