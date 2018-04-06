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
  }

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

update_aes <- function(mapping, ...) {
  if (is.null(mapping)) return(NULL)
  utils::modifyList(mapping, ...)
}

update_default_arg <- function(arg_name, arg_value) {
  default_vals <- as.list(formals(geom_pirate)[[arg_name]])[-1]
  utils::modifyList(default_vals, arg_value)
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
#' assuming a normal sampling distribution (using \link[ggplot2]{geom_tile}),
#' and violins indicating the density (using \link[ggplot2]{geom_violin}).
#'
#' @export
#'
#' @inheritParams ggplot2::geom_point
#' @param points logical indicating whether to show points
#' @param bars logical indicating whether to show mean bars
#' @param lines logical indicating whether to show mean lines
#' @param cis logical indicating whether to show 95\% confidence intervals boxes
#' @param violins logical indicating whether to show violins
#' @param points_params list of parameters to pass to points layer
#' @param bars_params list of parameters to pass to bars layer
#' @param lines_params list of parameters to pass to lines layer
#' @param cis_params list of parameters to pass to CIs layer
#' @param violins_params list of parameters to pass to violins layer
#' @param jitter_width amount of horizontal jitter added to the locations of the
#'   points (defaults to 20\% of the resolution of the data)
#' @param show.legend logical indicating whether this layer be included in the
#'   legends (defaults to FALSE)
#'
#' @examples
#' ggplot(mpg, aes(x = class, y = cty)) +
#'   geom_pirate(aes(colour = class))
geom_pirate <- function(mapping = NULL, data = NULL,
                        ...,
                        points = TRUE,
                        bars = TRUE,
                        lines = TRUE,
                        cis = TRUE,
                        violins = TRUE,
                        points_params = list(shape = 1, size = 1),
                        bars_params = list(alpha = 0.3, width = 0.9),
                        lines_params = list(size = 0.5, width = 0.9),
                        cis_params = list(fill = "white", size = 0.5,
                                          alpha = 0.5, width = 0.8),
                        violins_params = list(fill = "white", size = 0.5,
                                              alpha = 0, width = 0.7),
                        jitter_width = 0.2,
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
    bars_params <- update_default_arg("bars_params", bars_params)
    bars_layer <- layer(
      data = data,
      mapping = update_aes(mapping, aes_(colour = NULL, fill = group)),
      stat = "summary",
      geom = GeomCol,
      position = position_dodge(width = 0.9),
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = purrr::flatten(list(
        bars_params,
        list(
          fun.y = "mean",
          na.rm = na.rm,
          ...
        )
      ))
    )
    layers <- c(layers, bars_layer)
  }

  if (violins) {
    violins_params <- update_default_arg("violins_params", violins_params)
    violin_layer <- layer(
      data = data,
      mapping = mapping,
      stat = StatYdensityPirate,
      geom = GeomViolin,
      position = position_dodge(width = 0.9),
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = purrr::flatten(list(
        violins_params,
        list(
          na.rm = na.rm,
          ...
        )
      ))
    )
    layers <- c(layers, violin_layer)
  }

  if (cis) {
    cis_params <- update_default_arg("cis_params", cis_params)
    cis_layer <- layer(
      data = data,
      mapping = mapping,
      stat = StatCI,
      geom = "tile",
      position = position_dodge(width = 0.9),
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = purrr::flatten(list(
        cis_params,
        list(
          na.rm = na.rm,
          ...
        )
      ))
    )
    layers <- c(layers, cis_layer)
  }

  if (lines) {
    lines_params <- update_default_arg("lines_params", lines_params)
    lines_layer <- layer(
      data = data,
      mapping = mapping,
      stat = "summary",
      geom = GeomCrossbarPirate,
      position = position_dodge(width = 0.9),
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = purrr::flatten(list(
        lines_params,
        list(
          fun.y = "mean",
          fun.ymax = "mean",
          fun.ymin = "mean",
          na.rm = na.rm,
          ...
        )
      ))
    )
    layers <- c(layers, lines_layer)
  }

  points_position <- position_jitterdodge(jitter.width = jitter_width,
                                          jitter.height = 0,
                                          dodge.width = 0.9)
  if (points) {
    points_params <- update_default_arg("points_params", points_params)
    points_layer <- layer(
      data = data,
      mapping = mapping,
      stat = "identity",
      geom = GeomPoint,
      position = points_position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = purrr::flatten(list(
        points_params,
        list(
          na.rm = na.rm,
          ...
        )
      ))
    )
    layers <- c(layers, points_layer)
  }

  return(layers)

}
