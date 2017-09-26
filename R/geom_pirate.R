#' @import ggplot2
#' @importFrom dplyr "%>%"
NULL

modify_aes <- function(mapping, ...) {
  if (is.null(mapping)) mapping <- aes()
  utils::modifyList(mapping, ...)
}

GeomPointHollow <- ggproto(
  "GeomPointHollow", GeomPoint,
  default_aes = modify_aes(GeomPoint$default_aes, aes(shape = 1))
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

  # draw_key = function(data, params, size) {
  #   data <- data %>% dplyr::mutate(alpha = 1)
  #   GeomCol$draw_key(data, params, size)
  # },

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
#' than the traditional barplot. \code{geom_pirate} plots the raw data as
#' points, along with layers showing descriptive statistics -- horizontal line
#' segments indicating the mean and violin plots indicating density.
#'
#' @export
#'
#' @seealso \code{\link{geom_violin}}
#' @seealso \code{\link{geom_col}}
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_col
#' @inheritParams ggplot2::geom_violin
#' @inheritParams ggplot2::geom_crossbar
#' @inheritParams ggplot2::geom_point
#'
#' @examples
#' ggplot(mpg, aes(x = class, y = displ)) +
#'   geom_pirate(aes(colour = class, fill = class))
geom_pirate <- function(mapping = NULL, data = NULL,
                        ...,
                        trim = TRUE,      # violin
                        scale = "area",   # violin
                        fatten = 2.5,     # crossbar
                        width = 0.9,      # crossbar/col/point
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {

  list(

    # bars
    layer(
      data = data,
      mapping = mapping,
      stat = "identity",
      geom = GeomMeanBar,
      position = "stack",
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        width = width,
        na.rm = na.rm,
        ...
      )
    ),

    # density
    layer(
      data = data,
      mapping = mapping,
      stat = "ydensity",
      geom = GeomViolinBkg,
      position = "dodge",
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        width = width * 0.9,
        trim = trim,
        scale = scale,
        na.rm = na.rm,
        ...
      )
    ),

    # means
    layer(
      data = data,
      mapping = mapping,
      stat = "identity",
      geom = GeomMeanLine,
      position = "identity",
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        width = width,
        fatten = fatten,
        na.rm = na.rm,
        ...
      )
    ),

    # points
    layer(
      data = data,
      mapping = mapping,
      stat = "identity",
      geom = GeomPointHollow,
      position = position_jitter(width = width * 0.1, height = 0),
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        ...
      )
    )
  )
}
