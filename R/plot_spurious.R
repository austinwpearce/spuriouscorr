#' Dark Tron-like ggplot2 theme
#'
#' A ggplot2 theme inspired by the Tron aesthetic: near-black background with
#' cyan/electric-blue grid lines and text.
#'
#' @param base_size Numeric. Base font size in points. Default is 12.
#' @param base_family Character. Base font family. Default is `""`.
#'
#' @return A ggplot2 [ggplot2::theme()] object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(sim_linear(seed = 1), aes(x, y)) +
#'   geom_point() +
#'   theme_tron()
theme_tron <- function(base_size = 12, base_family = "") {
  bg_color     <- "#0a0a0f"
  grid_color   <- "#1a3a4a"
  text_color   <- "#00e5ff"
  axis_color   <- "#00b4cc"
  panel_color  <- "#0d1b2a"

  ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Plot background
      plot.background  = ggplot2::element_rect(fill = bg_color, color = NA),
      panel.background = ggplot2::element_rect(fill = panel_color, color = NA),

      # Grid lines
      panel.grid.major = ggplot2::element_line(color = grid_color,
                                               linewidth = 0.4),
      panel.grid.minor = ggplot2::element_line(color = grid_color,
                                               linewidth = 0.2),

      # Axes
      axis.line  = ggplot2::element_line(color = axis_color, linewidth = 0.6),
      axis.ticks = ggplot2::element_line(color = axis_color, linewidth = 0.4),
      axis.text  = ggplot2::element_text(color = text_color,
                                         size = base_size * 0.8),
      axis.title = ggplot2::element_text(color = text_color,
                                         size = base_size,
                                         margin = ggplot2::margin(4, 4, 4, 4)),

      # Titles and captions
      plot.title    = ggplot2::element_text(color = text_color,
                                            size = base_size * 1.3,
                                            face = "bold",
                                            margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(color = axis_color,
                                            size = base_size,
                                            margin = ggplot2::margin(b = 4)),
      plot.caption  = ggplot2::element_text(color = grid_color,
                                            size = base_size * 0.75,
                                            margin = ggplot2::margin(t = 6)),

      # Legend
      legend.background = ggplot2::element_rect(fill = bg_color, color = NA),
      legend.text       = ggplot2::element_text(color = text_color),
      legend.title      = ggplot2::element_text(color = text_color),
      legend.key        = ggplot2::element_rect(fill = panel_color, color = NA),

      # Margins
      plot.margin = ggplot2::margin(12, 12, 12, 12)
    )
}

#' Plot simulated data with an optional linear fit
#'
#' Creates a scatter plot of XY data using [ggplot2::ggplot()] with the
#' [theme_tron()] dark theme. Optionally overlays a linear regression line.
#'
#' @param data A data frame with columns `x` and `y`.
#' @param fit Logical. If `TRUE`, a linear regression line is added via
#'   [ggplot2::stat_smooth()]. Default is `FALSE`.
#' @param point_color Character. Color of the scatter plot points. Defaults to
#'   `"#00e5ff"` (cyan).
#' @param line_color Character. Color of the regression line (when `fit = TRUE`).
#'   Defaults to `"#ff6b35"` (neon orange).
#' @param point_alpha Numeric. Transparency of points (0–1). Default is `0.7`.
#' @param point_size Numeric. Size of points. Default is `2`.
#' @param title Character or NULL. Optional plot title.
#' @param subtitle Character or NULL. Optional plot subtitle.
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
#'
#' @examples
#' dat <- sim_linear(n = 80, slope = 1.5, seed = 10)
#' plot_spurious(dat)
#' plot_spurious(dat, fit = TRUE, title = "Linear Trend")
#'
#' plot_spurious(sim_cloud(n = 150, seed = 5), title = "Cloud Pattern")
#' plot_spurious(sim_ushape(seed = 3), title = "U-Shaped")
#' plot_spurious(sim_star(seed = 7), title = "Star Shape")
plot_spurious <- function(data,
                          fit         = FALSE,
                          point_color = "#00e5ff",
                          line_color  = "#ff6b35",
                          point_alpha = 0.7,
                          point_size  = 2,
                          title       = NULL,
                          subtitle    = NULL) {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(color = point_color, alpha = point_alpha,
                        size = point_size) +
    theme_tron()

  if (fit) {
    p <- p + ggplot2::stat_smooth(method = "lm", se = TRUE,
                                  color = line_color,
                                  fill  = line_color,
                                  alpha = 0.2,
                                  linewidth = 1)
  }

  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(label = title, subtitle = subtitle)
  }

  p
}
