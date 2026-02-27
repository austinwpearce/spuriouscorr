#' Simulate linear trend data
#'
#' Creates a data frame of simulated XY data with a linear trend.
#'
#' @param n Integer. Number of observations. Default is 100.
#' @param slope Numeric. Slope of the linear trend. Positive values give an
#'   upward trend, negative values give a downward trend. Default is 1.
#' @param noise Numeric. Standard deviation of random noise added to Y.
#'   Default is 1.
#' @param seed Integer or NULL. Random seed for reproducibility. Default is NULL.
#'
#' @return A [tibble::tibble()] with columns `x` and `y`.
#' @export
#'
#' @examples
#' sim_linear(n = 50, slope = 2, noise = 0.5)
#' sim_linear(n = 100, slope = -1, seed = 42)
sim_linear <- function(n = 100, slope = 1, noise = 1, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  x <- stats::runif(n, min = 0, max = 10)
  y <- slope * x + stats::rnorm(n, mean = 0, sd = noise)
  tibble::tibble(x = x, y = y)
}

#' Simulate cloud (potato) data
#'
#' Creates a data frame of simulated XY data with very low correlation,
#' appearing as a cloud or potato shape when plotted.
#'
#' @param n Integer. Number of observations. Default is 100.
#' @param seed Integer or NULL. Random seed for reproducibility. Default is NULL.
#'
#' @return A [tibble::tibble()] with columns `x` and `y`.
#' @export
#'
#' @examples
#' sim_cloud(n = 200, seed = 1)
sim_cloud <- function(n = 100, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  x <- stats::rnorm(n, mean = 5, sd = 2)
  y <- stats::rnorm(n, mean = 5, sd = 2)
  tibble::tibble(x = x, y = y)
}

#' Simulate U-shaped data
#'
#' Creates a data frame of simulated XY data with a U-shaped (or inverted
#' U-shaped) pattern.
#'
#' @param n Integer. Number of observations. Default is 100.
#' @param direction Character. Either `"up"` for a U-shape (opening upward) or
#'   `"down"` for an inverted U-shape (opening downward). Default is `"up"`.
#' @param noise Numeric. Standard deviation of random noise added to Y.
#'   Default is 0.5.
#' @param seed Integer or NULL. Random seed for reproducibility. Default is NULL.
#'
#' @return A [tibble::tibble()] with columns `x` and `y`.
#' @export
#'
#' @examples
#' sim_ushape(n = 100, direction = "up", seed = 7)
#' sim_ushape(n = 100, direction = "down")
sim_ushape <- function(n = 100, direction = c("up", "down"), noise = 0.5,
                       seed = NULL) {
  direction <- match.arg(direction)
  if (!is.null(seed)) set.seed(seed)
  x <- stats::runif(n, min = -5, max = 5)
  curve <- x^2
  if (direction == "down") curve <- -curve
  y <- curve + stats::rnorm(n, mean = 0, sd = noise)
  tibble::tibble(x = x, y = y)
}

#' Simulate star-shaped data
#'
#' Creates a data frame of simulated XY data arranged in a star shape,
#' with radiating arms from the center.
#'
#' @param n Integer. Approximate total number of observations. Default is 100.
#' @param arms Integer. Number of arms in the star. Default is 6.
#' @param noise Numeric. Standard deviation of random noise applied perpendicular
#'   to each arm. Default is 0.15.
#' @param seed Integer or NULL. Random seed for reproducibility. Default is NULL.
#'
#' @return A [tibble::tibble()] with columns `x` and `y`.
#' @export
#'
#' @examples
#' sim_star(n = 120, arms = 6, seed = 99)
#' sim_star(n = 80, arms = 4)
sim_star <- function(n = 100, arms = 6, noise = 0.15, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_per_arm <- max(1L, as.integer(n / arms))
  angles <- seq(0, pi * (1 - 1 / arms), length.out = arms)
  dfs <- lapply(angles, function(theta) {
    r <- stats::runif(n_per_arm, min = 0, max = 5)
    perp <- stats::rnorm(n_per_arm, mean = 0, sd = noise * 5)
    tibble::tibble(
      x = r * cos(theta) + perp * sin(theta),
      y = r * sin(theta) + perp * cos(theta)
    )
  })
  do.call(rbind, dfs)
}
