#' Fit a linear regression model
#'
#' Fits a simple linear regression model (`y ~ x`) to a data frame and returns
#' the model object along with a convenience data frame of fitted values.
#'
#' @param data A data frame with columns `x` and `y`.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{`model`}{The fitted [`lm`][stats::lm] object.}
#'     \item{`fitted`}{A [tibble::tibble()] with columns `x` and `y_hat`
#'       containing predicted values along the range of `x`.}
#'   }
#' @export
#'
#' @examples
#' dat <- sim_linear(n = 50, slope = 1.5, seed = 1)
#' result <- fit_linear(dat)
#' summary(result$model)
#' head(result$fitted)
fit_linear <- function(data) {
  model <- stats::lm(y ~ x, data = data)
  x_seq <- seq(min(data$x), max(data$x), length.out = 200)
  y_hat <- stats::predict(model, newdata = data.frame(x = x_seq))
  fitted <- tibble::tibble(x = x_seq, y_hat = y_hat)
  list(model = model, fitted = fitted)
}
