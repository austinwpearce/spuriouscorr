test_that("fit_linear returns a list with model and fitted", {
  dat    <- sim_linear(n = 50, seed = 4)
  result <- fit_linear(dat)
  expect_type(result, "list")
  expect_named(result, c("model", "fitted"))
})

test_that("fit_linear model element is an lm object", {
  dat    <- sim_linear(n = 50, seed = 4)
  result <- fit_linear(dat)
  expect_s3_class(result$model, "lm")
})

test_that("fit_linear fitted element is a tibble with x and y_hat", {
  dat    <- sim_linear(n = 50, seed = 4)
  result <- fit_linear(dat)
  expect_s3_class(result$fitted, "tbl_df")
  expect_named(result$fitted, c("x", "y_hat"))
  expect_equal(nrow(result$fitted), 200)
})

test_that("fit_linear recovers slope approximately", {
  dat    <- sim_linear(n = 500, slope = 2, noise = 0.1, seed = 99)
  result <- fit_linear(dat)
  coefs  <- coef(result$model)
  expect_equal(as.numeric(coefs["x"]), 2, tolerance = 0.05)
})
