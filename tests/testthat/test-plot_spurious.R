test_that("theme_tron returns a gg theme object", {
  skip_if_not_installed("ggplot2")
  th <- theme_tron()
  expect_s3_class(th, "theme")
})

test_that("plot_spurious returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  dat <- sim_linear(n = 30, seed = 1)
  p   <- plot_spurious(dat)
  expect_s3_class(p, "ggplot")
})

test_that("plot_spurious with fit = TRUE returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  dat <- sim_linear(n = 30, seed = 1)
  p   <- plot_spurious(dat, fit = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_spurious accepts optional title and subtitle", {
  skip_if_not_installed("ggplot2")
  dat <- sim_cloud(n = 30, seed = 2)
  p   <- plot_spurious(dat, title = "Test Title", subtitle = "Test Subtitle")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Test Title")
})
