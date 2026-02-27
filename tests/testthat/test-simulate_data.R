test_that("sim_linear returns a tibble with x and y columns", {
  dat <- sim_linear(n = 50, seed = 1)
  expect_s3_class(dat, "tbl_df")
  expect_named(dat, c("x", "y"))
  expect_equal(nrow(dat), 50)
})

test_that("sim_linear positive slope produces positive correlation", {
  dat <- sim_linear(n = 200, slope = 3, noise = 0.1, seed = 42)
  expect_gt(cor(dat$x, dat$y), 0.9)
})

test_that("sim_linear negative slope produces negative correlation", {
  dat <- sim_linear(n = 200, slope = -3, noise = 0.1, seed = 42)
  expect_lt(cor(dat$x, dat$y), -0.9)
})

test_that("sim_cloud returns a tibble with x and y columns", {
  dat <- sim_cloud(n = 80, seed = 5)
  expect_s3_class(dat, "tbl_df")
  expect_named(dat, c("x", "y"))
  expect_equal(nrow(dat), 80)
})

test_that("sim_cloud produces low correlation", {
  dat <- sim_cloud(n = 1000, seed = 99)
  expect_lt(abs(cor(dat$x, dat$y)), 0.15)
})

test_that("sim_ushape returns a tibble with x and y columns", {
  dat <- sim_ushape(n = 60, seed = 2)
  expect_s3_class(dat, "tbl_df")
  expect_named(dat, c("x", "y"))
  expect_equal(nrow(dat), 60)
})

test_that("sim_ushape direction 'up' gives positive second derivative character", {
  dat <- sim_ushape(n = 500, direction = "up", noise = 0.01, seed = 10)
  # For U-up, y at extreme x values should be higher than at center
  mid   <- dat[abs(dat$x) < 0.5, ]
  outer <- dat[abs(dat$x) > 4,   ]
  expect_gt(mean(outer$y), mean(mid$y))
})

test_that("sim_ushape direction 'down' inverts the shape", {
  dat <- sim_ushape(n = 500, direction = "down", noise = 0.01, seed = 11)
  mid   <- dat[abs(dat$x) < 0.5, ]
  outer <- dat[abs(dat$x) > 4,   ]
  expect_lt(mean(outer$y), mean(mid$y))
})

test_that("sim_star returns a tibble with x and y columns", {
  dat <- sim_star(n = 60, arms = 3, seed = 3)
  expect_s3_class(dat, "tbl_df")
  expect_named(dat, c("x", "y"))
  expect_gt(nrow(dat), 0)
})

test_that("seed argument produces reproducible results", {
  d1 <- sim_linear(n = 10, seed = 7)
  d2 <- sim_linear(n = 10, seed = 7)
  expect_identical(d1, d2)
})
