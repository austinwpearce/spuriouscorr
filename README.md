# spuriouscorr

An R package for simulating and visualizing spurious correlations with a dark
Tron-like aesthetic.

## Installation

```r
# install.packages("devtools")
devtools::install_github("austinwpearce/spuriouscorr")
```

## Usage

```r
library(spuriouscorr)

# Simulate different data patterns
dat_linear  <- sim_linear(n = 100, slope = 1.5, seed = 1)
dat_neg     <- sim_linear(n = 100, slope = -1,  seed = 2)
dat_cloud   <- sim_cloud(n = 150, seed = 3)
dat_u_up    <- sim_ushape(n = 100, direction = "up",   seed = 4)
dat_u_down  <- sim_ushape(n = 100, direction = "down", seed = 5)
dat_star    <- sim_star(n = 120, arms = 6, seed = 6)

# Plot with the Tron-like dark theme
plot_spurious(dat_linear, fit = TRUE,  title = "Linear Trend (positive)")
plot_spurious(dat_cloud,               title = "Cloud / Potato")
plot_spurious(dat_u_up,                title = "U-Shaped (up)")
plot_spurious(dat_star,                title = "Star Shape")

# Fit a linear model and inspect it
result <- fit_linear(dat_linear)
summary(result$model)
head(result$fitted)
```

## Functions

| Function | Description |
|---|---|
| `sim_linear()` | Simulate data with a positive or negative linear trend |
| `sim_cloud()` | Simulate a low-correlation cloud / potato shape |
| `sim_ushape()` | Simulate a U-shaped or inverted U-shaped pattern |
| `sim_star()` | Simulate a star-shaped pattern |
| `fit_linear()` | Fit a linear regression model (`y ~ x`) |
| `plot_spurious()` | Scatter plot with optional linear fit and Tron dark theme |
| `theme_tron()` | Reusable dark Tron-like ggplot2 theme |