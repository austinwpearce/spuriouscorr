# spuriouscorr demo
# Run interactively or via demo("spuriouscorr_demo", package = "spuriouscorr")
#
# Walks through every public function in the package:
#   sim_linear / sim_cloud / sim_ushape / sim_star  – data simulation
#   fit_linear                                       – linear model fitting
#   plot_spurious / theme_tron                       – Tron-themed ggplot2

library(spuriouscorr)

# ── 1. Simulate data ──────────────────────────────────────────────────────────

dat_pos   <- sim_linear(n = 120, slope =  1.5, noise = 1,   seed = 1)
dat_neg   <- sim_linear(n = 120, slope = -1.5, noise = 1,   seed = 2)
dat_cloud <- sim_cloud( n = 150,                             seed = 3)
dat_u_up  <- sim_ushape(n = 120, direction = "up",   noise = 0.5, seed = 4)
dat_u_dn  <- sim_ushape(n = 120, direction = "down", noise = 0.5, seed = 5)
dat_star  <- sim_star(  n = 120, arms = 6, noise = 0.15,    seed = 6)

# ── 2. Fit linear models ──────────────────────────────────────────────────────

fit_pos   <- fit_linear(dat_pos)
fit_neg   <- fit_linear(dat_neg)
fit_cloud <- fit_linear(dat_cloud)
fit_u_up  <- fit_linear(dat_u_up)
fit_u_dn  <- fit_linear(dat_u_dn)
fit_star  <- fit_linear(dat_star)

# Each result has $model (lm object) and $fitted (tibble of predictions)
cat("\n── Positive linear trend ──\n")
print(summary(fit_pos$model))

cat("\n── Negative linear trend ──\n")
print(summary(fit_neg$model))

cat("\n── Cloud / potato (spurious: low R²) ──\n")
print(summary(fit_cloud$model))

cat("\n── U-shaped (upward opening) ──\n")
print(summary(fit_u_up$model))

cat("\n── U-shaped (downward / inverted) ──\n")
print(summary(fit_u_dn$model))

cat("\n── Star shape ──\n")
print(summary(fit_star$model))

# ── 3. Individual scatter plots ───────────────────────────────────────────────

plot_spurious(dat_pos,   fit = TRUE,  title = "Positive Linear Trend")
plot_spurious(dat_neg,   fit = TRUE,  title = "Negative Linear Trend")
plot_spurious(dat_cloud, fit = TRUE,  title = "Cloud / Potato",
              subtitle = "Linear fit on uncorrelated data — classic spurious correlation")
plot_spurious(dat_u_up,  fit = TRUE,  title = "U-Shaped (upward)",
              subtitle = "Linear fit misses the quadratic pattern")
plot_spurious(dat_u_dn,  fit = TRUE,  title = "Inverted U-Shaped (downward)")
plot_spurious(dat_star,  fit = FALSE, title = "Star Shape")

# ── 4. Composite: all six patterns in one patchwork grid ─────────────────────
# Requires the 'patchwork' package (install.packages("patchwork")).

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)

  p1 <- plot_spurious(dat_pos,   fit = TRUE,  title = "Positive Linear")
  p2 <- plot_spurious(dat_neg,   fit = TRUE,  title = "Negative Linear")
  p3 <- plot_spurious(dat_cloud, fit = TRUE,  title = "Cloud / Potato")
  p4 <- plot_spurious(dat_u_up,  fit = TRUE,  title = "U-Shaped (up)")
  p5 <- plot_spurious(dat_u_dn,  fit = TRUE,  title = "Inverted U")
  p6 <- plot_spurious(dat_star,  fit = FALSE, title = "Star Shape")

  grid <- (p1 | p2 | p3) / (p4 | p5 | p6) +
    patchwork::plot_annotation(
      title   = "spuriouscorr — simulated pattern gallery",
      theme   = theme_tron()
    )

  print(grid)
} else {
  message("Install 'patchwork' to view all patterns in a single grid: ",
          "install.packages('patchwork')")
}
