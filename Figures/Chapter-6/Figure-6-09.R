library(splines)
source(file.path("Figures", "theme.R"))

N <- 5000
m <- 100
J <- 5
d <- 0.2

set.seed(123)

mu_base <- -log(((1:500) - 0.5)/500)
mu <- rep(mu_base, each = 10)

z <- rnorm(N, mean = mu, sd = 1)

selected <- rank(-z, ties.method = "first") <= m

breaks <- seq(-3.5, 9.1, by = d)
hist_z <- hist(z, breaks = breaks, plot = FALSE)

x <- hist_z$mids
counts <- hist_z$counts

df_bins <- data.frame(
  x = x,
  counts = counts
)

fit <- glm(
  counts ~ ns(x, df = J),
  family = poisson,
  offset = rep(log(N*d), length(counts)),
  data = df_bins
)

ell_hat <- function(z) {
  predict(fit, newdata = data.frame(x = z), type = "link")
}

ell_prime_hat <- function(z) {
  eps <- 1e-5
  (ell_hat(z + eps) - ell_hat(z - eps))/(2*eps)
}

mu_hat <- function(z) {
  z + ell_prime_hat(z)
}

h1 <- function(x) {
  exp(dnorm(x, log = TRUE) - pnorm(x, log.p = TRUE))
}

mu_bayes <- function(z) {
  z - 1 + h1(z - 1)
}

z_grid <- seq(-3.5, 9.2, length.out = 1000)

df_curve <- data.frame(
  z = z_grid,
  mu_hat = mu_hat(z_grid),
  mu_bayes = mu_bayes(z_grid)
)

df_selected <- data.frame(
  z = z[selected],
  mu_hat = mu_hat(z[selected])
)

p_plot <- ggplot() +
  geom_abline(
    slope = 1,
    intercept = 0,
    linewidth = 0.4,
    linetype = "dotted",
    color = "grey55"
  ) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_point(
    data = df_curve,
    aes(x = z, y = mu_bayes),
    shape = 16,
    size = 0.4,
    color = "grey55"
  ) +
  geom_line(
    data = df_curve,
    aes(x = z, y = mu_hat),
    linewidth = 1,
    color = "black"
  ) +
  geom_point(
    data = df_selected,
    aes(x = z, y = mu_hat),
    shape = 16,
    size = 2,
    color = "black"
  ) +
  geom_segment(
    data = df_selected,
    aes(x = z, xend = z, y = -0.1, yend = 0.1),
    linewidth = 0.3,
    color = "black"
  ) +
  annotate("point", x = 0, y = 0, shape = 16, size = 1.4, color = "black") +
  annotate("text", x = 6.25, y = -0.35, label = "100 largest z_i's", size = 4) +
  coord_cartesian(xlim = c(-4, 10), ylim = c(-1, 8.5)) +
  labs(x = "z value", y = expression(hat(mu))) +
  theme_book

ggsave(file.path("Figures", "Outputs", "fig-6-09.pdf"), plot = p_plot, width = 4, height = 3)
