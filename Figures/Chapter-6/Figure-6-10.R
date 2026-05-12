library(splines)
source(file.path("Figures", "theme.R"))

B <- 100
N <- 1000
m <- 20
J <- 5
d <- 0.2

set.seed(123)

diff_small_un <- numeric(B*m)
diff_small_eb <- numeric(B*m)
diff_large_un <- numeric(B*m)
diff_large_eb <- numeric(B*m)

pb <- txtProgressBar(min = 0, max = B, style = 3)

for (b in seq_len(B)) {
  mu <- rexp(N, rate = 1)
  z <- rnorm(N, mean = mu, sd = 1)
  
  breaks <- seq(
    floor(min(z)/d)*d,
    ceiling(max(z)/d)*d,
    by = d
  )
  
  hist_z <- hist(z, breaks = breaks, plot = FALSE)
  
  x <- hist_z$mids
  counts <- hist_z$counts
  
  df_bins <- data.frame(x = x, counts = counts)
  
  fit <- glm(
    counts ~ ns(x, df = J),
    family = poisson,
    offset = rep(log(N*d), length(counts)),
    data = df_bins
  )
  
  ell_hat <- function(z0) {
    predict(fit, newdata = data.frame(x = z0), type = "link")
  }
  
  ell_prime_hat <- function(z0) {
    eps <- 1e-5
    (ell_hat(z0 + eps) - ell_hat(z0 - eps))/(2*eps)
  }
  
  mu_hat <- z + ell_prime_hat(z)
  
  ind_small <- order(z)[seq_len(m)]
  ind_large <- order(z, decreasing = TRUE)[seq_len(m)]
  
  idx <- ((b - 1)*m + 1):(b*m)
  
  diff_small_un[idx] <- z[ind_small] - mu[ind_small]
  diff_small_eb[idx] <- mu_hat[ind_small] - mu[ind_small]
  diff_large_un[idx] <- z[ind_large] - mu[ind_large]
  diff_large_eb[idx] <- mu_hat[ind_large] - mu[ind_large]
  
  setTxtProgressBar(pb, b)
}

close(pb)

df_hist <- data.frame(
  difference = c(diff_small_eb, diff_small_un, diff_large_eb, diff_large_un),
  method = factor(
    rep(c("EB", "Uncorrected", "EB", "Uncorrected"), each = B*m),
    levels = c("EB", "Uncorrected")
  ),
  group = factor(
    rep(c("Smallest", "Smallest", "Largest", "Largest"), each = B*m),
    levels = c("Smallest", "Largest")
  )
)

p_plot <- ggplot() +
  geom_histogram(
    data = subset(df_hist, method == "EB"),
    aes(x = difference),
    binwidth = 0.15,
    fill = "grey75",
    color = "black",
    linewidth = 0.2
  ) +
  geom_histogram(
    data = subset(df_hist, method == "Uncorrected"),
    aes(x = difference),
    binwidth = 0.15,
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  facet_wrap(~ group, nrow = 1, scales = "free") +
  labs(x = "Difference", y = "Counts") +
  theme_book

ggsave(file.path("Figures", "Outputs", "fig-6-10.pdf"), plot = p_plot, width = 4, height = 3)
