source(file.path("Figures", "theme.R"))

n <- 5000
degree <- 5
n_bins <- 80

set.seed(123)

theta <- rexp(n, rate = 1)
y <- rnorm(n, mean = theta, sd = 1)

breaks <- seq(min(y), max(y), length.out = n_bins + 1)
hist_y <- hist(y, breaks = breaks, plot = FALSE)

bin_centers <- hist_y$mids
counts <- hist_y$counts
bin_width <- diff(breaks)[1]

df <- data.frame(
  y = bin_centers,
  count = counts,
  positive = counts > 0
)

fit <- glm(
  count ~ poly(y, degree = degree, raw = TRUE),
  family = poisson,
  data = df
)

y_grid <- seq(min(bin_centers), max(bin_centers), length.out = 1e3)

df_fit <- data.frame(
  y = y_grid,
  log_count = predict(fit, newdata = data.frame(y = y_grid), type = "link")
)

df$log_count <- ifelse(df$count > 0, log(df$count), NA)
zero_level <- min(log(df$count[df$count > 0])) - 0.5

p_plot <- ggplot() +
  geom_point(
    data = subset(df, positive),
    aes(x = y, y = log_count),
    shape = 16,
    size = 1.8,
    color = "black"
  ) +
  geom_point(
    data = subset(df, !positive),
    aes(x = y, y = zero_level),
    shape = 1,
    size = 1.8,
    color = "black"
  ) +
  geom_line(
    data = df_fit,
    aes(x = y, y = log_count),
    linewidth = 1,
    color = "grey50"
  ) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(x = "y", y = "log count") +
  theme_book

ggsave(file.path("Figures", "Outputs", "fig-6-08.pdf"), plot = p_plot, width = 4, height = 3)
