library(MASS)
source(file.path("Figures", "theme.R"))

B <- 1e4
n <- 80
p <- 20

p_vals <- c()

set.seed(123)
for (i in seq_len(B)) {
  X <- data.frame(matrix(rnorm(n * p), ncol = p))
  y <- rnorm(n)

  min_model <- lm(y ~ 1, data = X)
  max_model <- formula(lm(y ~ ., data = X))

  fit_step <- stepAIC(
    min_model,
    direction = "forward",
    scope = max_model,
    trace = FALSE
  )

  p_vals <- c(p_vals, summary(fit_step)$coefficients[, 4])
}

df <- data.frame(p_value = p_vals)

p_plot <- ggplot(df, aes(x = p_value)) +
  stat_ecdf(geom = "step", linewidth = 0.4, color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = "p-value",
    y = "Empirical CDF",
    title = "fig-1-2"
  ) +
  theme_book

ggsave(file.path("Figures", "Chapter-1", "fig-1-2.pdf"), plot = p_plot, width = 5, height = 5)
