source(file.path("Figures", "theme.R"))

n <- 5000
n_selected <- 100

set.seed(123)

theta <- rexp(n, rate = 1)
y <- rnorm(n, mean = theta, sd = 1)

selected <- rank(-y, ties.method = "first") <= n_selected

df <- data.frame(
  y = y,
  selected = selected
)

p_plot <- ggplot(df, aes(x = y)) +
  geom_histogram(
    data = subset(df, !selected),
    bins = 70,
    fill = "grey80",
    color = "black",
    linewidth = 0.2
  ) +
  geom_histogram(
    data = subset(df, selected),
    bins = 70,
    fill = "grey35",
    color = "black",
    linewidth = 0.2
  ) +
  labs(x = "z values", y = "Frequency") +
  theme_book

ggsave(file.path("Figures", "Outputs", "fig-6-07.pdf"), plot = p_plot, width = 4, height = 3)
