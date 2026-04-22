source(file.path("Figures", "theme.R"))

mu <- function(x) 5 + 5e-2 * x^4
x_grid <- seq(0, 4, length.out = 600)

curve_df <- data.frame(
  x = x_grid,
  y = mu(x_grid)
)

p <- ggplot(curve_df, aes(x = x, y = y)) +
  geom_line(linewidth = 1, color = "black") +
  scale_y_continuous(limits = c(0, 10)) +
  labs(x = "x", y = expression(mu(x)), title = "Figure 3.1: Ancillarity") +
  theme_book

ggsave(file.path("Figures", "Chapter-3", "ancillarity1.pdf"), plot = p, width = 5, height = 5)
