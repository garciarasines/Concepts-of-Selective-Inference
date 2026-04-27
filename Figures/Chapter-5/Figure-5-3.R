source(file.path("Figures", "theme.R"))

set.seed(123)

n <- 20
sigma <- sqrt(0.1)

x <- sort(runif(n, min = 1.5, max = 4.5))
eps <- rnorm(n, sd = sigma)
y <- x + eps

split <- sample(rep(c("A", "B"), each = n/2))

w <- rnorm(n, sd = sigma)
u <- y + w
v <- y - w

ylim_plot <- range(c(y, u, v)) + c(-0.2, 0.2)

df_split <- data.frame(x = x, y = y, split = split)
df_rand <- data.frame(x = x, y = y, u = u, v = v)

p_plot_a <- ggplot(df_split, aes(x = x, y = y)) +
  geom_point(aes(shape = split), size = 2.4, color = "black") +
  scale_shape_manual(values = c(16, 1)) +
  coord_cartesian(xlim = c(1.4, 4.6), ylim = ylim_plot) +
  labs(x = "x", y = "y", title = "Data splitting") +
  theme_book +
  theme(legend.position = "none")

p_plot_b <- ggplot(df_rand, aes(x = x)) +
  geom_segment(aes(xend = x, y = v, yend = u), linewidth = 0.8, color = "black") +
  geom_point(aes(y = y), shape = 16, size = 2.2, color = "grey50") +
  geom_point(aes(y = u), shape = 16, size = 2.2, color = "black") +
  geom_point(aes(y = v), shape = 1, size = 2.2, color = "black") +
  coord_cartesian(xlim = c(1.4, 4.6), ylim = ylim_plot) +
  labs(x = "x", y = "y", title = "Randomisation") +
  theme_book

ggsave(file.path("Figures", "Chapter-5", "fig-5-3-a.pdf"), plot = p_plot_a, width = 5, height = 4)
ggsave(file.path("Figures", "Chapter-5", "fig-5-3-b.pdf"), plot = p_plot_b, width = 5, height = 4)
