library(patchwork)
library(class)
library(fastcluster)
source(file.path("Figures", "theme.R"))

set.seed(123)

n <- 100
q <- 2
sig <- 1
K <- 2

X <- matrix(rnorm(n*q, sd = sig), nrow = n, ncol = q)

dat <- data.frame(
  X1 = X[, 1],
  X2 = X[, 2],
  split = factor(c(rep("Training", 50), rep("Test", 50)))
)

dat_train <- dat[1:50, ]
dat_test <- dat[51:100, ]

hc <- fastcluster::hclust(dist(X[1:50, ])^2, method = "average")
dat_train$cluster <- factor(cutree(hc, k = K))

dat_test$cluster <- class::knn(
  train = X[1:50, ],
  test = X[51:100, ],
  cl = dat_train$cluster,
  k = 3
)

p_plot_a <- ggplot(dat, aes(x = X1, y = X2)) +
  geom_point(
    aes(shape = split),
    size = 2.5,
    alpha = 0.9,
    colour = "black"
  ) +
  scale_shape_manual(
    values = c("Training" = 15, "Test" = 17)
  ) +
  coord_cartesian(xlim = c(-2.8, 2.8), ylim = c(-2.3, 3.3)) +
  labs(
    x = "Feature 1",
    y = "Feature 2",
    title = "(a) Data"
  ) +
  theme_book +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

p_plot_b <- ggplot(dat_train, aes(x = X1, y = X2)) +
  geom_point(
    aes(colour = cluster),
    size = 2.5,
    alpha = 0.9,
    shape = 15
  ) +
  scale_colour_grey(start = 0.25, end = 0.75) +
  coord_cartesian(xlim = c(-2.8, 2.8), ylim = c(-2.3, 3.3)) +
  labs(
    x = "Feature 1",
    y = "Feature 2",
    title = "(b) Training set"
  ) +
  theme_book +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

p_plot_c <- ggplot(dat_test, aes(x = X1, y = X2)) +
  geom_point(
    aes(colour = cluster),
    size = 2.5,
    alpha = 0.9,
    shape = 17
  ) +
  scale_colour_grey(start = 0.25, end = 0.75) +
  coord_cartesian(xlim = c(-2.8, 2.8), ylim = c(-2.3, 3.3)) +
  labs(
    x = "Feature 1",
    y = "Feature 2",
    title = "(c) Test set"
  ) +
  theme_book +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

p_plot <- p_plot_a + p_plot_b + p_plot_c + plot_layout(nrow = 1)

ggsave(file.path("Figures", "Outputs", "fig-4-10.pdf"), plot = p_plot, width = 9, height = 3.2)
