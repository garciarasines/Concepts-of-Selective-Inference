library(patchwork)
library(fastcluster)
source(file.path("Figures", "theme.R"))

norm_vec <- function(x) sqrt(sum(x^2))

set.seed(1)

n <- 30
q <- 2
sig <- 1

cl <- c(rep(1, 10), rep(2, 10), rep(3, 10))
mu <- rbind(
  c(0, 2),
  c(0, -2),
  c(sqrt(12), 0)
)

X <- matrix(rnorm(n*q, sd = sig), nrow = n, ncol = q) + mu[cl, ]

hcl <- fastcluster::hclust(dist(X)^2, method = "average")
clusters <- cutree(hcl, k = 3)

k1 <- 1
k2 <- 3

prop_k2 <- sum(clusters == k2)/(sum(clusters == k1) + sum(clusters == k2))
diff_means <- colMeans(X[clusters == k1, , drop = FALSE]) -
  colMeans(X[clusters == k2, , drop = FALSE])

stat <- norm_vec(diff_means)

perturb_data <- function(X, clusters, k1, k2, phi) {
  X_phi <- X
  
  X_phi[clusters == k1, ] <- t(
    t(X[clusters == k1, , drop = FALSE]) +
      prop_k2*(phi - stat)*diff_means/norm_vec(diff_means)
  )
  
  X_phi[clusters == k2, ] <- t(
    t(X[clusters == k2, , drop = FALSE]) +
      (prop_k2 - 1)*(phi - stat)*diff_means/norm_vec(diff_means)
  )
  
  X_phi
}

X_phi_0 <- perturb_data(
  X = X,
  clusters = clusters,
  k1 = k1,
  k2 = k2,
  phi = 0
)

X_phi_8 <- perturb_data(
  X = X,
  clusters = clusters,
  k1 = k1,
  k2 = k2,
  phi = 8
)

df_original <- data.frame(
  X1 = X[, 1],
  X2 = X[, 2],
  cluster = factor(clusters)
)

df_phi_0 <- data.frame(
  X1 = X_phi_0[, 1],
  X2 = X_phi_0[, 2],
  cluster = factor(clusters)
)

df_phi_8 <- data.frame(
  X1 = X_phi_8[, 1],
  X2 = X_phi_8[, 2],
  cluster = factor(clusters)
)

plot_panel <- function(dat, title) {
  ggplot(dat, aes(x = X1, y = X2, colour = cluster)) +
    geom_point(size = 2.4, alpha = 0.9) +
    scale_colour_grey(start = 0.25, end = 0.75) +
    coord_cartesian(xlim = c(-4.2, 7), ylim = c(-4, 4.5)) +
    labs(
      x = "Feature 1",
      y = "Feature 2",
      title = title
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
}

p_plot_a <- plot_panel(
  df_original,
  expression(paste("(a) Original data (", phi, " = 4)"))
)

p_plot_b <- plot_panel(
  df_phi_0,
  expression(paste("(b) Perturbed data (", phi, " = 0)"))
)

p_plot_c <- plot_panel(
  df_phi_8,
  expression(paste("(c) Perturbed data (", phi, " = 8)"))
)

p_plot <- p_plot_a + p_plot_b + p_plot_c + plot_layout(nrow = 1)

ggsave(file.path("Figures", "Outputs", "fig-4-11.pdf"), plot = p_plot, width = 10, height = 3.4)
