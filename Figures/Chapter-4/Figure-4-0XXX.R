library(ggplot2)
library(patchwork)
library(fastcluster)
source(file.path("Figures", "theme.R"))

norm_vec <- function(x) sqrt(sum(x^2))

set.seed(1)

n <- 60
q <- 2
sig <- 0.7
K <- 3

cl_true <- rep(1:3, each = 20)

mu <- rbind(
  c(-1.2, 2.1),
  c(-1.2, -2.1),
  c(3.2, 0)
)

Y <- matrix(rnorm(n * q, sd = sig), nrow = n, ncol = q) + mu[cl_true, ]

hcl <- fastcluster::hclust(dist(Y)^2, method = "average")
clusters <- cutree(hcl, k = K)

cluster_levels <- sort(unique(clusters))
cluster_sizes <- sapply(cluster_levels, function(k) sum(clusters == k))
cluster_means <- t(sapply(cluster_levels, function(k) colMeans(Y[clusters == k, , drop = FALSE])))

pair_indices <- combn(seq_along(cluster_levels), 2)

pair_balance <- apply(pair_indices, 2, function(j) {
  abs(cluster_sizes[j[1]] - cluster_sizes[j[2]])
})

candidate_pairs <- which(pair_balance == min(pair_balance))

pair_dist <- apply(pair_indices[, candidate_pairs, drop = FALSE], 2, function(j) {
  norm_vec(cluster_means[j[1], ] - cluster_means[j[2], ])
})

chosen_pair <- pair_indices[, candidate_pairs[which.max(pair_dist)]]

k1 <- cluster_levels[chosen_pair[1]]
k2 <- cluster_levels[chosen_pair[2]]

plot_cluster <- rep("C3", n)
plot_cluster[clusters == k1] <- "C1"
plot_cluster[clusters == k2] <- "C2"
plot_cluster <- factor(plot_cluster, levels = c("C1", "C2", "C3"))

eta <- numeric(n)
eta[plot_cluster == "C1"] <- 1 / sum(plot_cluster == "C1")
eta[plot_cluster == "C2"] <- -1 / sum(plot_cluster == "C2")

diff_means <- colMeans(Y[plot_cluster == "C1", , drop = FALSE]) -
  colMeans(Y[plot_cluster == "C2", , drop = FALSE])

r_obs <- norm_vec(diff_means)
d <- diff_means / r_obs
eta_sq <- sum(eta^2)

perturb_data <- function(Y, r) {
  Y + ((r - r_obs) / eta_sq) * tcrossprod(eta, d)
}

r_a <- r_obs
r_b <- 0
r_c <- 2 * r_obs

Y_a <- perturb_data(Y, r_a)
Y_b <- perturb_data(Y, r_b)
Y_c <- perturb_data(Y, r_c)

df_a <- data.frame(
  Y1 = Y_a[, 1],
  Y2 = Y_a[, 2],
  cluster = plot_cluster
)

df_b <- data.frame(
  Y1 = Y_b[, 1],
  Y2 = Y_b[, 2],
  cluster = plot_cluster
)

df_c <- data.frame(
  Y1 = Y_c[, 1],
  Y2 = Y_c[, 2],
  cluster = plot_cluster
)

x_lim <- range(c(df_a$Y1, df_b$Y1, df_c$Y1)) + c(-0.3, 0.3)
y_lim <- range(c(df_a$Y2, df_b$Y2, df_c$Y2)) + c(-0.3, 0.3)

plot_panel <- function(dat, title) {
  ggplot(dat, aes(x = Y1, y = Y2)) +
    geom_point(
      aes(shape = cluster, fill = cluster),
      size = 2.4,
      alpha = 0.7,
      colour = "black",
      stroke = 0.4
    ) +
    scale_shape_manual(
      values = c("C1" = 21, "C2" = 22, "C3" = 24)
    ) +
    scale_fill_manual(
      values = c("C1" = "white", "C2" = "grey55", "C3" = "grey85")
    ) +
    coord_cartesian(
      xlim = x_lim,
      ylim = y_lim
    ) +
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
      panel.background = element_rect(
        fill = "white",
        colour = "black",
        linewidth = 0.6
      ),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      plot.margin = margin(2, 2, 2, 2)
    )
}

p_plot_a <- plot_panel(df_a, bquote("(a) " * r[0] == .(round(r_a, 2))))
p_plot_b <- plot_panel(df_b, bquote("(b) " * r == 0))
p_plot_c <- plot_panel(df_c, bquote("(c) " * r == 2 * r[0]))

p_plot <- p_plot_a + p_plot_b + p_plot_c + plot_layout(nrow = 1)

ggsave(file.path("Figures", "Outputs", "fig-4-08.pdf"), plot = p_plot, width = 7.5, height = 3.2)
