library(fastcluster)
library(patchwork)
source(file.path("Figures", "theme.R"))

cluster_wald_pval <- function(X, K = 2, sigma2 = 1) {
  q <- ncol(X)
  
  hc <- fastcluster::hclust(dist(X)^2, method = "average")
  cl <- cutree(hc, k = K)
  
  pairs <- combn(sort(unique(cl)), 2)
  pair_id <- sample(seq_len(ncol(pairs)), 1)
  c1 <- pairs[1, pair_id]
  c2 <- pairs[2, pair_id]
  
  X1 <- X[cl == c1, , drop = FALSE]
  X2 <- X[cl == c2, , drop = FALSE]
  
  n1 <- nrow(X1)
  n2 <- nrow(X2)
  
  d_hat <- colMeans(X1) - colMeans(X2)
  stat <- sum(d_hat^2) / (sigma2 * (1/n1 + 1/n2))
  
  pchisq(stat, df = q, lower.tail = FALSE)
}

simulate_wald_pvals <- function(B = 2000, n = 100, q = 2, sigma = 1, K = 2) {
  pvals <- numeric(B)
  
  pb <- txtProgressBar(min = 0, max = B, style = 3)
  
  for (b in seq_len(B)) {
    X <- matrix(rnorm(n * q, sd = sigma), nrow = n, ncol = q)
    pvals[b] <- cluster_wald_pval(
      X,
      K = K,
      sigma2 = sigma^2
    )
    setTxtProgressBar(pb, b)
  }
  
  close(pb)
  
  pvals
}

set.seed(1)

n <- 100
q <- 2
sig <- 1
K <- 2

X <- data.frame(
  matrix(
    rnorm(n * q, sd = sig),
    nrow = n,
    ncol = q
  )
)

colnames(X) <- c("Feat1", "Feat2")

X$clusters <- as.factor(
  cutree(
    fastcluster::hclust(
      dist(X)^2,
      method = "average"
    ),
    k = K
  )
)

centroids <- aggregate(
  cbind(Feat1, Feat2) ~ clusters,
  X,
  mean
)

p_plot_a <- ggplot() +
  geom_point(
    aes(
      x = Feat1,
      y = Feat2,
      shape = clusters,
      fill = clusters
    ),
    colour = "black",
    alpha = 0.7,
    size = 2.4,
    stroke = 0.4,
    data = X
  ) +
  scale_shape_manual(
    values = c(21, 22)
  ) +
  scale_fill_manual(
    values = c("white", "grey55")
  ) +
  geom_point(
    aes(
      x = Feat1,
      y = Feat2
    ),
    shape = 4,
    colour = "black",
    size = 5,
    stroke = 1.2,
    data = centroids
  ) +
  xlab("Feature 1") +
  ylab("Feature 2") +
  ggtitle("(a) Data") +
  coord_fixed(
    xlim = c(-3, 3),
    ylim = c(-3, 3)
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
    axis.text = element_text(size = 10)
  )

set.seed(123)

pvals <- simulate_wald_pvals(
  B = 2000,
  n = n,
  q = q,
  sigma = sig,
  K = K
)

pval_df <- data.frame(
  pval = pvals
)

p_plot_b <- ggplot(
  pval_df,
  aes(x = pval)
) +
  stat_ecdf(
    geom = "step",
    linewidth = 0.8,
    colour = "black"
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linewidth = 0.5,
    linetype = "dashed",
    colour = "grey40"
  ) +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1)
  ) +
  labs(
    x = "p-value",
    y = "Empirical CDF",
    title = "(b) Wald test"
  ) +
  theme_book +
  theme(
    plot.title = element_text(hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

p_plot <- p_plot_a + p_plot_b

ggsave(file.path("Figures", "Outputs", "fig-4-07.pdf"), plot = p_plot, width = 6, height = 4)
