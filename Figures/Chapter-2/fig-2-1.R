library(PoSI)
source(file.path("Figures", "theme.R"))

B <- 5e4
p <- 10
n_max <- 40

Sigma <- matrix(0.5, nrow = p, ncol = p)
diag(Sigma) <- 1
L <- chol(Sigma)

set.seed(123)
X_max <- t(t(L) %*% matrix(rnorm(p * n_max), nrow = p))

n_grid <- 10:n_max
Ks1 <- numeric(length(n_grid))
Ks2 <- numeric(length(n_grid))

for (idx in seq_along(n_grid)) {
  n <- n_grid[idx]
  X <- X_max[1:n, ]

  posi_full <- PoSI(X, Nsim = B, verbose = 0)
  Ks1[idx] <- summary(posi_full, df.err = NULL)[1, 1]

  posi_small <- PoSI(X, modelSZ = 1:5, Nsim = B, verbose = 0)
  Ks2[idx] <- summary(posi_small, df.err = NULL)[1, 1]
}

df <- data.frame(
  n = n_grid,
  Ks1 = Ks1,
  Ks2 = Ks2
)

p_plot <- ggplot(df, aes(x = n)) +
  geom_point(aes(y = Ks1), shape = 16, size = 1.8) +
  geom_line(aes(y = Ks1), linewidth = 0.4) +
  geom_point(aes(y = Ks2), shape = 1, size = 1.8) +
  geom_line(aes(y = Ks2), linewidth = 0.4, linetype = "dashed") +
  labs(x = "n", y = "K", title = "fig-2-1") +
  theme_book

ggsave(file.path("Figures", "Chapter-2", "fig-2-1.pdf"), plot = p_plot, width = 6, height = 4)
