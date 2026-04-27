library(glmnet)
source(file.path("Figures", "theme.R"))

model_probability <- function(p, n, B, sigma) {
  Sigma <- matrix(NA, nrow = p, ncol = p)
  Sigma[upper.tri(Sigma, diag = FALSE)] <- 0.5
  Sigma[lower.tri(Sigma, diag = FALSE)] <- 0.5
  diag(Sigma) <- rep(1, p)
  L <- chol(Sigma)
  
  X <- t(t(L)%*%matrix(rnorm(p*n), nrow = p))
  beta <- matrix(c(2, 2, 2, 2, 2, rep(0, p - 5)), ncol = 1)
  
  v <- numeric(B)
  
  for (b in seq_len(B)) {
    y <- X%*%beta + rnorm(n, sd = sigma)
    lasso <- cv.glmnet(X, y)
    s <- coef(lasso, s = "lambda.min")[-1] != 0
    v[b] <- sum(s*2^(0:(p - 1)))
  }
  
  max(table(v))/B
}

B <- 1e4
n <- 80
sigma <- 1
p_grid <- seq(10, 30, length.out = 5)

set.seed(123)

res <- numeric(length(p_grid))

pb <- txtProgressBar(min = 0, max = length(p_grid), style = 3)

for (i in seq_along(p_grid)) {
  res[i] <- model_probability(p_grid[i], n = n, B = B, sigma = sigma)
  setTxtProgressBar(pb, i)
}

close(pb)

p_plot <- ggplot(data.frame(p = p_grid, probability = res), aes(x = p, y = log(probability))) +
  geom_line(linewidth = 1) +
  geom_point(shape = 16, size = 1.8) +
  labs(x = "p", y = "log probability") +
  theme_book

ggsave(file.path("Figures", "Chapter-5", "fig-5-2.pdf"), plot = p_plot, width = 5, height = 5)
