source(file.path("Figures", "theme.R"))

B <- 1e6

set.seed(123)
K <- quantile(replicate(1e5, max(abs(rnorm(2)))), 0.95)
e1s <- rnorm(B)
e2s <- rnorm(B)

coverages <- function(mu2) {
  mu <- c(0, mu2)
  I <- numeric(B)
  cov <- numeric(B)
  
  for (b in seq_len(B)) {
    y1 <- mu[1] + e1s[b]
    y2 <- mu[2] + e2s[b]
    y <- c(y1, y2)
    
    I[b] <- which(y == max(y))
    yI <- y[I[b]]
    cov[b] <- abs(yI - mu[I[b]]) < K
  }
  
  c(mean(cov), mean(cov[I == 1]), mean(cov[I == 2]))
}

mu2s <- seq(0, 3, length.out = 50)
res <- matrix(NA, nrow = 3, ncol = length(mu2s))

pb <- txtProgressBar(min = 0, max = length(mu2s), style = 3)

for (idx in seq_along(mu2s)) {
  res[, idx] <- coverages(mu2s[idx])
  setTxtProgressBar(pb, idx)
}

close(pb)

df <- data.frame(mu2 = mu2s, cov1 = res[2, ], cov2 = res[3, ]
)

p_plot <- ggplot(df, aes(x = mu2)) +
  geom_line(aes(y = cov1), linewidth = 0.4) +
  geom_point(aes(y = cov1), shape = 16, size = 1.8) +
  geom_line(aes(y = cov2), linewidth = 0.4, linetype = "dashed") +
  geom_point(aes(y = cov2), shape = 1, size = 1.8) +
  geom_hline(yintercept = res[1, 1], linewidth = 0.4) +
  geom_hline(yintercept = 0.95, linewidth = 0.4, linetype = "dashed") +
  scale_y_continuous(limits = c(min(res), 1)) +
  labs(x = expression(mu[2]), y = "Coverage") +
  theme_book

ggsave(file.path("Figures", "Chapter-4", "fig-4-1.pdf"), plot = p_plot, width = 7, height = 5)
