library(PoSI)
source(file.path("Figures", "theme.R"))

posi_constants <- function(p, n_max, B = 1e5) {
  Sigma <- matrix(0.5, nrow = p, ncol = p)
  diag(Sigma) <- 1
  L <- chol(Sigma)
  
  X_max <- t(t(L)%*%matrix(rnorm(p*n_max), nrow = p))
  
  ns <- 10:n_max
  Ks1 <- numeric(length(ns))
  Ks2 <- numeric(length(ns))
  
  pb <- txtProgressBar(min = 0, max = length(ns), style = 3)
  
  for (idx in seq_along(ns)) {
    n <- ns[idx]
    X <- X_max[1:n, ]
    
    posi_full <- PoSI(X, Nsim = B, verbose = 0)
    Ks1[idx] <- summary(posi_full, df.err = NULL)[1, 1]
    
    posi_small <- PoSI(X, modelSZ = 1:5, Nsim = B, verbose = 0)
    Ks2[idx] <- summary(posi_small, df.err = NULL)[1, 1]
    
    setTxtProgressBar(pb, idx)
  }
  
  close(pb)
  
  data.frame(n = ns, Ks1 = Ks1, Ks2 = Ks2)
}

p <- 10
n_max <- 40

set.seed(123)
df <- posi_constants(p, n_max)

p_plot <- ggplot(df, aes(x = n)) +
  geom_point(aes(y = Ks1), shape = 16, size = 1.8) +
  geom_line(aes(y = Ks1), linewidth = 0.4) +
  geom_point(aes(y = Ks2), shape = 1, size = 1.8) +
  geom_line(aes(y = Ks2), linewidth = 0.4, linetype = "dashed") +
  labs(x = "n", y = "K") +
  theme_book

ggsave(file.path("Figures", "Outputs", "fig-2-01.pdf"), plot = p_plot, width = 4, height = 3)
