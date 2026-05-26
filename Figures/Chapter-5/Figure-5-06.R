library(mvtnorm)
library(glmnet)
library(knockoff)
source(file.path("Figures", "theme.R"))

options(warn = -1)

design <- function(n, p, rho) {
  Sigma <- outer(1:p, 1:p, function(i, j) rho^abs(i - j))
  rmvnorm(n, mean = rep(0, p), sigma = Sigma)
}

y_sample <- function(X, theta, sigma) {
  n <- nrow(X)
  X %*% theta + sigma*rnorm(n)
}

Selection <- function(y, X) {
  p <- ncol(X)
  sel <- knockoff.filter(X, y, knockoffs = create.fixed, fdr = 0.3)$selected
  out <- rep(FALSE, p)
  out[sel] <- TRUE
  out
}

lasso <- function(y, X, max_size = 15) {
  fit <- cv.glmnet(X, y, intercept = FALSE)
  coef_vec <- as.vector(coef(fit, s = "lambda.min"))
  S <- which(coef_vec[-1] != 0)
  if (length(S) > max_size) {
    beta_abs <- abs(coef_vec[-1])
    S <- order(beta_abs, decreasing = TRUE)[seq_len(max_size)]
  }
  S
}

sigma_estimator <- function(y, X, gamma = 0.6) {
  n <- length(y)
  n2 <- floor(gamma*n)
  if (n2 %% 2 != 0) n2 <- n2 + 1
  
  y_new <- y[1:n2]
  X_new <- X[1:n2, ]
  
  y1 <- y_new[1:(n2/2)]
  y2 <- y_new[-(1:(n2/2))]
  X1 <- X_new[1:(n2/2), ]
  X2 <- X_new[-(1:(n2/2)), ]
  
  S <- lasso(y2, X2)
  df1 <- n2/2 - length(S)
  if (length(S) == 0) {
    sigma2_hat_1 <- sum(y1^2)/(n2/2)
  } else {
    X1_S <- X1[, S, drop = FALSE]
    M1 <- diag(1, n2/2) - X1_S %*% solve(t(X1_S) %*% X1_S) %*% t(X1_S)
    sigma2_hat_1 <- sum((M1 %*% y1)^2)/df1
  }
  
  S <- lasso(y1, X1)
  df2 <- n2/2 - length(S)
  if (length(S) == 0) {
    sigma2_hat_2 <- sum(y2^2)/(n2/2)
  } else {
    X2_S <- X2[, S, drop = FALSE]
    M2 <- diag(1, n2/2) - X2_S %*% solve(t(X2_S) %*% X2_S) %*% t(X2_S)
    sigma2_hat_2 <- sum((M2 %*% y2)^2)/df2
  }
  
  sqrt((df1*sigma2_hat_1 + df2*sigma2_hat_2)/(df1 + df2))
}

summarize_selection <- function(mat, beta_abs, method_name) {
  data.frame(
    effect = beta_abs,
    mean = colMeans(mat),
    lower = apply(mat, 2, quantile, probs = 0.1),
    upper = apply(mat, 2, quantile, probs = 0.9),
    method = method_name
  )
}

run_knockoff_stability <- function(n, p, rho, B, R, f, gamma_sigma = 0.6) {
  beta0 <- c(seq(1, 0.1, by = -0.1), rep(0, p - 10))
  active <- seq_len(10)
  
  split_mat <- matrix(0, nrow = B, ncol = length(active))
  rand_mat <- matrix(0, nrow = B, ncol = length(active))
  
  pb <- txtProgressBar(min = 0, max = B, style = 3)
  
  for (b in seq_len(B)) {
    set.seed(b)
    
    X <- design(n, p, rho)
    y <- y_sample(X, beta0, sigma = 1)
    
    sigma_hat_0 <- sigma_estimator(y, X, gamma = gamma_sigma)
    
    n1 <- floor(f*n)
    gamma <- sqrt(1/f - 1)
    
    split_reps <- matrix(0, nrow = R, ncol = length(active))
    rand_reps <- matrix(0, nrow = R, ncol = length(active))
    
    for (r in seq_len(R)) {
      ind <- sample(seq_len(n), size = n1, replace = FALSE)
      split_reps[r, ] <- Selection(y[ind], X[ind, , drop = FALSE])[active]
      
      w <- rnorm(n, sd = sigma_hat_0)
      u <- y + gamma*w
      rand_reps[r, ] <- Selection(u, X)[active]
    }
    
    split_mat[b, ] <- colMeans(split_reps)
    rand_mat[b, ] <- colMeans(rand_reps)
    
    setTxtProgressBar(pb, b)
  }
  
  close(pb)
  
  beta_abs <- abs(beta0[active])
  
  out <- rbind(
    summarize_selection(split_mat, beta_abs, "Data splitting"),
    summarize_selection(rand_mat, beta_abs, "Randomization")
  )
  
  out$method <- factor(out$method, levels = c("Data splitting", "Randomization"))
  
  out
}

B <- 100
R <- 50
n <- 200
p <- 50
rho <- 0.5
f <- 0.5

plot_df <- run_knockoff_stability(n = n, p = p, rho = rho, B = B, R = R, f = f, gamma_sigma = 0.6)

p_plot <- ggplot(plot_df, aes(x = effect, y = mean, color = method, linetype = method)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.025, linewidth = 0.35) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_color_manual(values = c("Data splitting" = "grey65", "Randomization" = "black")) +
  scale_linetype_manual(values = c("Data splitting" = "solid", "Randomization" = "solid")) +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  coord_cartesian(xlim = c(0.1, 1), ylim = c(0, 1)) +
  labs(x = expression(abs(beta[i])), y = expression(P(i %in% S~"|"~Y*","~X)), color = NULL, linetype = NULL) +
  theme_book +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

ggsave(file.path("Figures", "Outputs", "fig-5-06.pdf"), plot = p_plot, width = 4, height = 3)
