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

sample_indices <- function(X, n1) {
  n <- nrow(X)
  sample(seq_len(n), size = n1, replace = FALSE)
}

SEL <- function(n, p, rho, B, f) {
  Sigma_X <- outer(1:p, 1:p, function(i, j) rho^abs(i - j))
  
  FULL <- numeric(B)
  DS <- numeric(B)
  R <- numeric(B)
  
  pb <- txtProgressBar(min = 0, max = B, style = 3)
  
  for (i in seq_len(B)) {
    set.seed(i)
    
    nact <- 10
    beta0 <- rep(0, p)
    beta0[sample(1:p, nact)] <- sample(grid, nact, replace = TRUE)*sign(runif(nact, -1, 1))
    
    S_0 <- beta0 != 0
    
    X <- rmvnorm(n, mean = rep(0, p), sigma = Sigma_X)
    y <- y_sample(X, beta0, sigma = 1)
    
    S_FULL <- Selection(y, X)
    
    n1 <- floor(f*n)
    ind <- sample_indices(X, n1)
    
    X1 <- X[ind, ]
    y1 <- y[ind]
    S_DS <- Selection(y1, X1)
    
    sigma_hat_0 <- sigma_estimator(y, X, gamma = 0.6)
    w <- rnorm(n, sd = sigma_hat_0)
    gamma <- sqrt(1/f - 1)
    u <- y + gamma*w
    S_R <- Selection(u, X)
    
    if (i == 1) {
      beta0s <- abs(beta0)
      S_FULLs <- S_FULL
      S_DSs <- S_DS
      S_Rs <- S_R
    } else {
      beta0s <- c(beta0s, abs(beta0))
      S_FULLs <- c(S_FULLs, S_FULL)
      S_DSs <- c(S_DSs, S_DS)
      S_Rs <- c(S_Rs, S_R)
    }
    
    FULL[i] <- sum(S_FULL*S_0)/nact
    DS[i] <- sum(S_DS*S_0)/nact
    R[i] <- sum(S_R*S_0)/nact
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  out1 <- c(mean(FULL), mean(DS), mean(R))
  out2 <- cbind(beta0s, S_FULLs, S_DSs, S_Rs)
  
  list(out1, out2)
}

B <- 1000
n <- 200
p <- 50
rho <- 0.5
f <- 0.5
grid <- seq(0.1, 1, by = 0.1)

RESULT <- SEL(n = n, p = p, rho = rho, B = B, f = f)

power_df <- data.frame(
  effect = rep(grid, 3),
  power = c(
    sapply(grid, function(a) mean(RESULT[[2]][RESULT[[2]][, 1] == a, 2])),
    sapply(grid, function(a) mean(RESULT[[2]][RESULT[[2]][, 1] == a, 3])),
    sapply(grid, function(a) mean(RESULT[[2]][RESULT[[2]][, 1] == a, 4]))
  ),
  method = factor(rep(c("Full", "Data splitting", "Randomized"), each = length(grid)), levels = c("Full", "Data splitting", "Randomized"))
)

p_plot <- ggplot(power_df, aes(x = effect, y = power, linetype = method, color = method)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_linetype_manual(values = c("Full" = "solid", "Data splitting" = "dotted", "Randomized" = "dashed")) +
  scale_color_manual(values = c("Full" = "black", "Data splitting" = "grey60", "Randomized" = "black")) +
  coord_cartesian(xlim = c(0.1, 1), ylim = c(0, 1)) +
  labs(x = "Effect", y = "Power") +
  theme_book +
  theme(legend.position = "top", legend.title = element_blank())

ggsave(file.path("Figures", "Outputs", "fig-5-05.pdf"), plot = p_plot, width = 4, height = 3)
