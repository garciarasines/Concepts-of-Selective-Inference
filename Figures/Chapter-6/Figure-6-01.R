library(rmutil)
library(truncnorm)
source(file.path("Figures", "theme.R"))

G1 <- function(theta, B, n, k) {
  cov <- numeric(B)
  set.seed(123)
  
  for (b in seq_len(B)) {
    y <- rtruncnorm(1, mean = theta, sd = 1/sqrt(n), a = 0)
    cov[b] <- (theta < n*y/(n + 1) + k/sqrt(n + 1))*(theta > n*y/(n + 1) - k/sqrt(n + 1))
  }
  
  mean(cov)
}

G2 <- function(theta, B, n, k) {
  cov <- numeric(B)
  set.seed(123)
  
  for (b in seq_len(B)) {
    y <- rtruncnorm(1, mean = theta, sd = 1/sqrt(n), a = -Inf)
    cov[b] <- (theta < n*y/(n + 1) + k/sqrt(n + 1))*(theta > n*y/(n + 1) - k/sqrt(n + 1))
  }
  
  mean(cov)
}

G3 <- function(theta, B, n) {
  cov <- numeric(B)
  set.seed(123)
  
  for (b in seq_len(B)) {
    y <- rtruncnorm(1, mean = theta, sd = 1/sqrt(n), a = 0)
    
    post <- Vectorize(function(theta) {
      exp(dnorm(theta, log = TRUE) +
            dnorm(y, mean = theta, sd = 1/sqrt(n), log = TRUE) -
            pnorm(sqrt(n)*theta, log.p = TRUE))
    })
    
    K <- integrate(post, y - 10/sqrt(n), y + 10/sqrt(n))$value
    Post <- function(theta) integrate(post, y - 10/sqrt(n), theta)$value/K
    
    l <- uniroot(function(x) Post(x) - 0.05, c(y - 10/sqrt(n), y + 10/sqrt(n)))$root
    u <- uniroot(function(x) Post(x) - 0.95, c(y - 10/sqrt(n), y + 10/sqrt(n)))$root
    
    cov[b] <- (Post(theta) > 0.05)*(Post(theta) < 0.95)
  }
  
  mean(cov)
}

B <- 2000
n <- 5
k <- qnorm(0.95)

theta_grid <- seq(-1.8, 1.8, by = 0.1)

M1 <- numeric(length(theta_grid))
M2 <- numeric(length(theta_grid))
M3 <- numeric(length(theta_grid))

pb <- txtProgressBar(min = 0, max = 3*length(theta_grid), style = 3)
counter <- 0

for (i in seq_along(theta_grid)) {
  M1[i] <- G1(theta_grid[i], B, n, k)
  counter <- counter + 1
  setTxtProgressBar(pb, counter)
}

for (i in seq_along(theta_grid)) {
  M2[i] <- G2(theta_grid[i], B, n, k)
  counter <- counter + 1
  setTxtProgressBar(pb, counter)
}

for (i in seq_along(theta_grid)) {
  M3[i] <- G3(theta_grid[i], B, n)
  counter <- counter + 1
  setTxtProgressBar(pb, counter)
}

close(pb)

df <- data.frame(
  theta = rep(theta_grid, 3),
  coverage = c(M1, M2, M3),
  method = factor(
    rep(c("Truncated usual", "Untruncated usual", "Selective posterior"), each = length(theta_grid)),
    levels = c("Truncated usual", "Untruncated usual", "Selective posterior")
  )
)

p_plot <- ggplot(df, aes(x = theta, y = coverage, linetype = method, color = method)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.9, linewidth = 1, color = "grey70") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_color_manual(values = c("black", "black", "grey50")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = expression(theta), y = "Coverage") +
  theme_book +
  theme(
    legend.title = element_blank(),
    legend.position = "none"
  )

ggsave(file.path("Figures", "Chapter-6", "fig-6-1.pdf"), plot = p_plot, width = 5, height = 5)
