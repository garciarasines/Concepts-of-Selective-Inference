library(patchwork)
library(mvtnorm)
source(file.path("Figures", "theme.R"))

h1 <- function(x) {
  exp(dnorm(x, log = TRUE) - pnorm(x, log.p = TRUE))
}

h2 <- function(x) {
  -x*h1(x) - h1(x)^2
}

g <- function(x) {
  asin(sqrt(x))
}

t <- 0.5
gamma <- 0.8
n <- 10
n1 <- n*gamma
n2 <- n - n1
K <- sqrt(gamma/(1 - gamma))
b <- sqrt(gamma/(1 - gamma))
d <- sqrt(1 + b^2)
rho <- b/d

I <- function(psi) {
  a <- sqrt(n*gamma/(1 - gamma))*(psi - g(t))
  pmvnorm(
    lower = c(-Inf, -Inf),
    upper = c(a/d, sqrt(n)*(psi - psi_hat)),
    mean = c(0, 0),
    corr = matrix(c(1, rho, rho, 1), ncol = 2)
  )[1]
}

pi_PMP_psi <- function(psi) {
  sqrt(gamma)*exp(dnorm(sqrt(n*gamma)*(g(t) - psi), log = TRUE) - dnorm(sqrt(n)*(psi_hat - psi), log = TRUE))*
    (1 - exp(pnorm(sqrt(n/(1 - gamma))*(psi_hat - psi + gamma*(psi - g(t))), log.p = TRUE)) -
       I(psi)*exp(-pnorm(sqrt(n*gamma)*(psi - g(t)), log.p = TRUE))) +
    exp(pnorm(sqrt(n*gamma/(1 - gamma))*(psi_hat - g(t)), log.p = TRUE))
}

log_phi <- function(theta) {
  pbinom(ceiling(0.5*n1) - 1, n1, theta, lower.tail = FALSE, log.p = TRUE)
}

theta_hat_1 <- 0.5
theta_hat_2 <- 0.5
theta_hat <- (n1*theta_hat_1 + n2*theta_hat_2)/n
psi_hat <- g(theta_hat)

pi_STD <- function(theta) {
  1/(sqrt(theta)*sqrt(1 - theta))
}

pi_J <- function(theta) {
  sqrt(1 + gamma*h2(sqrt(n1)*(g(theta) - g(t))))/(sqrt(theta)*sqrt(1 - theta))
}

pi_PMP <- function(theta) {
  pi_PMP_psi(g(theta))/(sqrt(theta)*sqrt(1 - theta))
}

post_STD_u <- Vectorize(function(theta) {
  pi_STD(theta)*exp(dbinom(n*theta_hat, n, theta, log = TRUE) - log_phi(theta))
})

post_J_u <- Vectorize(function(theta) {
  pi_J(theta)*exp(dbinom(n*theta_hat, n, theta, log = TRUE) - log_phi(theta))
})

post_PMP_u <- Vectorize(function(theta) {
  pi_PMP(theta)*exp(dbinom(n*theta_hat, n, theta, log = TRUE) - log_phi(theta))
})

K_STD <- integrate(post_STD_u, 1e-4, 1 - 1e-4)$value
K_J <- integrate(post_J_u, 1e-4, 1 - 1e-4)$value
K_PMP <- integrate(post_PMP_u, 1e-4, 1 - 1e-4)$value

post_STD <- Vectorize(function(theta) {
  post_STD_u(theta)/K_STD
})

post_J <- Vectorize(function(theta) {
  post_J_u(theta)/K_J
})

post_PMP <- Vectorize(function(theta) {
  post_PMP_u(theta)/K_PMP
})

thetas <- seq(1e-4, 1 - 1e-4, length.out = 1e3)

df_a <- data.frame(
  theta = rep(thetas, 3),
  value = c(
    0.08*sapply(thetas, pi_STD),
    0.08*pi_STD(0.5)*sapply(thetas, pi_J)/pi_J(0.5),
    0.08*pi_STD(0.5)*sapply(thetas, pi_PMP)/pi_PMP(0.5)
  ),
  prior = factor(
    rep(c("Standard", "Jeffreys", "PMP"), each = length(thetas)),
    levels = c("Standard", "Jeffreys", "PMP")
  )
)

df_b <- data.frame(
  theta = rep(thetas, 3),
  value = c(
    sapply(thetas, post_STD),
    sapply(thetas, post_J),
    sapply(thetas, post_PMP)
  ),
  posterior = factor(
    rep(c("Standard", "Jeffreys", "PMP"), each = length(thetas)),
    levels = c("Standard", "Jeffreys", "PMP")
  )
)

p_plot_a <- ggplot(df_a, aes(x = theta, y = value, linetype = prior)) +
  geom_line(linewidth = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "42", "dotted")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = expression(theta), y = "Prior") +
  theme_book +
  theme(legend.position = "none")

p_plot_b <- ggplot(df_b, aes(x = theta, y = value, linetype = posterior)) +
  geom_line(linewidth = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "42", "dotted")) +
  coord_cartesian(ylim = c(0, 2)) +
  labs(x = expression(theta), y = "Posterior") +
  theme_book +
  theme(legend.position = "none")

p_plot <- p_plot_a / p_plot_b +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )

ggsave(file.path("Figures", "Outputs", "fig-6-04.pdf"), plot = p_plot, width = 4, height = 6)
