library(patchwork)
library(mvtnorm)
source(file.path("Figures", "theme.R"))

h1 <- function(x) {
  exp(dnorm(x, log = TRUE) - pnorm(x, log.p = TRUE))
}

h2 <- function(x) {
  -x*h1(x) - h1(x)^2
}

n <- 20
t <- 0
n1 <- 20
n2 <- n - n1
gamma <- n1/n
y <- 0.2

pi_STD <- function(theta) {
  1
}

pi_J <- function(theta) {
  sqrt(1 + gamma*h2(sqrt(n1)*(theta - t)))
}

pi_PMP <- function(theta) {
  1 - h1(sqrt(n)*theta)/h1(sqrt(n)*(theta - y))
}

log_phi <- function(theta) {
  pnorm(sqrt(n1)*theta, log.p = TRUE)
}

post_STD_u <- Vectorize(function(theta) {
  pi_STD(theta)*exp(dnorm(y, mean = theta, sd = 1/sqrt(n), log = TRUE) - log_phi(theta))
})

post_J_u <- Vectorize(function(theta) {
  pi_J(theta)*exp(dnorm(y, mean = theta, sd = 1/sqrt(n), log = TRUE) - log_phi(theta))
})

post_PMP_u <- Vectorize(function(theta) {
  pi_PMP(theta)*exp(dnorm(y, mean = theta, sd = 1/sqrt(n), log = TRUE) - log_phi(theta))
})

K_STD <- integrate(post_STD_u, -3, 1.5)$value
K_J <- integrate(post_J_u, -3, 1.5)$value
K_PMP <- integrate(post_PMP_u, -3, 1.5)$value

post_STD <- Vectorize(function(theta) {
  post_STD_u(theta)/K_STD
})

post_J <- Vectorize(function(theta) {
  post_J_u(theta)/K_J
})

post_PMP <- Vectorize(function(theta) {
  post_PMP_u(theta)/K_PMP
})

thetas <- seq(-3, 3, length.out = 1e3)

df_a <- data.frame(
  theta = rep(thetas, 3),
  value = c(sapply(thetas, pi_STD), sapply(thetas, pi_J), sapply(thetas, pi_PMP)),
  prior = factor(
    rep(c("Standard", "Jeffreys", "PMP"), each = length(thetas)),
    levels = c("Standard", "Jeffreys", "PMP")
  )
)

df_b <- data.frame(
  theta = rep(thetas, 3),
  value = c(sapply(thetas, post_STD), sapply(thetas, post_J), sapply(thetas, post_PMP)),
  posterior = factor(
    rep(c("Standard", "Jeffreys", "PMP"), each = length(thetas)),
    levels = c("Standard", "Jeffreys", "PMP")
  )
)

p_plot_a <- ggplot(df_a, aes(x = theta, y = value, linetype = prior)) +
  geom_line(linewidth = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(0, 1.1)) +
  labs(x = expression(theta), y = "Prior") +
  theme_book +
  theme(legend.position = "none")

p_plot_b <- ggplot(df_b, aes(x = theta, y = value, linetype = posterior)) +
  geom_line(linewidth = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "42", "dotted")) +
  coord_cartesian(xlim = c(-2, 1)) +
  labs(x = expression(theta), y = "Posterior") +
  theme_book +
  theme(legend.position = "none")

p_plot <- p_plot_a / p_plot_b +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )

ggsave(file.path("Figures", "Outputs", "fig-6-03.pdf"), plot = p_plot, width = 4, height = 6)
