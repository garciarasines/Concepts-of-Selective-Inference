source(file.path("Figures", "theme.R"))

alpha <- 0.10
c_sel <- 2
pi_signal <- 0.10
prior_var <- 3
y_var_signal <- prior_var + 1

p_sel_theta <- function(theta) {
  pnorm(-c_sel - theta) + pnorm(c_sel - theta, lower.tail = FALSE)
}

F_theta_E <- function(y, theta) {
  p_left <- pnorm(-c_sel - theta)
  p_E <- p_sel_theta(theta)
  
  ifelse(
    y < -c_sel,
    pnorm(y - theta)/p_E,
    ifelse(
      y > c_sel,
      (p_left + pnorm(y - theta) - pnorm(c_sel - theta))/p_E,
      NA
    )
  )
}

q_theta_E <- function(u, theta) {
  u <- pmin(pmax(u, 1e-12), 1 - 1e-12)
  
  p_left <- pnorm(-c_sel - theta)
  p_E <- p_sel_theta(theta)
  split <- p_left/p_E
  
  ifelse(
    u <= split,
    theta + qnorm(u*p_E),
    theta + qnorm(pnorm(c_sel - theta) + u*p_E - p_left)
  )
}

F_marginal <- function(y) {
  (1 - pi_signal)*pnorm(y) + pi_signal*pnorm(y, sd = sqrt(y_var_signal))
}

f_marginal <- function(y) {
  (1 - pi_signal)*dnorm(y) + pi_signal*dnorm(y, sd = sqrt(y_var_signal))
}

p_E_marginal <- F_marginal(-c_sel) + 1 - F_marginal(c_sel)

F_marginal_E <- function(y) {
  ifelse(
    y < -c_sel,
    F_marginal(y)/p_E_marginal,
    ifelse(
      y > c_sel,
      (F_marginal(-c_sel) + F_marginal(y) - F_marginal(c_sel))/p_E_marginal,
      NA
    )
  )
}

H <- function(w, theta) {
  y_lower <- q_theta_E(alpha*w, theta)
  y_upper <- q_theta_E(alpha*w + 1 - alpha, theta)
  F_marginal_E(y_upper) - F_marginal_E(y_lower)
}

w_opt <- function(theta) {
  ws <- seq(0, 1, length.out = 501)
  vals <- sapply(ws, H, theta = theta)
  w0 <- ws[which.min(vals)]
  lo <- max(0, w0 - 0.01)
  hi <- min(1, w0 + 0.01)
  
  optimize(function(w) H(w, theta), interval = c(lo, hi))$minimum
}

theta_grid <- seq(-5, 5, length.out = 1001)
w_star <- sapply(theta_grid, w_opt)

df_11 <- data.frame(
  theta = rep(theta_grid, 2),
  w = c(w_star, rep(0.5, length(theta_grid))),
  method = factor(
    rep(c("Optimal", "Equal-tailed"), each = length(theta_grid)),
    levels = c("Optimal", "Equal-tailed")
  )
)

p_plot <- ggplot(df_11, aes(x = theta, y = w, linetype = method, color = method)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_color_manual(values = c("black", "grey70")) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 1)) +
  labs(x = expression(theta), y = expression(w(theta))) +
  theme_book +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

ggsave(file.path("Figures", "Outputs", "fig-6-11.pdf"), plot = p_plot, width = 5, height = 5)
