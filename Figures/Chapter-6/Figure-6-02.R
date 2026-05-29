source(file.path("Figures", "theme.R"))

y <- 0.1

theta_grid <- seq(-3, 3, length.out = 1e4)

prior <- dnorm(theta_grid)

unadjusted <- dnorm(theta_grid, mean = y/2, sd = 1/sqrt(2))

log_post_sel <- function(theta) {
  dnorm(theta, log = TRUE) +
    dnorm(y, mean = theta, sd = 1, log = TRUE) -
    pnorm(theta, log.p = TRUE)
}

post_sel_unnormalized <- function(theta) {
  exp(log_post_sel(theta))
}

K <- integrate(post_sel_unnormalized, lower = -40, upper = 40)$value
selective <- post_sel_unnormalized(theta_grid)/K

df <- data.frame(
  theta = rep(theta_grid, 3),
  density = c(prior, selective, unadjusted),
  distribution = factor(
    rep(c("Prior", "Selective posterior", "Unadjusted posterior"), each = length(theta_grid)),
    levels = c("Prior", "Selective posterior", "Unadjusted posterior")
  )
)

p_plot <- ggplot(df, aes(x = theta, y = density, linetype = distribution, color = distribution)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "42", "dotted")) +
  scale_color_manual(values = c("grey50", "black", "black")) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(0, 0.72)) +
  labs(x = expression(theta), y = "Density") +
  theme_book +
  theme(legend.position = "none")

ggsave(file.path("Figures", "Outputs", "fig-6-02.pdf"), plot = p_plot, width = 4, height = 3)

