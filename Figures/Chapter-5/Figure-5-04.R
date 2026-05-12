source(file.path("Figures", "theme.R"))

h1 <- function(x) {
  exp(dnorm(x, log = TRUE) - pnorm(x, log.p = TRUE))
}

h2 <- function(x) {
  -x*h1(x) - h1(x)^2
}

i <- function(gamma) {
  (1 + h2(theta/sqrt((1 + gamma)*sigma2)))/((1 + gamma)*sigma2) + (1 + 1/gamma)^(-1)/sigma2
}

iv <- function(gamma) {
  (1 + 1/gamma)^(-1)/sigma2
}

sigma2 <- 1/100
xs <- seq(0, 3, length.out = 1e3)

theta_values <- c(-1, 0, 1)

df <- do.call(rbind, lapply(theta_values, function(theta_value) {
  theta <<- theta_value
  data.frame(
    gamma = xs,
    I = sapply(xs, i),
    Iv = sapply(xs, iv),
    theta = paste0("theta == ", theta_value)
  )
}))

p_plot <- ggplot(df, aes(x = gamma)) +
  geom_line(aes(y = I), linewidth = 1) +
  geom_line(aes(y = Iv), linewidth = 1, linetype = "dashed") +
  facet_wrap(~ theta, nrow = 1, labeller = label_parsed) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = expression(gamma), y = "I") +
  theme_book

ggsave(file.path("Figures", "Chapter-5", "fig-5-04.pdf"), plot = p_plot, width = 4, height = 3)
