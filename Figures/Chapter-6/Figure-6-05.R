source(file.path("Figures", "theme.R"))

B <- 1000
m <- 1000
sigma2 <- 1

set.seed(123)

face_diff <- numeric(B)
eb_diff <- numeric(B)

pb <- txtProgressBar(min = 0, max = B, style = 3)

for (b in seq_len(B)) {
  theta <- rnorm(m)
  y <- rnorm(m, mean = theta, sd = sqrt(sigma2))
  
  tau2_hat <- max(var(y) - sigma2, 0)
  theta_hat <- tau2_hat/(tau2_hat + sigma2)*y
  
  I <- which.max(y)
  
  face_diff[b] <- y[I] - theta[I]
  eb_diff[b] <- theta_hat[I] - theta[I]
  
  setTxtProgressBar(pb, b)
}

close(pb)

df <- data.frame(
  difference = c(face_diff, eb_diff),
  method = factor(
    rep(c("Face value", "EB"), each = B),
    levels = c("Face value", "EB")
  )
)

p_plot <- ggplot(df, aes(x = difference, y = after_stat(density))) +
  geom_histogram(
    data = subset(df, method == "Face value"),
    bins = 45,
    fill = "white",
    color = "black",
    linewidth = 0.3
  ) +
  geom_histogram(
    data = subset(df, method == "EB"),
    bins = 45,
    fill = "grey75",
    color = "black",
    linewidth = 0.3,
    alpha = 0.8
  ) +
  coord_cartesian(xlim = c(-2, 4.5), ylim = c(0, 0.7)) +
  labs(x = "Difference", y = "Density") +
  theme_book +
  theme(legend.position = "none")

ggsave(file.path("Figures", "Chapter-6", "fig-6-5.pdf"), plot = p_plot, width = 5, height = 5)
