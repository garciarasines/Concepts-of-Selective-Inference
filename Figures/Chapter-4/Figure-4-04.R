library(truncnorm)
library(patchwork)
source(file.path("Figures", "theme.R"))

coverage_a <- function(mu, B = 4e4) {
  cov <- numeric(B)
  set.seed(123)
  
  for (b in seq_len(B)) {
    y <- rtruncnorm(1, mean = mu, a = 0)
    cov[b] <- abs(y - mu) < qnorm(0.95)
  }
  
  mean(cov)
}

mus_a <- seq(-2, 4, length.out = 3e2)
res_a <- numeric(length(mus_a))

pb <- txtProgressBar(min = 0, max = length(mus_a), style = 3)

for (i in seq_along(mus_a)) {
  res_a[i] <- coverage_a(mus_a[i])
  setTxtProgressBar(pb, i)
}

close(pb)

df_a <- data.frame(mu = mus_a, coverage = res_a)

p_plot_a <- ggplot(df_a, aes(x = mu, y = coverage)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.9, linewidth = 1, linetype = "dotted", color = "grey70") +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey70") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = expression(mu), y = "Coverage", title = expression("T" == "["*0*", "*infinity*")")) +
  theme_book


coverage_b <- function(mu, B = 4e4) {
  cov <- numeric(B)
  set.seed(123)
  
  for (b in seq_len(B)) {
    y <- rtruncnorm(1, mean = mu, a = -2, b = 2)
    cov[b] <- abs(y - mu) < qnorm(0.95)
  }
  
  mean(cov)
}

mus_b <- seq(-4, 4, length.out = 3e2)
res_b <- numeric(length(mus_b))

pb <- txtProgressBar(min = 0, max = length(mus_b), style = 3)

for (i in seq_along(mus_b)) {
  res_b[i] <- coverage_b(mus_b[i])
  setTxtProgressBar(pb, i)
}

close(pb)

df_b <- data.frame(mu = mus_b, coverage = res_b)

p_plot_b <- ggplot(df_b, aes(x = mu, y = coverage)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.9, linewidth = 1, linetype = "dotted", color = "grey70") +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey70") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = expression(mu), y = "Coverage", title = expression("T" == "["*-2*", "*2*"]")) +
  theme_book

p_plot <- p_plot_a / p_plot_b +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )

ggsave(file.path("Figures", "Outputs", "fig-4-04.pdf"), plot = p_plot, width = 4, height = 6)
