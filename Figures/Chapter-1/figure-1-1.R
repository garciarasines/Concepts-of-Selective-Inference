source(file.path("Figures", "theme.R"))

coverage <- function(n, B = 5e4) {
  covs <- numeric(B)
  set.seed(111)

  for (b in seq_len(B)) {
    y <- rnorm(n)
    covs[b] <- (max(y) < 1.96) * (max(y) > -1.96)
  }

  mean(covs)
}

ns <- 1 + 5 * (0:20)
results <- sapply(ns, coverage)

df <- data.frame(
  n = ns,
  coverage = results
)

p <- ggplot(df, aes(x = n, y = coverage)) +
  geom_point(shape = 16, size = 2, color = "black") +
  geom_line(linewidth = 0.4, color = "black") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "n", y = "Coverage", title = "Figure 1.1: Winner's Curse Coverage") +
  theme_book

ggsave(file.path("Figures", "Chapter-1", "winners.pdf"), plot = p, width = 5, height = 5)
