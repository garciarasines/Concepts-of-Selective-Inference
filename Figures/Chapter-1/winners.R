source(file.path("Figures", "theme.R"))

coverage <- function(n, B = 5e4, pb = NULL, offset = 0) {
  covered <- logical(B)

  for (b in seq_len(B)) {
    y <- rnorm(n)
    covered[b] <- max(y) < 1.96 && max(y) > -1.96

    if (!is.null(pb)) {
      setTxtProgressBar(pb, offset + b)
    }
  }

  mean(covered)
}

ns <- 1 + 5 * (0:20)
B <- 5e4

set.seed(123)

results <- numeric(length(ns))
pb <- txtProgressBar(min = 0, max = length(ns) * B, style = 3)

for (i in seq_along(ns)) {
  results[i] <- coverage(ns[i], B = B, pb = pb, offset = (i - 1) * B)
}

close(pb)

df <- data.frame(n = ns, coverage = results)

p <- ggplot(df, aes(x = n, y = coverage)) +
  geom_point(shape = 16, size = 2, color = "black") +
  geom_line(linewidth = 0.4, color = "black") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "n", y = "Coverage") +
  theme_book

ggsave(file.path("Figures", "Chapter-1", "winners.pdf"), plot = p, width = 5, height = 5)
