source(file.path("Figures", "theme.R"))

CDF <- function(y, mu) {
  1 - exp(pnorm(mu - y, log.p = TRUE) - pnorm(mu, log.p = TRUE))
}

L <- function(y) {
  G <- function(mu) CDF(y, mu) - 0.95
  uniroot(G, interval = c(-100000, 100))$root
}

U <- function(y) {
  G <- function(mu) CDF(y, mu) - 0.05
  uniroot(G, interval = c(-100000, 100))$root
}

L_u <- function(y) {
  y - qnorm(0.95)
}

U_u <- function(y) {
  y + qnorm(0.95)
}

coverage <- function(mu, n, B) {
  set.seed(123)
  covs <- numeric(B)
  
  for (b in seq_len(B)) {
    y <- -1
    while (y < 0.00003) {
      y <- mu + sqrt(n)*mean(rt(n, df = 3)/sqrt(3))
    }
    covs[b] <- (mu > L(y))*(mu < U(y))
  }
  
  mean(covs)
}

coverage_nonsel <- function(mu, n, B) {
  set.seed(123)
  covs <- numeric(B)
  
  for (b in seq_len(B)) {
    y <- mu + sqrt(n)*mean(rt(n, df = 3)/sqrt(3))
    covs[b] <- (mu > L_u(y))*(mu < U_u(y))
  }
  
  mean(covs)
}

B <- 1e4
n <- 10
mus <- seq(-3, 4, length.out = 20)
res <- numeric(length(mus))

pb <- txtProgressBar(min = 0, max = length(mus), style = 3)

for (j in seq_along(mus)) {
  res[j] <- coverage(mus[j], n, B)
  setTxtProgressBar(pb, j)
}

close(pb)

nonsel <- coverage_nonsel(0, n, B)

df <- data.frame(mu = mus, coverage = res)

p_plot <- ggplot(df, aes(x = mu, y = coverage)) +
  geom_line(linewidth = 1) +
  geom_hline(
    yintercept = nonsel,
    linewidth = 1,
    color = "grey50"
  ) +
  geom_hline(
    yintercept = 0.9,
    linewidth = 1,
    linetype = "dotted",
    color = "grey70"
  ) +
  coord_cartesian(ylim = c(0.5, 1)) +
  labs(x = expression(mu), y = "Coverage") +
  theme_book

ggsave(file.path("Figures", "Outputs", "fig-5-01.pdf"), plot = p_plot, width = 4, height = 3)
