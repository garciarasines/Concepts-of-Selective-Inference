source(file.path("Figures", "theme.R"))

CDF_a <- function(y, mu) {
  1 - exp(pnorm(mu - y, log.p = TRUE) - pnorm(mu, log.p = TRUE))
}

L_a <- function(y) {
  G <- function(mu) CDF_a(y, mu) - 0.95
  uniroot(G, interval = c(-1000, 5))$root
}

U_a <- function(y) {
  G <- function(mu) CDF_a(y, mu) - 0.05
  uniroot(G, interval = c(-8, 5))$root
}

L_u_a <- function(y) {
  y - qnorm(0.95)
}

U_u_a <- function(y) {
  y + qnorm(0.95)
}

ys_a <- seq(0.01, 3, length.out = 1e4)
Ls_a <- sapply(ys_a, L_a)
Us_a <- sapply(ys_a, U_a)
L_us_a <- sapply(ys_a, L_u_a)
U_us_a <- sapply(ys_a, U_u_a)

df_a <- data.frame(y = ys_a, L = Ls_a, U = Us_a, L_u = L_us_a, U_u = U_us_a)

p_plot_a <- ggplot(df_a, aes(x = y)) +
  geom_line(aes(y = L), linewidth = 1, linetype = "longdash") +
  geom_line(aes(y = U), linewidth = 1, linetype = "longdash") +
  geom_line(aes(y = L_u), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = U_u), linewidth = 1, linetype = "solid") +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey70") +
  geom_abline(slope = 1, intercept = 0, linewidth = 1, linetype = "dotted", color = "grey70") +
  coord_cartesian(ylim = c(-10, 5)) +
  labs(x = "y", y = "CIs", title = expression(E == "["*0*", "*infinity*")")) +
  theme_book


a <- -2
b <- 2

CDF_b <- function(y, mu) {
  1/(1 - exp(pnorm(b - mu, log.p = TRUE, lower.tail = FALSE) - pnorm(a - mu, log.p = TRUE, lower.tail = FALSE))) - 1/(exp(pnorm(a - mu, log.p = TRUE, lower.tail = FALSE) - pnorm(y - mu, log.p = TRUE, lower.tail = FALSE)) - exp(pnorm(b - mu, log.p = TRUE, lower.tail = FALSE) - pnorm(y - mu, log.p = TRUE, lower.tail = FALSE)))
}

L_b <- function(y) {
  G <- function(mu) CDF_b(y, mu) - 0.95
  uniroot(G, interval = c(-1000, 5))$root
}

U_b <- function(y) {
  G <- function(mu) CDF_b(y, mu) - 0.05
  uniroot(G, interval = c(-1000, 5))$root
}

L_u_b <- function(y) {
  y - qnorm(0.95)
}

U_u_b <- function(y) {
  y + qnorm(0.95)
}

ys_b <- seq(a + 0.01, 0.0001, length.out = 1e3)
Ls_b <- sapply(ys_b, L_b)
Us_b <- sapply(ys_b, U_b)
L_us_b <- sapply(ys_b, L_u_b)
U_us_b <- sapply(ys_b, U_u_b)

df_b <- data.frame(y = c(ys_b, -rev(ys_b)), L = c(Ls_b, -rev(Us_b)), U = c(Us_b, -rev(Ls_b)), L_u = c(L_us_b, -rev(U_us_b)), U_u = c(U_us_b, -rev(L_us_b)))

p_plot_b <- ggplot(df_b, aes(x = y)) +
  geom_line(aes(y = L), linewidth = 1, linetype = "longdash") +
  geom_line(aes(y = U), linewidth = 1, linetype = "longdash") +
  geom_line(aes(y = L_u), linewidth = 1, linetype = "solid") +
  geom_line(aes(y = U_u), linewidth = 1, linetype = "solid") +
  geom_vline(xintercept = a, linewidth = 0.4, color = "grey70") +
  geom_vline(xintercept = b, linewidth = 0.4, color = "grey70") +
  geom_abline(slope = 1, intercept = 0, linewidth = 1, linetype = "dotted", color = "grey70") +
  coord_cartesian(ylim = c(-20, 20)) +
  labs(x = "y", y = "CIs", title = expression(E == "["*-2*", "*2*"]")) +
  theme_book

ggsave(file.path("Figures", "Chapter-4", "fig-4-4-a.pdf"), plot = p_plot_a, width = 5, height = 5)
ggsave(file.path("Figures", "Chapter-4", "fig-4-4-b.pdf"), plot = p_plot_b, width = 5, height = 5)
