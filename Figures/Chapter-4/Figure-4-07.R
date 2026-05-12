library(glmnet)
library(selectiveInference)
source(file.path("Figures", "theme.R"))

data(diabetes, package = "lars")
y <- diabetes$y
X <- diabetes$x
n <- dim(X)[1]
p <- dim(X)[2]

X <- scale(X, TRUE, TRUE)

gfit <- glmnet(X, y, standardize = FALSE)
beta <- coef(gfit, x = X, y = y, s = 7, exact = TRUE)[-1]

lambda <- 7

beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y
sigma_hat <- sqrt(sum((y - X%*%beta_hat)^2)/(n - p))

out <- fixedLassoInf(X, y, beta, lambda, sigma = sigma_hat)
lower_ad <- out$ci[, 1]
upper_ad <- out$ci[, 2]

d <- dim(out$ci)[1]
s <- which(beta != 0)
Xs <- X[, s, drop = FALSE]

beta_hat <- solve(t(Xs)%*%Xs)%*%t(Xs)%*%y
yhat <- Xs%*%beta_hat
sqrt(sum((y - yhat)^2)/(n - p))

sigma_hat <- sqrt(sum((y - Xs%*%beta_hat)^2)/(n - d))
upper_un <- beta_hat + sqrt(diag(solve(t(Xs)%*%Xs)))*sigma_hat*qnorm(0.95)
lower_un <- beta_hat - sqrt(diag(solve(t(Xs)%*%Xs)))*sigma_hat*qnorm(0.95)

var_names <- toupper(colnames(diabetes$x)[s])

df_un <- data.frame(
  x = 1:d - 0.12,
  lower = as.numeric(lower_un),
  upper = as.numeric(upper_un)
)

df_ad <- data.frame(
  x = 1:d + 0.12,
  lower = as.numeric(lower_ad),
  upper = as.numeric(upper_ad)
)

cap_width <- 0.07

p_plot <- ggplot() +
  geom_segment(
    data = df_un,
    aes(x = x, xend = x, y = lower, yend = upper),
    linewidth = 1,
    color = "black"
  ) +
  geom_segment(
    data = df_un,
    aes(x = x - cap_width, xend = x + cap_width, y = lower, yend = lower),
    linewidth = 1,
    color = "black"
  ) +
  geom_segment(
    data = df_un,
    aes(x = x - cap_width, xend = x + cap_width, y = upper, yend = upper),
    linewidth = 1,
    color = "black"
  ) +
  geom_segment(
    data = df_ad,
    aes(x = x, xend = x, y = lower, yend = upper),
    linewidth = 1,
    color = "grey50"
  ) +
  geom_segment(
    data = df_ad,
    aes(x = x - cap_width, xend = x + cap_width, y = lower, yend = lower),
    linewidth = 1,
    color = "grey50"
  ) +
  geom_segment(
    data = df_ad,
    aes(x = x - cap_width, xend = x + cap_width, y = upper, yend = upper),
    linewidth = 1,
    color = "grey50"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey70") +
  scale_x_continuous(
    breaks = 1:d,
    labels = var_names
  ) +
  coord_cartesian(
    xlim = c(0.5, d + 0.5),
    ylim = c(min(c(lower_un, lower_ad)) - 20, max(c(upper_un, upper_ad)) + 20)
  ) +
  labs(x = NULL, y = "CIs") +
  theme_book

ggsave(file.path("Figures", "Chapter-4", "fig-4-8.pdf"), plot = p_plot, width = 6, height = 4)
