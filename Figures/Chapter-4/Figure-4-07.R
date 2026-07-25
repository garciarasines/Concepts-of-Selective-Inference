library(glmnet)
library(selectiveInference)
source(file.path("Figures", "theme.R"))

data(diabetes, package = "lars")
y <- diabetes$y
X <- diabetes$x
n <- dim(X)[1]
p <- dim(X)[2]

X <- scale(X, TRUE, TRUE)

lambda <- 12

gfit <- glmnet(X, y, standardize = FALSE)
beta <- coef(gfit, x = X, y = y, s = lambda, exact = TRUE)[-1]

# Selective CIs
X_int <- cbind(rep(1, n), X)
beta_hat <- solve(t(X_int)%*%X_int)%*%t(X_int)%*%y
sigma_hat <- sqrt(sum((y - X_int%*%beta_hat)^2)/(n - p - 1))

out <- fixedLassoInf(X, y, beta, n*lambda, sigma = sigma_hat)
lower_ad <- out$ci[, 1]
upper_ad <- out$ci[, 2]

# Non-selective CIs
d <- dim(out$ci)[1]
s <- which(beta != 0)
Xs <- cbind(rep(1, n), X[, s, drop = FALSE])
beta_hat <- solve(t(Xs)%*%Xs)%*%t(Xs)%*%y

upper_un <- (beta_hat + sqrt(diag(solve(t(Xs)%*%Xs)))*sigma_hat*qnorm(0.95))[-1]
lower_un <- (beta_hat - sqrt(diag(solve(t(Xs)%*%Xs)))*sigma_hat*qnorm(0.95))[-1]

var_names <- colnames(diabetes$x)[s]

df_plot <- data.frame(
  variable = factor(rep(var_names, 2), levels = var_names),
  index = rep(seq_len(d), 2),
  method = factor(
    rep(c("Unadjusted", "Selective"), each = d),
    levels = c("Unadjusted", "Selective")
  ),
  estimate = rep(as.numeric(beta_hat[-1]), 2),
  lower = c(as.numeric(lower_un), as.numeric(lower_ad)),
  upper = c(as.numeric(upper_un), as.numeric(upper_ad))
)

df_plot$x <- df_plot$index + ifelse(df_plot$method == "Unadjusted", -0.12, 0.12)

p_plot <- ggplot(df_plot, aes(x = x)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_errorbar(
    data = subset(df_plot, method == "Unadjusted"),
    aes(ymin = lower, ymax = upper),
    width = 0.06,
    linewidth = 0.5,
    linetype = "11",
    color = "grey45"
  ) +
  geom_point(
    data = subset(df_plot, method == "Unadjusted"),
    aes(y = estimate, shape = method),
    size = 1.8,
    color = "grey45"
  ) +
  geom_errorbar(
    data = subset(df_plot, method == "Selective"),
    aes(ymin = lower, ymax = upper),
    width = 0.12,
    linewidth = 0.9,
    color = "black"
  ) +
  geom_point(
    data = subset(df_plot, method == "Selective"),
    aes(y = estimate, shape = method),
    size = 2.2,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq_len(d),
    labels = var_names
  ) +
  scale_shape_manual(values = c(
    "Unadjusted" = 1,
    "Selective" = 16
  )) +
  labs(
    x = NULL,
    y = "Coefficient",
    shape = NULL
  ) +
  theme_book +
  theme(
    legend.position = c(0.68, 0.92),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

ggsave(file.path("Figures", "Outputs", "fig-4-07.pdf"), plot = p_plot, width = 4, height = 3)

