library(lars)
library(glmnet)
library(PoSI)
library(mvtnorm)

source(file.path("Figures", "theme.R"))

set.seed(123)

# Data
data(diabetes)

y <- diabetes$y
X <- diabetes$x

n <- nrow(X)
p <- ncol(X)

X_int <- cbind(int = rep(1, n), X)

# Model selection
lasso <- cv.glmnet(X, y)

s <- as.logical(coef(lasso, s = "lambda.1se") != 0)

d <- sum(s)
Xs <- X_int[, s]

sel_cov <- colnames(Xs)

# PoSI constant
posi_obj <- PoSI(X, Nsim = 5e4, verbose = 0)
K_posi <- summary(posi_obj, df.err = NULL)[1, 1]

# Unadjusted multiplicative constant for the selected model
XtX_inv <- solve(t(Xs)%*%Xs)

U <- rmvnorm(1e4, sigma = XtX_inv)

K_un <- quantile(
  apply(U, 1, function(u) max(abs(u)/sqrt(diag(XtX_inv)))),
  0.95
)

# Least-squares estimate in the selected model
psi_hat <- XtX_inv%*%t(Xs)%*%y

sigma_hat <- sqrt(sum((y - Xs%*%psi_hat)^2)/(n - d))
se_hat <- sqrt(diag(XtX_inv))*sigma_hat

# Unadjusted intervals
upper_un <- psi_hat + K_un*se_hat
lower_un <- psi_hat - K_un*se_hat

# PoSI intervals
upper_posi <- psi_hat + K_posi*se_hat
lower_posi <- psi_hat - K_posi*se_hat

df_plot <- data.frame(
  variable = factor(rep(sel_cov, 2), levels = sel_cov),
  index = rep(seq_len(d), 2),
  method = factor(
    rep(c("Unadjusted", "PoSI"), each = d),
    levels = c("Unadjusted", "PoSI")
  ),
  estimate = rep(as.numeric(psi_hat), 2),
  lower = c(as.numeric(lower_un), as.numeric(lower_posi)),
  upper = c(as.numeric(upper_un), as.numeric(upper_posi))
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
    data = subset(df_plot, method == "PoSI"),
    aes(ymin = lower, ymax = upper),
    width = 0.12,
    linewidth = 0.9,
    color = "black"
  ) +
  geom_point(
    data = subset(df_plot, method == "PoSI"),
    aes(y = estimate, shape = method),
    size = 2.2,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq_len(d),
    labels = sel_cov
  ) +
  scale_shape_manual(values = c(
    "Unadjusted" = 1,
    "PoSI" = 16
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

ggsave(file.path("Figures", "Outputs", "fig-2-02.pdf"), plot = p_plot, width = 4, height = 3)
