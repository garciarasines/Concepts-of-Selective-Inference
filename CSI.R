library(ggplot2)
library(MASS)
library(PoSI)
library(mvtnorm)

theme_book = theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2),
    axis.title = element_text(face = "bold", size = 12),
    axis.text  = element_text(color = "black", size = 11),
    legend.text = element_text(color = "black", size = 11),
    legend.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
  )

################# 1.1 #################

coverage = function(n, B = 5e4){
  covs = numeric(B)
  set.seed(111)
  for (b in 1:B){
    y = rnorm(n)
    covs[b] = (max(y) < 1.96) * (max(y) > -1.96)
  }
  mean(covs)
}

ns = 1 + 5 * (0:20)
results = sapply(ns, coverage)

df = data.frame(
  n = ns,
  coverage = results
)

p = ggplot(df, aes(x = n, y = coverage)) +
  geom_point(shape = 16, size = 2, color = "black") +
  geom_line(linewidth = 0.4, color = "black") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "n", y = "Coverage") +
  theme_book

ggsave("winners.pdf", plot = p, width = 5, height = 5)


################# 1.2 #################

B = 1e4
n = 80
p = 20

p_vals = c()

set.seed(111)
for (i in 1:B){
  
  X = data.frame(matrix(rnorm(n * p), ncol = p))
  y = rnorm(n)
  
  min.model = lm(y ~ 1, data = X)
  max.model = formula(lm(y ~ ., data = X))
  
  step = stepAIC(min.model, direction = "forward", scope = max.model, trace = FALSE)
  p_vals = c(p_vals, summary(step)$coefficients[ , 4])
}

mean(p_vals < 0.05)

df = data.frame(p_value = p_vals)

p_plot = ggplot(df, aes(x = p_value)) +
  stat_ecdf(geom = "step", linewidth = 0.4, color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = "p-value",
    y = "Empirical CDF"
  ) +
  theme_book

ggsave("stepwise.pdf", plot = p_plot, width = 5, height = 5)


################# 2.1 #################

# Simulation size
B = 5e4

# Parameters
p = 10
n_max = 40

# Design matrix with correlation matrix Sigma
Sigma = matrix(NA, nrow = p, ncol = p)
Sigma[upper.tri(Sigma, diag = FALSE)] = 0.5
Sigma[lower.tri(Sigma, diag = FALSE)] = 0.5
diag(Sigma) = rep(1, p)
L = chol(Sigma)

set.seed(111)
X_max = t(t(L) %*% matrix(rnorm(p * n_max), nrow = p))

# Plot
ns = 10:n_max
Ks1 = numeric(length(ns))
Ks2 = numeric(length(ns))

for (n in ns){
  
  X = X_max[1:n, ]
  
  POSI = PoSI(X, Nsim = B, verbose = 0)
  Ks1[i] = summary(POSI, df.err = NULL)[1, 1]
  
  POSI = PoSI(X, modelSZ = 1:5, Nsim = B, verbose = 0)
  Ks2[i] = summary(POSI, df.err = NULL)[1, 1]
  
}

df = data.frame(
  n = ns,
  Ks1 = Ks1,
  Ks2 = Ks2
)

p_plot = ggplot(df, aes(x = n)) +
  geom_point(aes(y = Ks1), shape = 16, size = 1.8) +
  geom_line(aes(y = Ks1), linewidth = 0.4) +
  geom_point(aes(y = Ks2), shape = 1, size = 1.8) +
  geom_line(aes(y = Ks2), linewidth = 0.4, linetype = "dashed") +
  labs(x = "n", y = "K") +
  theme_book

ggsave("posi_constants.pdf", plot = p_plot, width = 6, height = 4)



################# 3.1 #################

mu = function(x) 5 + 5e-2*x^4

x_grid = seq(0, 4, length.out = 600)

curve_df = data.frame(
  x = x_grid,
  y = mu(x_grid)
)

x1 = 3.0
x2 = 6.9

p = ggplot(curve_df, aes(x = x, y = y)) +
    geom_line(linewidth = 1, color = "black") +
  scale_y_continuous(limits = c(0, 10)) +
    theme_book
p

ggsave("ancillarity1.pdf", width = 5, height = 5)

################# 4.1 #################

# Simulation size
B = 1e6

set.seed(111)
K = quantile(replicate(1e5, max(abs(rnorm(2)))), 0.95)
e1s = rnorm(B)
e2s = rnorm(B)

coverages = function(mu2){
  
  mu = c(0, mu2)
  I = numeric(B)
  cov = numeric(B)
  
  for(b in 1:B){
    
    y1 = mu[1] + e1s[b]
    y2 = mu[2] + e2s[b]
    y = c(y1, y2)
    
    I[b] = which(y == max(y))
    yI = y[I[b]]
    cov[b] = (abs(yI - mu[I[b]]) < K)
    
  }
  
  c(mean(cov), mean(cov[I == 1]), mean(cov[I == 2]))
}

mu2s = seq(0, 3, length.out = 1e2)
res = sapply(mu2s, coverages)

df = data.frame(
  mu2 = mu2s,
  cov1 = res[2, ],
  cov2 = res[3, ]
)

p = ggplot(df, aes(x = mu2)) +
  geom_line(aes(y = cov1), linewidth = 0.4) +
  geom_point(aes(y = cov1), shape = 16, size = 1.8) +
  geom_line(aes(y = cov2), linewidth = 0.4, linetype = "dashed") +
  geom_point(aes(y = cov2), shape = 1, size = 1.8) +
  geom_hline(yintercept = res[1, 1], linewidth = 0.4) +
  geom_hline(yintercept = 0.95, linewidth = 0.4, linetype = "dashed") +
  scale_y_continuous(limits = c(min(res), 1)) +
  labs(x = expression(mu[2]), y = "Coverage") +
  theme_book

ggsave("selected_mean.pdf", plot = p, width = 7, height = 5)
