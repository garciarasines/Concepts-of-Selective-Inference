library(MASS)
source(file.path("Figures", "theme.R"))

stepwise_p_values <- function(n, p, B = 1e4) {
  
  p_values <- vector("list", B)
  
  pb <- txtProgressBar(min = 0, max = B, style = 3)
  
  for (b in seq_len(B)) {
    X <- data.frame(matrix(rnorm(n*p), ncol = p))
    y <- rnorm(n)
    
    min_model <- lm(y ~ 1, data = X)
    max_model <- formula(lm(y ~ ., data = X))
    
    fit_step <- stepAIC(min_model, direction = "forward", scope = max_model, trace = FALSE)
    
    p_values[[b]] <- summary(fit_step)$coefficients[, 4]
    
    setTxtProgressBar(pb, b)
  }
  
  close(pb)
  
  unlist(p_values)
}

B <- 1e4
n <- 80
p <- 20

set.seed(123)
results <- stepwise_p_values(n, p, B)

df <- data.frame(p_value = results)

p_plot <- ggplot(df, aes(x = p_value)) +
  stat_ecdf(geom = "step", linewidth = 0.4, color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = "p-value",
    y = "Empirical CDF"
  ) +
  theme_book

ggsave(file.path("Figures", "Outputs", "fig-1-2.pdf"), plot = p_plot, width = 5, height = 5)
