library(locfdr)
source(file.path("Figures", "theme.R"))
source(file.path("Datasets", "Prostate.R"))

control <- as.matrix(prostmat[, 1:50])
cancer <- as.matrix(prostmat[, 51:102])

n_control <- ncol(control)
n_cancer <- ncol(cancer)

mean_control <- rowMeans(control)
mean_cancer <- rowMeans(cancer)

var_control <- apply(control, 1, var)
var_cancer <- apply(cancer, 1, var)

sp2 <- ((n_control - 1)*var_control + (n_cancer - 1)*var_cancer)/(n_control + n_cancer - 2)

t_stat <- (mean_cancer - mean_control)/sqrt(sp2*(1/n_control + 1/n_cancer))
z <- qnorm(pt(t_stat, df = n_control + n_cancer - 2))

out <- locfdr(z, bre = 120, df = 7, pct = 0, nulltype = 1, plot = 0)

df_plot <- data.frame(
  x = out$mat[, "x"],
  counts = out$mat[, "counts"],
  f = out$mat[, "f"],
  f0 = out$mat[, "f0"]
)

bin_width <- mean(diff(df_plot$x))

p_plot <- ggplot(df_plot, aes(x = x)) +
  geom_col(aes(y = counts), width = bin_width, fill = "grey85", color = "black", linewidth = 0.2) +
  geom_line(aes(y = f), linewidth = 1, color = "black") +
  geom_line(aes(y = f0), linewidth = 1, color = "black", linetype = "dashed") +
  labs(x = "z", y = "Counts") +
  theme_book

ggsave(file.path("Figures", "Chapter-6", "fig-6-6.pdf"), plot = p_plot, width = 5, height = 5)
