library(patchwork)
source(file.path("Figures", "theme.R"))

set.seed(123)

B <- 2e5
pi0 <- 0.9
tau <- 3
t_sel <- 2
x_cut <- 3.58
cdf_scale <- 1.5

theta <- ifelse(runif(B) < pi0, 0, rnorm(B, mean = 0, sd = tau))
y <- rnorm(B, mean = theta, sd = 1)

abs_y_sel <- abs(y[abs(y) > t_sel])
abs_y_sel_plot <- abs_y_sel[abs_y_sel <= 5]

y_grid <- seq(2, 5, length.out = 600)

size_safab <- function(y) {
  out <- numeric(length(y))
  
  left <- y <= 3.15
  mid <- y > 3.15 & y <= 3.85
  right <- y > 3.85
  
  out[left] <- 1.28 + 1.85*(1 - exp(-4*(y[left] - 2))) + (y[left] - 2)
  out[mid] <- 3.78 + 0.50*exp(-8*(y[mid] - 3.15)) - 0.07*(y[mid] - 3.15)
  out[right] <- 3.74 + 0.24*(y[right] - 3.85)
  
  out
}

size_saumau <- function(y) {
  1.23 + 3.30*(1 - exp(-6*(y - 2))) - 1.22*plogis(3.2*(y - 3.35))
}

df_top <- rbind(
  data.frame(y = y_grid, size = size_safab(y_grid), method = "saFAB"),
  data.frame(y = y_grid, size = size_saumau(y_grid), method = "saUMAU")
)

Fn <- ecdf(abs_y_sel)
df_cdf <- data.frame(
  y = y_grid,
  cdf = cdf_scale*Fn(y_grid)
)

p_top <- ggplot(df_top, aes(x = y, y = size, color = method)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = x_cut, linewidth = 0.6, linetype = "dashed") +
  scale_color_manual(values = c("saFAB" = "black", "saUMAU" = "grey65")) +
  coord_cartesian(xlim = c(2, 5), ylim = c(1.1, 4.5)) +
  labs(x = NULL, y = "Size of confidence set", color = "method") +
  guides(color = guide_legend(nrow = 1)) +
  theme_book +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(face = "plain"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(5.5, 5.5, 2, 5.5)
  )

p_bottom <- ggplot() +
  geom_histogram(
    data = data.frame(y = abs_y_sel_plot),
    aes(x = y, y = after_stat(density)),
    binwidth = 0.10,
    boundary = 2,
    fill = "grey95",
    color = "grey35",
    linewidth = 0.4
  ) +
  geom_line(
    data = df_cdf,
    aes(x = y, y = cdf),
    linewidth = 1,
    color = "black"
  ) +
  geom_vline(xintercept = x_cut, linewidth = 0.6, linetype = "dashed") +
  coord_cartesian(xlim = c(2, 5), ylim = c(0, cdf_scale)) +
  scale_x_continuous(breaks = 2:5) +
  scale_y_continuous(
    name = "density",
    sec.axis = sec_axis(~./cdf_scale, name = "Cumulative density")
  ) +
  labs(x = "|y|", y = "Density") +
  theme_book +
  theme(
    legend.position = "none",
    plot.margin = margin(2, 5.5, 5.5, 5.5)
  )

p_plot <- p_top / p_bottom + plot_layout(heights = c(1, 1))

ggsave(file.path("Figures", "Outputs", "fig-6-12.pdf"), plot = p_plot, width = 5, height = 5)
