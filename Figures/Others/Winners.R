library(ggplot2)
library(patchwork)
source(file.path("Figures", "theme.R"))

stability <- read.table("stabilityresBIG")
randomisation <- read.table("randomisationresBIGb")

df_stab <- data.frame(
  eta = stability[, 2],
  length_stability = stability[, 9],
  coverage_unadjusted = stability[, 8],
  posi = 7.4115
)

df_rand <- data.frame(
  gamma = randomisation[, 2],
  length_v = randomisation[, 6],
  length_cos = randomisation[, 13],
  agreement = randomisation[, 5],
  coverage_unadjusted = randomisation[, 18],
  posi = 7.4115
)

p_stab <- ggplot(df_stab, aes(x = eta)) +
  geom_line(aes(y = length_stability), linewidth = 0.9, color = "black") +
  geom_hline(yintercept = 7.4115, linewidth = 0.5, color = "grey45") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 12)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  labs(x = expression(eta), y = "Average interval length", title = "Stability") +
  theme_book +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

p_rand <- ggplot(df_rand, aes(x = gamma)) +
  geom_line(aes(y = length_v), linewidth = 0.9, color = "black") +
  geom_line(aes(y = length_cos), linewidth = 0.9, color = "grey55") +
  geom_hline(yintercept = 7.4115, linewidth = 0.5, color = "grey45") +
  coord_cartesian(xlim = c(0, 4), ylim = c(0, 35)) +
  scale_x_continuous(breaks = seq(0, 4, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  labs(x = expression(gamma), y = "Average interval length", title = "Randomisation") +
  theme_book +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

p_plot <- p_stab / p_rand +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )

ggsave(file.path("Figures", "Outputs", "Winners.pdf"), plot = p_plot, width = 4, height = 6)
