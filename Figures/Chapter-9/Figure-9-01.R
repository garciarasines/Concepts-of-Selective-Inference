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

stab_left_min <- 0
stab_left_max <- 12
stab_right_min <- 0.75
stab_right_max <- 1

stab_to_left <- function(z) {
  stab_left_min + (z - stab_right_min)/(stab_right_max - stab_right_min)*(stab_left_max - stab_left_min)
}

stab_to_right <- function(z) {
  stab_right_min + (z - stab_left_min)/(stab_left_max - stab_left_min)*(stab_right_max - stab_right_min)
}

rand_left_min <- 0
rand_left_max <- 35
rand_right_min <- 0
rand_right_max <- 1

rand_to_left <- function(z) {
  rand_left_min + (z - rand_right_min)/(rand_right_max - rand_right_min)*(rand_left_max - rand_left_min)
}

rand_to_right <- function(z) {
  rand_right_min + (z - rand_left_min)/(rand_left_max - rand_left_min)*(rand_right_max - rand_left_min)
}

p_stab <- ggplot(df_stab, aes(x = eta)) +
  geom_line(aes(y = length_stability), linewidth = 0.9, color = "black") +
  geom_hline(yintercept = 7.4115, linewidth = 0.5, color = "grey45") +
  geom_line(aes(y = stab_to_left(coverage_unadjusted)), linewidth = 0.7, linetype = "11", color = "grey40") +
  coord_cartesian(xlim = c(0, 10), ylim = c(stab_left_min, stab_left_max)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  scale_y_continuous(
    name = "Average interval length",
    breaks = seq(0, 12, by = 2),
    sec.axis = sec_axis(
      trans = ~ stab_to_right(.),
      name = "Unadj. cov.",
      breaks = seq(0.75, 1, by = 0.05)
    )
  ) +
  labs(x = expression(eta), title = "Stability") +
  theme_book +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y.right = element_text(color = "grey40"),
    axis.text.y.right = element_text(color = "grey40")
  )

p_rand <- ggplot(df_rand, aes(x = gamma)) +
  geom_line(aes(y = length_v), linewidth = 0.9, color = "black") +
  geom_line(aes(y = length_cos), linewidth = 0.9, color = "grey55") +
  geom_hline(yintercept = 7.4115, linewidth = 0.5, color = "grey45") +
  geom_line(aes(y = rand_to_left(agreement)), linewidth = 0.7, linetype = "11", color = "grey40") +
  coord_cartesian(xlim = c(0, 4), ylim = c(rand_left_min, rand_left_max)) +
  scale_x_continuous(breaks = seq(0, 4, by = 0.5)) +
  scale_y_continuous(
    name = "Average interval length",
    breaks = seq(0, 35, by = 5),
    sec.axis = sec_axis(
      trans = ~ rand_to_right(.),
      name = "Agree prob.",
      breaks = seq(0, 1, by = 0.2)
    )
  ) +
  labs(x = expression(gamma), title = "Randomisation") +
  theme_book +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y.right = element_text(color = "grey40"),
    axis.text.y.right = element_text(color = "grey40")
  )

p_plot <- p_stab / p_rand +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )

ggsave(file.path("Figures", "Outputs", "Winners.pdf"), plot = p_plot, width = 4, height = 6)
