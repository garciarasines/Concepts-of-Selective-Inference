source(file.path("Figures", "theme.R"))

xlim <- c(0, 4.4)
ylim <- c(0, 3.8)

poly_df <- data.frame(
  x = c(1.85, 2.95, 3.75, 1.85),
  y = c(1.35, 2.45, 3.25, 3.25)
)

boundary_df <- rbind(poly_df, poly_df[1, ])

line_df <- data.frame(
  x = 1.05,
  y = 0.55,
  xend = 2.45,
  yend = 2.45
)

interval_df <- data.frame(
  x = 1.85,
  xend = 2.95,
  y = 2.45,
  yend = 2.45
)

tick_df <- data.frame(
  x = c(1.85, 2.95),
  xend = c(1.85, 2.95),
  y = c(2.33, 2.33),
  yend = c(2.57, 2.57)
)

guide_df <- data.frame(
  x = c(1.85, 2.95),
  xend = c(1.85, 2.95),
  y = c(0.55, 0.55),
  yend = c(2.45, 2.45)
)

eta_ty_df <- data.frame(
  x = 1.05,
  y = 0.55,
  xend = 2.45,
  yend = 0.55
)

z_df <- data.frame(
  x = 1.05,
  y = 0.55,
  xend = 1.05,
  yend = 2.75
)

eta_df <- data.frame(
  x = 1.05,
  y = 0.55,
  xend = 1.85,
  yend = 0.55
)

p_plot <- ggplot() +
  geom_polygon(
    data = poly_df,
    aes(x = x, y = y),
    fill = "grey88",
    color = NA
  ) +
  geom_path(
    data = boundary_df,
    aes(x = x, y = y),
    linewidth = 0.5,
    color = "grey60"
  ) +
  geom_segment(
    data = line_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.7,
    color = "black",
    arrow = arrow(length = grid::unit(0.16, "cm"), type = "closed")
  ) +
  geom_segment(
    data = interval_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 1,
    color = "black"
  ) +
  geom_segment(
    data = tick_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 1,
    color = "black"
  ) +
  geom_segment(
    data = guide_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.4,
    linetype = "dotted",
    color = "grey45"
  ) +
  geom_segment(
    data = eta_ty_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.55,
    linetype = "dashed",
    lineend = "round",
    color = "black",
    arrow = arrow(length = grid::unit(0.18, "cm"), type = "closed")
  ) +
  geom_segment(
    data = z_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.55,
    linetype = "dashed",
    lineend = "round",
    color = "black",
    arrow = arrow(length = grid::unit(0.18, "cm"), type = "closed")
  ) +
  geom_segment(
    data = eta_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.7,
    color = "black",
    arrow = arrow(length = grid::unit(0.16, "cm"), type = "closed")
  ) +
  annotate(
    "text",
    x = 2.83,
    y = 2.93,
    label = 'group("{", A*y <= b, "}")',
    parse = TRUE,
    size = 5.5
  ) +
  annotate(
    "text",
    x = 2.58,
    y = 2.26,
    label = "bold(y)",
    parse = TRUE,
    size = 5
  ) +
  annotate(
    "text",
    x = 0.88,
    y = 2.83,
    label = "z",
    parse = TRUE,
    size = 5
  ) +
  annotate(
    "text",
    x = 1.72,
    y = 0.74,
    label = "eta",
    parse = TRUE,
    size = 5
  ) +
  annotate(
    "text",
    x = 2.48,
    y = 0.74,
    label = "eta^T * y",
    parse = TRUE,
    size = 5
  ) +
  annotate(
    "text",
    x = 1.85,
    y = 0.25,
    label = "V^'-'~(z)",
    parse = TRUE,
    size = 5.5
  ) +
  annotate(
    "text",
    x = 2.95,
    y = 0.25,
    label = "V^'+'~(z)",
    parse = TRUE,
    size = 5.5
  ) +
  coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_void()

p_plot

ggsave(file.path("Figures", "Outputs", "fig-4-3.pdf"), plot = p_plot, width = 6, height = 5)
