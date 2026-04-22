library(ggplot2)

theme_book <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(color = "black", size = 11),
    legend.text = element_text(color = "black", size = 11),
    legend.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
  )
