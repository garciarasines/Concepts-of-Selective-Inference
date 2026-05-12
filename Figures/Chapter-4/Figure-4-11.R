library(palmerpenguins)
library(fastcluster)
source(file.path("Figures", "theme.R"))

penguins_complete <- palmerpenguins::penguins
penguins_complete <- penguins_complete[complete.cases(penguins_complete), ]

dat <- penguins_complete[
  penguins_complete$sex == "female" & penguins_complete$year != 2009,
  c("species", "bill_length_mm", "flipper_length_mm")
]

X <- as.matrix(dat[, c("bill_length_mm", "flipper_length_mm")])

K <- 3

hc <- fastcluster::hclust(dist(X)^2, method = "complete")
dat$cluster <- factor(cutree(hc, k = K))

print(nrow(dat))
print(table(dat$species, dat$cluster))

p_plot <- ggplot(
  dat,
  aes(
    x = flipper_length_mm,
    y = bill_length_mm,
    fill = cluster,
    shape = species
  )
) +
  geom_point(
    size = 3.2,
    colour = "black",
    stroke = 0.35
  ) +
  scale_fill_grey(
    name = "Clusters",
    start = 0.25,
    end = 0.8,
    guide = guide_legend(
      ncol = 1,
      override.aes = list(shape = 21)
    )
  ) +
  scale_shape_manual(
    name = "Species",
    values = c(
      "Adelie" = 21,
      "Chinstrap" = 24,
      "Gentoo" = 22
    ),
    guide = guide_legend(
      override.aes = list(fill = "black")
    )
  ) +
  labs(
    x = "Flipper length (mm)",
    y = "Bill length (mm)"
  ) +
  theme_book +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(file.path("Figures", "Outputs", "fig-4-12.pdf"), plot = p_plot, width = 7, height = 4)
