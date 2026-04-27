source(file.path("Figures", "theme.R"))

ancillarity_plot_2 <- function() {
  x <- seq(-3, 3.5, length.out = 1000)
  
  mu <- 1.55 + 0.28*x
  mu_dash <- mu - 0.01
  
  dens_left <- dnorm(x, mean = -1.1, sd = 0.55)
  dens_right <- dnorm(x, mean = 1.7, sd = 0.42)
  
  dens_scale <- 0.55
  dens_left <- dens_scale*dens_left
  dens_right <- dens_scale*dens_right
  
  df_lines <- data.frame(
    x = x,
    mu = mu,
    mu_dash = mu_dash
  )
  
  df_dens <- data.frame(
    x = x,
    dens_left = dens_left,
    dens_right = dens_right
  )
  
  ggplot() +
    geom_line(
      data = df_lines,
      aes(x = x, y = mu),
      linewidth = 1,
      color = "black"
    ) +
    geom_line(
      data = df_lines,
      aes(x = x, y = mu_dash),
      linewidth = 1,
      color = "black",
      linetype = "dashed"
    ) +
    geom_line(
      data = df_dens,
      aes(x = x, y = dens_left),
      linewidth = 1,
      color = "black"
    ) +
    geom_line(
      data = df_dens,
      aes(x = x, y = dens_right),
      linewidth = 1,
      color = "black"
    ) +
    geom_hline(
      yintercept = 0,
      linewidth = 0.2,
      color = "grey70"
    ) +
    coord_cartesian(
      xlim = c(-3, 3.3),
      ylim = c(-0.05, 2.8),
      expand = FALSE
    ) +
    labs(
      x = "x",
      y = expression(mu(x))
    ) +
    theme_book
}

p_plot <- ancillarity_plot_2()

ggsave(file.path("Figures", "Chapter-3", "fig-3-2.pdf"), plot = p_plot, width = 5, height = 5)
