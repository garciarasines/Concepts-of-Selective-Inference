source(file.path("Figures", "theme.R"))

ancillarity_plot_a <- function() {
  x <- seq(-3, 3.5, length.out = 1000)
  
  mu <- 0.80 + 0.18*x + 0.18*x^2
  
  dens_left <- dnorm(x, mean = -1.1, sd = 0.55)
  dens_right <- dnorm(x, mean = 1.7, sd = 0.42)
  
  dens_scale <- 0.55
  dens_left <- dens_scale*dens_left
  dens_right <- dens_scale*dens_right
  
  df_curve <- data.frame(x = x, mu = mu)
  df_dens <- data.frame(x = x, dens_left = dens_left, dens_right = dens_right)
  
  x1 <- -1.35
  x2 <- 1.70
  
  y1 <- 0.80 + 0.18*x1 + 0.18*x1^2
  y2 <- 0.80 + 0.18*x2 + 0.18*x2^2
  
  slope1 <- 0.18 + 2*0.18*x1
  slope2 <- 0.18 + 2*0.18*x2
  
  tangent <- function(x, x0, y0, slope) {
    y0 + slope*(x - x0)
  }
  
  df_tangent_left <- data.frame(
    x = seq(-2.65, 0.35, length.out = 200)
  )
  df_tangent_left$y <- tangent(df_tangent_left$x, x1, y1, slope1)
  
  df_tangent_right <- data.frame(
    x = seq(0.55, 2.85, length.out = 200)
  )
  df_tangent_right$y <- tangent(df_tangent_right$x, x2, y2, slope2)
  
  ggplot() +
    geom_line(
      data = df_curve,
      aes(x = x, y = mu),
      linewidth = 1,
      color = "black"
    ) +
    geom_line(
      data = df_tangent_left,
      aes(x = x, y = y),
      linewidth = 1,
      color = "black",
      linetype = "dashed"
    ) +
    geom_line(
      data = df_tangent_right,
      aes(x = x, y = y),
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
      ylim = c(-0.05, 3.1),
      expand = FALSE
    ) +
    labs(
      x = "x",
      y = expression(mu(x))
    ) +
    theme_book
}

ancillarity_plot_b <- function() {
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

p_plot_a <- ancillarity_plot_a()
p_plot_b <- ancillarity_plot_b()

ggsave(file.path("Figures", "Outputs", "fig-3-1-a.pdf"), plot = p_plot_a, width = 5, height = 5)
ggsave(file.path("Figures", "Outputs", "fig-3-1-b.pdf"), plot = p_plot_b, width = 5, height = 5)
