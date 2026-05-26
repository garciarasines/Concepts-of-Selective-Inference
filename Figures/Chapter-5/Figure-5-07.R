library(mvtnorm)
library(patchwork)
source(file.path("Figures", "theme.R"))

set.seed(123)

alpha <- 0.10
q1 <- alpha/2
q2 <- 1 - alpha/2
B <- 5e4
n_y_grid <- 101

Sigma_YU <- matrix(c(1, 1, 1, 2), nrow = 2)

L_V <- 2*qnorm(1 - alpha/2)*sqrt(2)

p_selection <- function(theta, t, type) {
  if (type == "right") {
    1 - pnorm(t, mean = theta, sd = sqrt(2))
  } else if (type == "middle") {
    pnorm(t, mean = theta, sd = sqrt(2)) - pnorm(-t, mean = theta, sd = sqrt(2))
  } else if (type == "tails") {
    pnorm(-t, mean = theta, sd = sqrt(2)) + 1 - pnorm(t, mean = theta, sd = sqrt(2))
  }
}

prob_y_leq_and_selection <- function(y, theta, t, type) {
  mean_vec <- c(theta, theta)
  
  if (type == "right") {
    as.numeric(
      pmvnorm(
        lower = c(-Inf, t),
        upper = c(y, Inf),
        mean = mean_vec,
        sigma = Sigma_YU
      )
    )
  } else if (type == "middle") {
    as.numeric(
      pmvnorm(
        lower = c(-Inf, -t),
        upper = c(y, t),
        mean = mean_vec,
        sigma = Sigma_YU
      )
    )
  } else if (type == "tails") {
    left <- as.numeric(
      pmvnorm(
        lower = c(-Inf, -Inf),
        upper = c(y, -t),
        mean = mean_vec,
        sigma = Sigma_YU
      )
    )
    
    right <- as.numeric(
      pmvnorm(
        lower = c(-Inf, t),
        upper = c(y, Inf),
        mean = mean_vec,
        sigma = Sigma_YU
      )
    )
    
    left + right
  }
}

F_carve <- function(y, theta, t, type) {
  p_sel <- p_selection(theta, t, type)
  
  if (!is.finite(p_sel) || p_sel <= 0) {
    return(NA_real_)
  }
  
  prob_y_leq_and_selection(y, theta, t, type)/p_sel
}

root_for_q <- function(y, t, type, q) {
  f <- function(theta) {
    val <- F_carve(y, theta, t, type) - q
    
    if (!is.finite(val)) {
      return(NA_real_)
    }
    
    val
  }
  
  lower <- y - 8
  upper <- y + 8
  
  f_lower <- f(lower)
  f_upper <- f(upper)
  
  count <- 0
  
  while ((!is.finite(f_lower) || f_lower < 0) && count < 50) {
    lower <- lower - 2
    f_lower <- f(lower)
    count <- count + 1
  }
  
  count <- 0
  
  while ((!is.finite(f_upper) || f_upper > 0) && count < 50) {
    upper <- upper + 2
    f_upper <- f(upper)
    count <- count + 1
  }
  
  if (!is.finite(f_lower) || !is.finite(f_upper)) {
    return(NA_real_)
  }
  
  if (f_lower*f_upper > 0) {
    return(NA_real_)
  }
  
  uniroot(f, interval = c(lower, upper))$root
}

carving_length <- function(y, t, type) {
  lower <- root_for_q(y, t, type, q2)
  upper <- root_for_q(y, t, type, q1)
  
  if (!is.finite(lower) || !is.finite(upper)) {
    return(NA_real_)
  }
  
  upper - lower
}

sample_selected_y <- function(B, mu, t, type) {
  if (type == "right") {
    a <- pnorm(t, mean = mu, sd = sqrt(2))
    u <- qnorm(runif(B, a, 1), mean = mu, sd = sqrt(2))
  } else if (type == "middle") {
    a <- pnorm(-t, mean = mu, sd = sqrt(2))
    b <- pnorm(t, mean = mu, sd = sqrt(2))
    u <- qnorm(runif(B, a, b), mean = mu, sd = sqrt(2))
  } else if (type == "tails") {
    p_left <- pnorm(-t, mean = mu, sd = sqrt(2))
    p_right <- 1 - pnorm(t, mean = mu, sd = sqrt(2))
    side <- runif(B) < p_left/(p_left + p_right)
    
    u <- numeric(B)
    
    n_left <- sum(side)
    n_right <- B - n_left
    
    if (n_left > 0) {
      u[side] <- qnorm(runif(n_left, 0, p_left), mean = mu, sd = sqrt(2))
    }
    
    if (n_right > 0) {
      u[!side] <- qnorm(runif(n_right, 1 - p_right, 1), mean = mu, sd = sqrt(2))
    }
  }
  
  rnorm(B, mean = mu + 0.5*(u - mu), sd = sqrt(0.5))
}

compute_panel <- function(t_grid, mu, type, panel_label) {
  out <- data.frame(
    t = t_grid,
    efficiency = NA_real_,
    selection = NA_real_,
    panel = panel_label
  )
  
  pb <- txtProgressBar(min = 0, max = length(t_grid), style = 3)
  
  for (i in seq_along(t_grid)) {
    t <- t_grid[i]
    
    y_sim <- sample_selected_y(B, mu, t, type)
    
    y_grid <- seq(
      quantile(y_sim, 0.002),
      quantile(y_sim, 0.998),
      length.out = n_y_grid
    )
    
    L_C_grid <- sapply(y_grid, carving_length, t = t, type = type)
    keep <- is.finite(L_C_grid)
    
    if (sum(keep) >= 2) {
      L_C_fun <- approxfun(y_grid[keep], L_C_grid[keep], rule = 2)
      L_C <- L_C_fun(y_sim)
      out$efficiency[i] <- mean((L_V - L_C)/L_C, na.rm = TRUE)
    }
    
    out$selection[i] <- p_selection(mu, t, type)
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  out
}

plot_panel <- function(df, xlim, ylim_eff, panel_label) {
  df_long <- rbind(
    data.frame(t = df$t, value = df$efficiency, method = "Efficiency loss"),
    data.frame(t = df$t, value = df$selection*ylim_eff, method = "Selection prob")
  )
  
  ggplot(df_long, aes(x = t, y = value, color = method, linetype = method)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    scale_color_manual(values = c("Efficiency loss" = "black", "Selection prob" = "grey60")) +
    scale_linetype_manual(values = c("Efficiency loss" = "solid", "Selection prob" = "longdash")) +
    scale_y_continuous(
      limits = c(0, ylim_eff),
      breaks = seq(0, ylim_eff, length.out = 6),
      name = "Efficiency loss",
      sec.axis = sec_axis(~./ylim_eff, name = "Selection prob", breaks = seq(0, 1, by = 0.2))
    ) +
    coord_cartesian(xlim = xlim) +
    labs(x = "t", title = panel_label, color = NULL, linetype = NULL) +
    theme_book +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
}

t_a <- seq(-4, 5.2, length.out = 45)
t_b <- seq(0.05, 4, length.out = 45)
t_c <- seq(0, 4, length.out = 45)
t_d <- seq(0, 4, length.out = 45)

res_a <- compute_panel(t_a, mu = 0, type = "right", panel_label = "(a)")
res_b <- compute_panel(t_b, mu = 0, type = "middle", panel_label = "(b)")
res_c <- compute_panel(t_c, mu = 0, type = "tails", panel_label = "(c)")
res_d <- compute_panel(t_d, mu = 2, type = "tails", panel_label = "(d)")

p_a <- plot_panel(res_a, xlim = c(-4, 5.2), ylim_eff = 0.5, panel_label = "(a)")
p_b <- plot_panel(res_b, xlim = c(0, 4), ylim_eff = 0.5, panel_label = "(b)")
p_c <- plot_panel(res_c, xlim = c(0, 4), ylim_eff = 1, panel_label = "(c)")
p_d <- plot_panel(res_d, xlim = c(0, 4), ylim_eff = 1, panel_label = "(d)")

p_plot <- (p_a + p_b) / (p_c + p_d) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(file.path("Figures", "Outputs", "fig-5-07.pdf"), plot = p_plot, width = 6, height = 4)
