source(file.path("Figures", "theme.R"))

sigma_y <- 1/10

fisher_selected <- function(theta, gamma) {
  sigma_u <- sqrt((1 + gamma)/100)
  z <- theta/sigma_u
  
  p_sel <- pnorm(z)
  score_shift <- dnorm(z)/(p_sel*sigma_u)
  
  integrand <- function(y) {
    dens_y <- dnorm(y, mean = theta, sd = sigma_y)
    
    if (gamma == 0) {
      p_event_y <- as.numeric(y > 0)
    } else {
      p_event_y <- pnorm(y/sqrt(gamma/100))
    }
    
    score <- (y - theta)/sigma_y^2 - score_shift
    
    score^2*dens_y*p_event_y/p_sel
  }
  
  integrate(integrand, lower = -Inf, upper = Inf, rel.tol = 1e-8)$value
}

fisher_randomized <- function(gamma) {
  100*gamma/(1 + gamma)
}

gammas <- seq(0, 3, length.out = 300)
thetas <- c(-1, 0, 1)

df <- do.call(
  rbind,
  lapply(thetas, function(theta) {
    data.frame(
      gamma = gammas,
      I_selected = sapply(gammas, function(gamma) fisher_selected(theta, gamma)),
      I_randomized = fisher_randomized(gammas),
      theta = paste0("theta == ", theta)
    )
  })
)

p_plot <- ggplot(df, aes(x = gamma)) +
  geom_line(aes(y = I_selected), linewidth = 1) +
  geom_line(aes(y = I_randomized), linewidth = 1, linetype = "dashed") +
  facet_wrap(~ theta, nrow = 1, labeller = label_parsed) +
  coord_cartesian(ylim = c(0, 105)) +
  labs(x = expression(gamma), y = "FI") +
  theme_book

ggsave(file.path("Figures", "Chapter-5", "fig-5-4.pdf"), plot = p_plot, width = 10, height = 4)
