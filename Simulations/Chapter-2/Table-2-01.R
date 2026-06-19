alpha <- 0.05
B <- 1e6
p_values <- c(2, 5, 10, 25, 50, 100)

set.seed(123)

simulate_winner <- function(mu) {
  p <- length(mu)
  
  K_unadj <- qnorm(1 - alpha/2)
  K_posi <- qnorm((1 + (1 - alpha)^(1/p))/2)
  
  Y <- matrix(rnorm(B*p, mean = rep(mu, each = B)), nrow = B, ncol = p)
  
  winner <- max.col(Y, ties.method = "first")
  y_winner <- Y[cbind(seq_len(B), winner)]
  mu_winner <- mu[winner]
  
  cover_unadj <- abs(y_winner - mu_winner) <= K_unadj
  cover_posi <- abs(y_winner - mu_winner) <= K_posi
  
  data.frame(
    coverage_unadjusted = mean(cover_unadj),
    coverage_posi = mean(cover_posi),
    size_unadjusted = 2*K_unadj,
    size_posi = 2*K_posi
  )
}

settings_null <- lapply(p_values, function(p) {
  list(name = paste0("p=", p), mu = rep(0, p))
})

mu_gaussian <- lapply(p_values, function(p) {
  rnorm(p)
})

settings_gaussian <- lapply(seq_along(p_values), function(j) {
  list(name = paste0("p=", p_values[j]), mu = mu_gaussian[[j]])
})

run_simulation <- function(settings, label) {
  pb <- txtProgressBar(min = 0, max = length(settings), style = 3)
  
  results <- do.call(
    rbind,
    lapply(seq_along(settings), function(j) {
      out <- simulate_winner(settings[[j]]$mu)
      out$setting <- settings[[j]]$name
      setTxtProgressBar(pb, j)
      out
    })
  )
  
  close(pb)
  
  results <- results[, c(
    "setting",
    "coverage_unadjusted",
    "coverage_posi",
    "size_unadjusted",
    "size_posi"
  )]
  
  results_print <- results
  results_print[, -1] <- round(results_print[, -1], 3)
  
  cat("\n", label, "\n", sep = "")
  print(results_print, row.names = FALSE)
  
  invisible(results)
}

results_null <- run_simulation(
  settings_null,
  "Simulation 1: mu = 0 for all p"
)

results_gaussian <- run_simulation(
  settings_gaussian,
  "Simulation 2: mu fixed as one N(0, I_p) realization for each p"
)
