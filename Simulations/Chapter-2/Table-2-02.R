library(PoSI)
library(glmnet)
library(mvtnorm)

alpha <- 0.05
B <- 5e3
Nsim_posi <- 5e4
Nsim_unadjusted <- 1e4

p <- 10
n_values <- c(30, 1000)

set.seed(123)

make_design <- function(n, p) {
  Sigma <- matrix(0.5, nrow = p, ncol = p)
  diag(Sigma) <- 1
  
  L <- chol(Sigma)
  X <- t(t(L) %*% matrix(rnorm(p*n), nrow = p))
  
  X
}

make_beta_settings <- function(p) {
  list(
    list(
      name = "beta^(1)",
      beta = rep(0, p)
    ),
    list(
      name = "beta^(2)",
      beta = c(1, 1, -1, -1, rep(0, p - 4))
    )
  )
}

select_model <- function(X, y, rule) {
  if (rule == "Lasso") {
    fit <- cv.glmnet(X, y)
    s <- as.vector(coef(fit, s = "lambda.min")[-1] != 0)
  }
  
  if (rule == "Screen.") {
    fit <- lm(y ~ X)
    s <- summary(fit)[[4]][-1, 4] < 0.05
  }
  
  s
}

get_selected_quantities <- function(X, beta, s) {
  Xs <- X[, s, drop = FALSE]
  XtX_inv <- solve(t(Xs) %*% Xs)
  
  psi <- XtX_inv %*% t(Xs) %*% X %*% beta
  
  list(
    Xs = Xs,
    XtX_inv = XtX_inv,
    psi = psi
  )
}

get_unadjusted_constant <- function(XtX_inv) {
  d <- ncol(XtX_inv)
  
  U <- rmvnorm(
    Nsim_unadjusted,
    mean = rep(0, d),
    sigma = XtX_inv
  )
  
  se <- sqrt(diag(XtX_inv))
  
  quantile(
    apply(U, 1, function(u) max(abs(u) / se)),
    1 - alpha
  )
}

check_coverage <- function(psi, psi_hat, XtX_inv, K) {
  se <- sqrt(diag(XtX_inv))
  
  lower <- psi_hat - se*K
  upper <- psi_hat + se*K
  
  all((psi > lower) & (psi < upper))
}

get_average_ci_size <- function(XtX_inv, K) {
  se <- sqrt(diag(XtX_inv))
  mean(2*K*se)
}

simulate_setting <- function(X, beta, selection_rule, K_posi, K_scheffe) {
  cov_posi <- numeric(B)
  cov_scheffe <- numeric(B)
  cov_unadjusted <- numeric(B)
  
  size_posi <- numeric(B)
  size_scheffe <- numeric(B)
  size_unadjusted <- numeric(B)
  
  unadjusted_constants <- new.env(parent = emptyenv())
  
  pb <- txtProgressBar(min = 0, max = B, style = 3)
  
  for (b in seq_len(B)) {
    selected <- FALSE
    
    while (!selected) {
      y <- as.vector(X %*% beta + rnorm(nrow(X)))
      s <- select_model(X, y, selection_rule)
      selected <- sum(s) > 0
    }
    
    key <- paste(as.integer(s), collapse = "")
    
    q <- get_selected_quantities(X, beta, s)
    
    psi_hat <- q$XtX_inv %*% t(q$Xs) %*% y
    
    cov_posi[b] <- check_coverage(
      psi = q$psi,
      psi_hat = psi_hat,
      XtX_inv = q$XtX_inv,
      K = K_posi
    )
    
    cov_scheffe[b] <- check_coverage(
      psi = q$psi,
      psi_hat = psi_hat,
      XtX_inv = q$XtX_inv,
      K = K_scheffe
    )
    
    if (!exists(key, envir = unadjusted_constants, inherits = FALSE)) {
      assign(
        key,
        get_unadjusted_constant(q$XtX_inv),
        envir = unadjusted_constants
      )
    }
    
    K_unadjusted <- get(key, envir = unadjusted_constants)
    
    cov_unadjusted[b] <- check_coverage(
      psi = q$psi,
      psi_hat = psi_hat,
      XtX_inv = q$XtX_inv,
      K = K_unadjusted
    )
    
    size_posi[b] <- get_average_ci_size(
      XtX_inv = q$XtX_inv,
      K = K_posi
    )
    
    size_scheffe[b] <- get_average_ci_size(
      XtX_inv = q$XtX_inv,
      K = K_scheffe
    )
    
    size_unadjusted[b] <- get_average_ci_size(
      XtX_inv = q$XtX_inv,
      K = K_unadjusted
    )
    
    setTxtProgressBar(pb, b)
  }
  
  close(pb)
  
  data.frame(
    coverage_posi = mean(cov_posi),
    coverage_scheffe = mean(cov_scheffe),
    coverage_unadjusted = mean(cov_unadjusted),
    size_posi = mean(size_posi),
    size_scheffe = mean(size_scheffe),
    size_unadjusted = mean(size_unadjusted)
  )
}

run_simulation <- function() {
  beta_settings <- make_beta_settings(p)
  selection_rules <- c("Lasso", "Screen.")
  
  results <- list()
  counter <- 1
  
  for (n in n_values) {
    cat("\nComputing constants for p =", p, "and n =", n, "\n")
    
    X <- make_design(n, p)
    
    POSI <- PoSI(X, Nsim = Nsim_posi, verbose = 0)
    K_posi <- summary(POSI, df.err = NULL)[1, 1]
    K_scheffe <- summary(POSI, df.err = NULL)[1, 3]
    
    for (beta_setting in beta_settings) {
      for (selection_rule in selection_rules) {
        cat(
          "\nRunning:",
          "p =", p,
          ", n =", n,
          ", beta =", beta_setting$name,
          ", selection =", selection_rule,
          "\n"
        )
        
        out <- simulate_setting(
          X = X,
          beta = beta_setting$beta,
          selection_rule = selection_rule,
          K_posi = K_posi,
          K_scheffe = K_scheffe
        )
        
        out$p <- p
        out$n <- n
        out$beta <- beta_setting$name
        out$selection <- selection_rule
        
        results[[counter]] <- out
        counter <- counter + 1
      }
    }
  }
  
  results <- do.call(rbind, results)
  
  results <- results[, c(
    "p",
    "n",
    "beta",
    "selection",
    "coverage_posi",
    "coverage_scheffe",
    "coverage_unadjusted",
    "size_posi",
    "size_scheffe",
    "size_unadjusted"
  )]
  
  results_print <- results
  
  results_print[, c(
    "coverage_posi",
    "coverage_scheffe",
    "coverage_unadjusted"
  )] <- round(
    100*results_print[, c(
      "coverage_posi",
      "coverage_scheffe",
      "coverage_unadjusted"
    )],
    1
  )
  
  results_print[, c(
    "size_posi",
    "size_scheffe",
    "size_unadjusted"
  )] <- round(
    results_print[, c(
      "size_posi",
      "size_scheffe",
      "size_unadjusted"
    )],
    3
  )
  
  cat("\nFull simulation results\n")
  cat("Coverages are percentages; sizes are average CI lengths.\n")
  print(results_print, row.names = FALSE)
  
  invisible(results)
}

results <- run_simulation()
