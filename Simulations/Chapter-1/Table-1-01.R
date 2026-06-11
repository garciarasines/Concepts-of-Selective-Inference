alpha <- 0.05
B <- 200
N <- 250000

set.seed(123)

simulate_mu <- function(mu, alpha, B, N) {
  fcp_bh <- numeric(N)
  fcp_unad <- numeric(N)
  fcp_bonf <- numeric(N)
  fcp_naive <- numeric(N)
  
  sel_bh <- numeric(N)
  sel_unad <- numeric(N)
  sel_bonf <- numeric(N)
  sel_naive <- numeric(N)
  
  q_unad <- qnorm(1 - alpha/2)
  q_bonf <- qnorm(1 - alpha/(2*B))
  ranks <- seq_len(B)
  
  for (j in seq_len(N)) {
    z <- rnorm(B, mean = mu)
    pvals <- 2*(1 - pnorm(abs(z)))
    
    ord <- order(pvals)
    z <- z[ord]
    pvals <- pvals[ord]
    
    ind_bh <- pvals < ranks*alpha/B
    ind_unad <- abs(z) > q_unad
    ind_bonf <- abs(z) > q_bonf
    ind_naive <- abs(z) > q_unad
    
    select_bh <- sum(ind_bh)
    select_unad <- sum(ind_unad)
    select_bonf <- sum(ind_bonf)
    select_naive <- sum(ind_naive)
    
    sel_bh[j] <- select_bh
    sel_unad[j] <- select_unad
    sel_bonf[j] <- select_bonf
    sel_naive[j] <- select_naive
    
    q_bh_sel <- qnorm(1 - select_bh*alpha/(2*B))
    q_unad_sel <- qnorm(1 - select_unad*alpha/(2*B))
    q_bonf_sel <- qnorm(1 - select_bonf*alpha/(2*B))
    
    fcp_bh[j] <- sum((abs(z - mu) > q_bh_sel)*ind_bh)/max(select_bh, 1)
    fcp_unad[j] <- sum((abs(z - mu) > q_unad_sel)*ind_unad)/max(select_unad, 1)
    fcp_bonf[j] <- sum((abs(z - mu) > q_bonf_sel)*ind_bonf)/max(select_bonf, 1)
    fcp_naive[j] <- sum((abs(z - mu) > q_unad)*ind_naive)/max(select_naive, 1)
  }
  
  data.frame(
    mu = mu,
    BH = mean(fcp_bh),
    Unadjusted = mean(fcp_unad),
    Bonferroni = mean(fcp_bonf),
    Naive = mean(fcp_naive),
    Selected_BH = mean(sel_bh),
    Selected_unadjusted = mean(sel_unad),
    Selected_Bonferroni = mean(sel_bonf),
    Selected_naive = mean(sel_naive)
  )
}

mu_values <- 0:6

pb <- txtProgressBar(min = 0, max = length(mu_values), style = 3)

results <- do.call(
  rbind,
  lapply(seq_along(mu_values), function(i) {
    out <- simulate_mu(mu = mu_values[i], alpha = alpha, B = B, N = N)
    setTxtProgressBar(pb, i)
    out
  })
)

close(pb)

fcp_table <- results[, c("mu", "BH", "Unadjusted", "Bonferroni", "Naive")]

selection_table <- results[, c(
  "mu",
  "Selected_BH",
  "Selected_unadjusted",
  "Selected_Bonferroni",
  "Selected_naive"
)]

names(selection_table) <- c("mu", "BH", "Unadjusted", "Bonferroni", "Naive")

cat("\nFalse coverage proportion among selected intervals:\n")
print(round(fcp_table, 4), row.names = FALSE)

cat("\nAverage number of selected coordinates:\n")
print(round(selection_table, 3), row.names = FALSE)
