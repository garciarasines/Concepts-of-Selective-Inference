source(file.path("Figures", "theme.R"))

lambda <- 1

X <- cbind(c(1, 1)/sqrt(2), c(-1, 0), c(0, 1))

target_M <- c(1, 3)

xlim <- c(-3.2, 3.2)
ylim <- c(-3.2, 3.2)

projector <- function(X_M, n) {
  if (ncol(X_M) == 0) {
    return(matrix(0, nrow = n, ncol = n))
  }
  
  X_M%*%solve(t(X_M)%*%X_M)%*%t(X_M)
}

pseudo_inverse <- function(X_M) {
  solve(t(X_M)%*%X_M)%*%t(X_M)
}

pseudo_inverse_t <- function(X_M) {
  X_M%*%solve(t(X_M)%*%X_M)
}

event_matrices <- function(M, s, X, lambda) {
  p <- ncol(X)
  n <- nrow(X)
  
  if (length(M) == 0) {
    A <- rbind(t(X)/lambda, -t(X)/lambda)
    b <- rep(1, 2*p)
    
    return(list(A = A, b = b))
  }
  
  M_comp <- setdiff(seq_len(p), M)
  
  X_M <- X[, M, drop = FALSE]
  X_notM <- X[, M_comp, drop = FALSE]
  
  P_M <- projector(X_M, n)
  X_M_dag <- pseudo_inverse(X_M)
  Xt_M_dag <- pseudo_inverse_t(X_M)
  
  if (length(M_comp) > 0) {
    A_1 <- t(X_notM)%*%(diag(n) - P_M)/lambda
    A_2 <- -A_1
    
    b_1 <- rep(1, length(M_comp)) - t(X_notM)%*%Xt_M_dag%*%s
    b_2 <- rep(1, length(M_comp)) + t(X_notM)%*%Xt_M_dag%*%s
  } else {
    A_1 <- matrix(numeric(0), nrow = 0, ncol = n)
    A_2 <- matrix(numeric(0), nrow = 0, ncol = n)
    
    b_1 <- numeric(0)
    b_2 <- numeric(0)
  }
  
  A_3 <- -diag(s, nrow = length(M))%*%X_M_dag
  b_3 <- -lambda*diag(s, nrow = length(M))%*%solve(t(X_M)%*%X_M)%*%s
  
  A <- rbind(A_1, A_2, A_3)
  b <- c(b_1, b_2, b_3)
  
  list(A = A, b = as.numeric(b))
}

clip_polytope <- function(A, b, xlim, ylim, tol = 1e-9) {
  pts <- matrix(numeric(0), ncol = 2)
  
  corners <- rbind(
    c(xlim[1], ylim[1]),
    c(xlim[1], ylim[2]),
    c(xlim[2], ylim[1]),
    c(xlim[2], ylim[2])
  )
  
  pts <- rbind(pts, corners)
  
  if (nrow(A) >= 2) {
    for (i in 1:(nrow(A) - 1)) {
      for (j in (i + 1):nrow(A)) {
        A_ij <- rbind(A[i, ], A[j, ])
        
        if (abs(det(A_ij)) > tol) {
          pts <- rbind(pts, solve(A_ij, c(b[i], b[j])))
        }
      }
    }
  }
  
  for (i in seq_len(nrow(A))) {
    a1 <- A[i, 1]
    a2 <- A[i, 2]
    bi <- b[i]
    
    for (x0 in xlim) {
      if (abs(a2) > tol) {
        pts <- rbind(pts, c(x0, (bi - a1*x0)/a2))
      }
    }
    
    for (y0 in ylim) {
      if (abs(a1) > tol) {
        pts <- rbind(pts, c((bi - a2*y0)/a1, y0))
      }
    }
  }
  
  pts <- unique(round(pts, 10))
  
  keep <- apply(
    pts,
    1,
    function(z) {
      all(A%*%z <= b + 1e-8) &&
        z[1] >= xlim[1] - 1e-8 &&
        z[1] <= xlim[2] + 1e-8 &&
        z[2] >= ylim[1] - 1e-8 &&
        z[2] <= ylim[2] + 1e-8
    }
  )
  
  pts <- pts[keep, , drop = FALSE]
  
  if (nrow(pts) < 3) {
    return(NULL)
  }
  
  center <- colMeans(pts)
  ord <- order(atan2(pts[, 2] - center[2], pts[, 1] - center[1]))
  pts <- pts[ord, , drop = FALSE]
  
  area <- 0.5*abs(sum(
    pts[, 1]*c(pts[-1, 2], pts[1, 2]) -
      pts[, 2]*c(pts[-1, 1], pts[1, 1])
  ))
  
  if (area < 1e-6) {
    return(NULL)
  }
  
  data.frame(x = pts[, 1], y = pts[, 2])
}

event_name <- function(M, s) {
  if (length(M) == 0) {
    return("empty")
  }
  
  paste0(
    "M=",
    paste(M, collapse = ""),
    "_s=",
    paste(ifelse(s > 0, "+", "-"), collapse = "")
  )
}

sign_label <- function(s) {
  paste0("s = (", paste(ifelse(s > 0, "+", "-"), collapse = ", "), ")")
}

events <- list()

events[[1]] <- list(
  M = integer(0),
  s = numeric(0),
  name = "empty"
)

for (M in combn(1:3, m = 1, simplify = FALSE)) {
  S <- expand.grid(rep(list(c(-1, 1)), length(M)))
  
  for (i in seq_len(nrow(S))) {
    s <- as.numeric(S[i, ])
    
    events[[length(events) + 1]] <- list(
      M = M,
      s = s,
      name = event_name(M, s)
    )
  }
}

for (M in combn(1:3, m = 2, simplify = FALSE)) {
  S <- expand.grid(rep(list(c(-1, 1)), length(M)))
  
  for (i in seq_len(nrow(S))) {
    s <- as.numeric(S[i, ])
    
    events[[length(events) + 1]] <- list(
      M = M,
      s = s,
      name = event_name(M, s)
    )
  }
}

poly_list <- list()
label_list <- list()

for (i in seq_along(events)) {
  ev <- events[[i]]
  
  mats <- event_matrices(ev$M, ev$s, X, lambda)
  poly <- clip_polytope(mats$A, mats$b, xlim, ylim)
  
  if (!is.null(poly)) {
    highlight <- length(ev$M) == length(target_M) && all(ev$M == target_M)
    
    poly$event <- ev$name
    poly$highlight <- highlight
    
    poly_list[[length(poly_list) + 1]] <- poly
    
    if (highlight) {
      label_list[[length(label_list) + 1]] <- data.frame(
        x = mean(poly$x),
        y = mean(poly$y),
        lab = sign_label(ev$s)
      )
    }
  }
}

df_poly <- do.call(rbind, poly_list)
label_df <- do.call(rbind, label_list)

df_all <- df_poly[df_poly$highlight == FALSE, ]
df_highlight <- df_poly[df_poly$highlight == TRUE, ]

df_arrows <- data.frame(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  xend = 0.7*X[1, ],
  yend = 0.7*X[2, ],
  labx = 0.9*X[1, ],
  laby = 0.9*X[2, ],
  lab = c("x1", "x2", "x3")
)

p_plot <- ggplot() +
  geom_polygon(
    data = df_all,
    aes(x = x, y = y, group = event),
    fill = NA,
    color = "grey75",
    linewidth = 0.35
  ) +
  geom_polygon(
    data = df_highlight,
    aes(x = x, y = y, group = event),
    fill = "grey88",
    color = "black",
    linewidth = 0.7
  ) +
  geom_segment(
    data = df_arrows,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 0.35,
    color = "grey45",
    arrow = arrow(length = grid::unit(0.12, "cm"))
  ) +
  geom_text(
    data = df_arrows,
    aes(x = labx, y = laby, label = lab),
    color = "black",
    size = 4
  ) +
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = lab),
    size = 3.6
  ) +
  coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(
    x = expression(y[1]),
    y = expression(y[2]),
    title = expression(M == "{"*1*", "*3*"}")
  ) +
  theme_book +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

ggsave(file.path("Figures", "Outputs", "fig-4-02.pdf"), plot = p_plot, width = 4, height = 4)
