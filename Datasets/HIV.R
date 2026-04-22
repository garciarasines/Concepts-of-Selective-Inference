library(readr)
library(dplyr)
library(tidyr)

allowed_aas <- c("A","C","D","E","F","G","H","I","K","L","M","N","P","Q","R","S","T","V","W","Y")

process <- function(path, drug, p_max = 1e3, min_occurrence = 11) {
  
  # Read data
  df <- read_tsv(path, show_col_types = FALSE)
  if (!drug %in% names(df)) {
    stop(sprintf("Column '%s' not found in %s", drug, basename(path)))
  }
  
  # Covariates
  positions <- intersect(paste0("P", seq_len(p_max)), names(df))
  df <- df %>% mutate(.row = row_number())
  
  # Long format, keep only single-letter amino acids (no '-', '.', mixtures, NA)
  long <- df %>%
    pivot_longer(all_of(positions), names_to = "pos", values_to = "aa") %>%
    filter(nchar(aa) == 1, aa != "-", aa != ".")
  
  # Count prevalence and keep features with >= min_occurrence
  counts <- long %>% count(pos, aa, name = "n")
  keep_pairs <- counts %>%
    filter(n >= min_occurrence) %>%
    mutate(var = paste0(pos, aa)) %>%
    select(pos, aa, var)
  
  # Build design matrix
  wide <- long %>%
    semi_join(keep_pairs, by = c("pos", "aa")) %>%
    mutate(var = paste0(pos, aa), value = 1L) %>%
    select(.row, var, value) %>%
    distinct() %>%
    pivot_wider(id_cols = .row, names_from = var, values_from = value, values_fill = 0L)
  
  X <- df %>%
    select(.row, all_of(drug)) %>%
    left_join(wide, by = ".row")
  
  # Filter rows with measured response
  X <- X %>% filter(!is.na(.data[[drug]]))
  
  # Response: log + center
  y <- log(as.numeric(X[[drug]]))
  y <- y - mean(y, na.rm = TRUE)
  
  # Design matrix
  X <- X %>% select(-.row, -all_of(drug))
  X <- as.matrix(X)
  
  # Remove duplicated columns
  keep <- !duplicated(as.data.frame(t(X)))
  X <- X[, keep, drop = FALSE]
  
  # Standardize predictors (z-score each column)
  X <- scale(X, center = TRUE, scale = TRUE)
  
  # Scale by 1/sqrt(n)
  n <- nrow(X)
  X <- X / sqrt(n)
  
  list(y = y, X = X, features = colnames(X))
}

# Example: 3TC
data_3tc <- process("https://hivdb.stanford.edu/_wrapper/pages/published_analysis/genophenoPNAS2006/DATA/NRTI_DATA.txt", drug = "3TC")
dim(data_3tc$X)





