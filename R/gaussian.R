gen_clusts_gau <- function(n = 300, p = 4, k = 3, m = c(0, 0, 0), s = 0.05) {
  df <- mvtnorm::rmvnorm(n, mean = m, sigma = diag(4) * s)/6

  df <- suppressMessages(tibble::as_tibble(df, .name_repair = "unique"))

  names(df) <- paste0("x", 1:p)

  return(df)
}
