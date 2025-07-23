make_three_clust_01 <- function(n = c(700, 300, 500), p = 4) {

  ## To generate data
  df <- gen_multicluster(n = n, p = p, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           0, 0, 0, 0,
                           0, 0, 0, 0
                         ), nrow = 3, byrow = TRUE),
                         scale = c(1, 1, 1),
                         shape = c("mobius", "gaussian"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}
