#' Generate Gaussian cluster with the Mobius Cluster
#'
#' This function generates a dataset consisting of a mobius cluster and Gaussian cluster.
#'
#' @param n A numeric vector (default: c(200, 100)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the mobius cluster and Gaussian cluster.
#' @export
#'
#' @examples
#' data <- make_mobiusgau(n = c(200, 100), p = 4)
make_mobiusgau <- function(n = c(200, 100), p = 4) {

  ## Assign weights for the dimensions
  dim4_weights <- list(
  cluster1 = c(1.0, 1.0, 1.0, 1.0),
  cluster2 = c(1.0, 1.0, 1.0, 1.0)
  )

  ## To generate data
  df <- gen_multicluster(n = n, p = p, k = 2,
                               loc = matrix(c(
                                 0, 0, 0, 0,
                                 0, 0, 0, 0
                               ), nrow = 4, byrow = TRUE),
                               dim_weights = dim4_weights,
                               scale = c(1, 1),
                               shape = c("mobius", "gaussian"),
                               rotation = NULL,
                               is_bkg = FALSE)

  return(df)
}
