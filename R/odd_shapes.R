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
#' mobgau <- make_mobiusgau(n = c(200, 100), p = 4)
make_mobiusgau <- function(n = c(200, 100), p = 4) {

  ## To generate data
  df <- gen_multicluster(n = n, p = p, k = 2,
                               loc = matrix(c(
                                 0, 0, 0, 0,
                                 0, 0, 0, 0
                               ), nrow = 4, byrow = TRUE),
                               scale = c(1, 0.3),
                               shape = c("mobius", "gaussian"),
                               rotation = NULL,
                               is_bkg = FALSE)

  return(df)
}

#' Generate Multiple Gaussian Clusters
#'
#' This function generates a dataset consisting of multiple Gaussian clusters.
#'
#' @param n A numeric vector (default: c(300, 200, 500)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 5) representing the number of clusters.
#' @param loc A numeric matrix (default: NULL) representing the locations/centroids of clusters.
#' @param scale A numeric vector (default: NULL) representing the scaling factors of clusters.
#' @return A data containing the Gaussian clusters.
#' @export
#'
#' @examples
#' loc_matrix <- matrix(c(0, 0, 0, 0,
#' 5, 9, 0, 0,
#' 3, 4, 10, 7
#' ), nrow = 4, byrow = TRUE)
#' multigau <- make_multigau(n = c(300, 200, 500), p = 4, k = 3, loc = loc_matrix, scale = c(0.2, 1.5, 0.5))
make_multigau <- function(n = c(300, 200, 500), p = 4, k = 3, loc = NULL, scale = NULL) {

  if (is.null(loc)) {
    loc <- gen_clustloc(p = p, k = k)
  }

  if(is.null(scale)) {
    scale <- sample(seq(0.5, 2.3), k, replace = TRUE)
  }

  ## To generate data
  df <- gen_multicluster(n = n, p = p, k = k,
                         loc = loc,
                         scale = scale,
                         shape = rep("gaussian", k),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}
