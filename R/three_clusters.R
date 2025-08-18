#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a \eqn{p}-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#' @param p Integer, the dimensionality of the space. Default is \code{4}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_01 <- make_three_clust_01()
#'
#' @export
make_three_clust_01 <- function(n = c(700, 300, 500), p = 4) {

  ## To generate data
  df <- gen_multicluster(n = n, p = p, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 0, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(1, 1, 1),
                         shape = c("quadratic", "cone", "gaussian"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a \eqn{p}-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#' @param p Integer, the dimensionality of the space. Default is \code{4}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_02 <- make_three_clust_02()
#'
#' @export
make_three_clust_02 <- function(n = c(700, 300, 500), p = 4) {

  ## To generate data
  df <- gen_multicluster(n = n, p = p, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 0, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(1, 1, 1),
                         shape = c("crescent", "pyrrect", "unifcube"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

