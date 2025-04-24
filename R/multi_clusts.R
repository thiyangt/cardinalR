#' Generate Multiple Clusters
#'
#' This function generates a dataset with multiple clusters.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 3) representing the number of clusters.
#' @param loc A numeric matrix (default: matrix(c(0, 0, 0, 0,
#' 5, 9, 0, 0,
#' 3, 4, 10, 7
#' ), nrow = 4, byrow = TRUE)) representing the locations/centroids of clusters.
#' @param scale A numeric vector (default: c(3, 1, 2)) representing the scaling factors of clusters.
#' @param shape A character vector (default: c("gen_gaussian", "gen_bluntedcorn", "gen_unifcube")) representing the shapes of clusters.
#' @param rotation A numeric list which contains plane and the corresponding angle along that plane for each cluster.
#' @param is_bkg A Boolean value (default: FALSE) representing the background noise should exist or not.
#' @return A data containing same/different shaped clusters.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' rotations_4d <- list(
#' cluster1 = list(
#'   list(plane = c(1, 2), angle = 60), # Rotation in the (1, 2) plane
#'   list(plane = c(3, 4), angle = 90)  # Rotation in the (3, 4) plane
#'   ),
#' cluster2 = list(
#'   list(plane = c(1, 3), angle = 30) # Rotation in the (1, 3) plane
#'   ),
#' cluster3 = list(
#'   list(plane = c(2, 4), angle = 45) # Rotation in the (2, 4) plane
#'   )
#' )
#' clust_data <- gen_multicluster(n = c(200, 300, 500), p = 4, k = 3,
#' loc = matrix(c(
#'   0, 0, 0, 0,
#'   5, 9, 0, 0,
#'   3, 4, 10, 7
#' ), nrow = 4, byrow = TRUE),
#' scale = c(3, 1, 2),
#' shape = c("gaussian", "bluntedcorn", "unifcube"),
#' rotation = rotations_4d,
#' is_bkg = FALSE)
gen_multicluster <- function(n = c(200, 300, 500), p = 4, k = 3,
                             loc = matrix(c(
                               0, 0, 0, 0,
                               5, 9, 0, 0,
                               3, 4, 10, 7  # height of smaller equilateral triangle in 2D
                             ), nrow = 4, byrow = TRUE),
                             scale = c(3, 1, 2),
                             shape = c("gaussian", "bluntedcorn", "unifcube"),
                             rotation = NULL,
                             is_bkg = FALSE) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (k < 1) {
    cli::cli_abort("k should be greater than 1.")
  }

  if (length(n) != k) {
    cli::cli_abort("n should contain exactly {.val {k}} values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  if (length(scale) != k) {
    cli::cli_abort("scale should contain exactly {.val {k}} values.")
  }

  if (any(scale < 0)) {
    cli::cli_abort("Values in scale should be positive.")
  }

  if (length(shape) != k) {
    cli::cli_abort("shape should contain exactly {.val {k}} values.")
  }

  if (!is.matrix(loc)) {
    cli::cli_abort("loc should be a matrix.")
  }

  if (NROW(loc) != p) {
    cli::cli_abort("Number of rows in loc should be {.val {p}}.")
  }

  if (NCOL(loc) != k) {
    cli::cli_abort("Number of rows in loc should be {.val {k}}.")
  }

  dfs <- list()

  ## To generate different shaped clusters
  for (i in 1:k) {

    ## To generate the cluster
    cluster_df <- get(paste0("gen_", shape[i]))(n = n[i], p = p)

    ## To normalize the data
    cluster_df <- normalize_data(data = cluster_df)

    ## To multiply by the scale factor
    cluster_df <- scale[i] * cluster_df

    ## To rotate the cluster
    if(!is.null(rotation)) {
      rotation_clust <- gen_rotation(p = p, planes_angles = rotation[[i]])
      cluster_df <- t(rotation_clust %*% t(cluster_df))

    }

    ## To move to the center
    cluster_df <- apply(cluster_df, 2, function(col) col - mean(col))

    ## To re-position the data to centroids given
    cluster_df <- cluster_df + matrix(rep(loc[,i], n[i]), ncol=p, byrow=T)

    cluster_df <- cluster_df |>
      tibble::as_tibble(.name_repair = "minimal")

    names(cluster_df) <- paste0("x", 1:p)

    ## To add cluster label
    dfs[[i]] <- cluster_df |>
      dplyr::mutate(cluster = paste0("cluster", i))


  }

  ## To combine the data
  df <- dplyr::bind_rows(dfs)

  # ## Add background noise
  if(isTRUE(is_bkg)) {

    mean <- colMeans(df[sapply(df, is.numeric)])
    std <- sapply(df[sapply(df, is.numeric)], sd)

    noise_df <- gen_bkgnoise(n = max(n) * 0.1, p = p,
                             m = mean, s = std) |>
      dplyr::mutate(cluster = "bkg_noise")

    df <- dplyr::bind_rows(df, noise_df)
  }

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Multiple clusters generation completed successfully! 🎉")
  return(df)

}
