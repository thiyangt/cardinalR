#' Generate Multiple Clusters
#'
#' This function generates a dataset with multiple clusters in high-dimensional space.
#' Each cluster can have a different shape, scale, rotation, and centroid,
#' allowing the construction of complex synthetic datasets.
#'
#' @param n A numeric vector (default: c(200, 500, 300)) representing the sample sizes
#'   for each cluster. Must have length \code{k}.
#' @param k A numeric value (default: 3) representing the number of clusters.
#' @param loc A numeric matrix giving the centroids of the clusters.
#'   The number of rows must equal \code{k}; the number of columns should match
#'   the maximum dimensionality across all shapes.
#' @param scale A numeric vector (default: c(3, 1, 2)) giving the scaling factors
#'   for each cluster. Must have length \code{k}.
#' @param shape A character vector (default: c("gaussian", "cone", "unifcube"))
#'   specifying the generator function to use for each cluster. Must have length \code{k}.
#' @param rotation A list of rotation matrices (one per cluster), or \code{NULL}.
#'   Each matrix must be square with dimension equal to the total number of
#'   structural dimensions in the dataset. Rotation matrices can be generated
#'   using \code{\link{gen_rotation}} for convenience.
#' @param is_bkg Logical (default: FALSE). If \code{TRUE}, adds background noise
#'   sampled from a multivariate normal distribution centered on the dataset mean
#'   with standard deviations matching the observed spread.
#' @param ... Additional arguments passed to the cluster generator functions.
#'
#' @return A tibble containing all generated clusters, with columns \code{x1, x2, ...}
#'   for dimensions and a \code{cluster} label.
#'
#' @export
#'
#' @examples
#' set.seed(20240412)
#'
#' # Example rotation matrices for 4D space
#' rot1 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(1, 2), angle = 60),
#'                                                 list(plane = c(3, 4), angle = 90)))
#' rot2 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(1, 3), angle = 30)))
#' rot3 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(2, 4), angle = 45)))
#'
#' clust_data <- gen_multicluster(
#'   n = c(200, 300, 500),
#'   k = 3,
#'   loc = matrix(c(
#'     0, 0, 0, 0,
#'     5, 9, 0, 0,
#'     3, 4, 10, 7
#'   ), nrow = 3, byrow = TRUE),
#'   scale = c(3, 1, 2),
#'   shape = c("gaussian", "cone", "unifcube"),
#'   rotation = list(rot1, rot2, rot3),
#'   is_bkg = FALSE
#' )
gen_multicluster <- function(n = c(200, 300, 500),
                             k = 3,
                             loc = matrix(c(
                               0, 0, 0, 0,
                               5, 9, 0, 0,
                               3, 4, 10, 7
                             ), nrow = 3, byrow = TRUE),
                             scale = c(3, 1, 2),
                             shape = c("gaussian", "bluntedcorn", "unifcube"),
                             rotation = NULL,
                             is_bkg = FALSE,
                             ...) {

  # --- checks ---
  if (k < 1) cli::cli_abort("k should be greater than 1.")
  if (length(n) != k) cli::cli_abort("n should contain exactly {.val {k}} values.")
  if (any(n < 0)) cli::cli_abort("Values in n should be positive.")
  if (length(scale) != k) cli::cli_abort("scale should contain exactly {.val {k}} values.")
  if (any(scale < 0)) cli::cli_abort("Values in scale should be positive.")
  if (length(shape) != k) cli::cli_abort("shape should contain exactly {.val {k}} values.")

  if (!is.null(rotation)) {
    if (!is.list(rotation)) rotation <- list(rotation)
    if (length(rotation) < k) {
      # fill missing rotations with identity matrices of appropriate size
      for (i in (length(rotation)+1):k) {
        rotation[[i]] <- "identity"  # will be treated as no rotation later
      }
    }
  } else {
    rotation <- vector("list", k)  # all "identity", i.e., no rotation
  }


  # --- generate clusters ---
  dfs <- vector("list", k)
  max_dims <- 0L  # track max structural dimensions

  raw_clusters <- vector("list", k)
  for (i in seq_len(k)) {
    fn <- tryCatch(match.fun(paste0("gen_", shape[i])),
                   error = function(e) cli::cli_abort("No generator found for shape {.val {shape[i]}}."))

    # Get the formal arguments of the generator
    fn_args <- names(formals(fn))

    # Pass only the arguments that exist for this generator
    extra_args <- list(...)
    pass_args <- extra_args[names(extra_args) %in% fn_args]

    cluster_df <- do.call(fn, c(list(n = n[i]), pass_args))

    cluster_df <- as.data.frame(cluster_df)
    d_i <- ncol(cluster_df)
    max_dims <- max(max_dims, d_i)

    raw_clusters[[i]] <- cluster_df
  }


  # --- pad clusters to max_dims ---
  dfs <- vector("list", k)
  for (i in seq_len(k)) {
    cluster_df <- raw_clusters[[i]]

    d_i <- ncol(cluster_df)

    # pad with Gaussian noise if fewer than max_dims
    if (d_i < max_dims) {
      mean_vals <- colMeans(cluster_df, na.rm = TRUE)

      # repeat values so they match the number of noise dims
      m <- rep(mean(mean_vals), max_dims - d_i)
      s <- rep(0.2, max_dims - d_i)

      pad <- gen_noisedims(n = n[i], p = max_dims - d_i, m = m, s = s)
      cluster_df <- cbind(cluster_df, pad)
    }

    # rotate if rotation matrix provided
    if (!is.null(rotation) && !is.null(rotation[[i]])) {
      rot_mat <- rotation[[i]]

      if (is.character(rot_mat) && rot_mat == "identity") {
        # create identity matrix of appropriate size
        rot_mat <- diag(ncol(cluster_df))
      }

      if (!is.matrix(rot_mat)) cli::cli_abort("rotation[[{i}]] must be a matrix.")
      if (!all(dim(rot_mat) == c(ncol(cluster_df), ncol(cluster_df)))) {
        cli::cli_abort("rotation[[{i}]] must be square with dimension {.val {ncol(cluster_df)}}.")
      }
      cluster_df <- t(rot_mat %*% t(cluster_df))
    }

    # normalize + scale
    cluster_df <- normalize_data(cluster_df)
    cluster_df <- scale[i] * cluster_df

    # center and shift
    cluster_df <- apply(cluster_df, 2, function(col) col - mean(col))
    if (!is.null(loc)) {
      if (NCOL(loc) != max_dims) {
        cli::cli_abort("Number of cols in loc must match the total dimensions {.val {max_dims}}.")
      }
      cluster_df <- cluster_df + matrix(rep(loc[i, ], NROW(cluster_df)), ncol = max_dims, byrow = TRUE)
    }

    # tibble and label
    cluster_df <- tibble::as_tibble(cluster_df, .name_repair = "minimal")
    names(cluster_df)[1:max_dims] <- paste0("x", 1:max_dims)

    dfs[[i]] <- cluster_df |> dplyr::mutate(cluster = paste0("cluster", i))
  }

  # --- combine ---
  df <- dplyr::bind_rows(dfs)

  # background noise
  if (isTRUE(is_bkg)) {
    mean_vals <- colMeans(df[sapply(df, is.numeric)])
    std_vals  <- sapply(df[sapply(df, is.numeric)], stats::sd)
    noise_df <- gen_bkgnoise(n = max(n) * 0.1, p = max_dims, m = mean_vals, s = std_vals) |>
      dplyr::mutate(cluster = "bkg_noise")
    df <- dplyr::bind_rows(df, noise_df)
  }

  # shuffle rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Multiple clusters generation completed successfully!!!")
  return(df)
}

