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
#' ), nrow = 3, byrow = TRUE)) representing the locations/centroids of clusters.
#' @param scale A numeric vector (default: c(3, 1, 2)) representing the scaling factors of clusters.
#' @param shape A character vector (default: c("gen_gaussian", "gen_cone", "gen_unifcube")) representing the shapes of clusters.
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
#' ), nrow = 3, byrow = TRUE),
#' scale = c(3, 1, 2),
#' shape = c("gen_gaussian", "gen_cone", "gen_unifcube"),
#' rotation = rotations_4d,
#' is_bkg = FALSE)
gen_multicluster <- function(n = c(200, 300, 500), p = 4, k = 3,
                             loc = NULL,
                             scale = rep(1, k),
                             shape = NULL,
                             generators = NULL,
                             clusters = NULL,
                             rotation = NULL,
                             is_bkg = FALSE,
                             ...) {

  # --- checks ---
  if (p < 2) cli::cli_abort("p should be greater than 2.")
  if (k < 1) cli::cli_abort("k should be greater than 1.")
  if (length(n) != k) cli::cli_abort("n should contain exactly {.val {k}} values.")
  if (any(n < 0)) cli::cli_abort("Values in n should be positive.")
  if (length(scale) != k) cli::cli_abort("scale should contain exactly {.val {k}} values.")
  if (any(scale < 0)) cli::cli_abort("Values in scale should be positive.")

  # handle loc
  if (!is.null(loc)) {
    if (!is.matrix(loc)) cli::cli_abort("loc should be a matrix.")
    if (NROW(loc) != k) cli::cli_abort("Number of rows in loc should be {.val {k}}.")
    if (NCOL(loc) != p) cli::cli_abort("Number of cols in loc should be {.val {p}}.")
  }

  # handle rotation
  if (!is.null(rotation) && !is.list(rotation)) cli::cli_abort("rotation should be a list or NULL.")
  if (is.list(rotation) && length(rotation) != k) {
    cli::cli_abort("Number of elements in rotation should be {.val {k}}.")
  }

  # --- mode selection ---
  if (!is.null(clusters)) {
    if (length(clusters) != k) {
      cli::cli_abort("clusters must have exactly {.val {k}} elements.")
    }
    cluster_list <- clusters
  } else {
    # determine generator functions
    if (!is.null(generators)) {
      if (length(generators) != k) cli::cli_abort("generators must have exactly {.val {k}} functions.")
      gens <- generators
    } else if (!is.null(shape)) {
      if (length(shape) != k) cli::cli_abort("shape should contain exactly {.val {k}} values.")
      gens <- lapply(shape, function(s) {
        fn <- tryCatch(match.fun(paste0("gen_", s)), error = function(e) NULL)
        if (is.null(fn)) cli::cli_abort("No generator found for shape {.val {s}}.")
        fn
      })
    } else {
      cli::cli_abort("You must provide either shape, generators, or clusters.")
    }

    # generate clusters using functions
    cluster_list <- vector("list", k)
    for (i in seq_len(k)) {
      cluster_list[[i]] <- gens[[i]](n = n[i], p = p, ...)
    }
  }

  # --- apply normalization, scaling, rotation, location ---
  dfs <- vector("list", k)
  for (i in seq_len(k)) {
    cluster_df <- normalize_data(cluster_list[[i]])
    cluster_df <- scale[i] * cluster_df

    # rotation
    if (!is.null(rotation)) {
      rotation_clust <- gen_rotation(p = p, planes_angles = rotation[[i]])
      cluster_df <- t(rotation_clust %*% t(cluster_df))
    }

    # center & shift
    cluster_df <- apply(cluster_df, 2, function(col) col - mean(col))
    if (!is.null(loc)) {
      cluster_df <- cluster_df + matrix(rep(loc[i, ], NROW(cluster_df)), ncol = p, byrow = TRUE)
    }

    dfs[[i]] <- tibble::as_tibble(cluster_df, .name_repair = "minimal") |>
      dplyr::mutate(cluster = paste0("cluster", i))
    names(dfs[[i]])[1:p] <- paste0("x", 1:p)
  }

  # --- combine ---
  df <- dplyr::bind_rows(dfs)

  # background noise
  if (isTRUE(is_bkg)) {
    mean <- colMeans(df[sapply(df, is.numeric)])
    std  <- sapply(df[sapply(df, is.numeric)], stats::sd)
    noise_df <- gen_bkgnoise(n = max(n) * 0.1, p = p, m = mean, s = std) |>
      dplyr::mutate(cluster = "bkg_noise")
    df <- dplyr::bind_rows(df, noise_df)
  }

  # shuffle
  df <- randomize_rows(df)

  cli::cli_alert_success("Multiple clusters generation completed successfully!!!")
  return(df)
}

