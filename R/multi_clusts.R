gen_multicluster <- function(n = c(200, 300, 500), p = 4, k = 3, loc, scale, shape, bkg) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
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

  if (NROW(loc) != k) {
    cli::cli_abort("Number of rows in loc should be {.val {k}}.")
  }

  if (NCOL(loc) != p) {
    cli::cli_abort("Number of rows in loc should be {.val {p}}.")
  }


  ## If the location is not given generate simple points to position the clusters

  dfs <- list()

  ## To generate different shaped clusters
  if (shape_vec %in% "gen_gaussian") {

    dfs[[1]] <- gen_gaussian() |>
      dplyr::mutate(cluster = paste0("cluster", "1"))

  }

  ## To re-position the data to centroids given

  ## To combine the data
  df <- tibble::as_tibble(dfs)

  ## Add background noise
  noise_df <- gen_bkg_noise() |>
    dplyr::mutate(cluster = paste0("bkg_noise"))
  df <- dplyr::bind_rows(df, noise_df)

  ## Swap rows
  df <- randomize_rows(df)

  return(df)

}
