gen_multicluster <- function(n = c(200, 300, 500), p = 4, k = 3,
                             loc = matrix(c(1.50988357, -0.3136416, -1.19624196,
                                            0.18266102, -0.9118574,  0.72919641,
                                            0.30807755, -0.2243636, -0.08371394,
                                            -0.08444208,  0.2832499, -0.19880779), nrow = 4, byrow = TRUE),
                             scale = c(1, 3, 2), shape = c("gen_gaussian", "gen_bluntedcorn", "gen_pyrrect"),
                             bkg) {

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

  if (NROW(loc) != p) {
    cli::cli_abort("Number of rows in loc should be {.val {p}}.")
  }

  if (NCOL(loc) != k) {
    cli::cli_abort("Number of rows in loc should be {.val {k}}.")
  }


  ## If the location is not given generate simple points to position the clusters

  dfs <- list()

  ## To generate different shaped clusters
  for (i in 1:k) {

    df <- scale[i] * get(shape[i])(n = n[i], p = p)

    dfs[[i]] <- df |>
      tibble::as_tibble() |>
      dplyr::mutate(cluster = paste0("cluster", i))

  }

  ## To re-position the data to centroids given

  ## To combine the data
  df <- dplyr::bind_rows(dfs)

  # ## Add background noise
  # noise_df <- gen_bkg_noise() |>
  #   dplyr::mutate(cluster = paste0("bkg_noise"))
  # df <- dplyr::bind_rows(df, noise_df)

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Multiple clusters generation completed successfully! 🎉")
  return(df)

}
