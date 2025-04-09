gen_multicluster <- function(n = c(200, 300, 500), p = 4, k = 3,
                             loc = matrix(c(
                               0, 0, 0, 0,
                               5, 9, 0, 0,
                               3, 4, 10, 7  # height of smaller equilateral triangle in 2D
                             ), nrow = 4, byrow = TRUE),
                             scale = c(3, 1, 2),
                             shape = c("gen_gaussian", "gen_bluntedcorn", "gen_pyrrect"),
                             is_bkg = FALSE) {

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

    cluster_df <- scale[i] * get(shape[i])(n = n[i], p = p)

    cluster_df <- apply(cluster_df, 2, function(col) col - mean(col))

    ## To re-position the data to centroids given
    cluster_df <- cluster_df + matrix(rep(loc[,i], n[i]), ncol=p, byrow=T)

    dfs[[i]] <- cluster_df |>
      tibble::as_tibble() |>
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
      dplyr::mutate(cluster = paste0("bkg_noise"))

    df <- dplyr::bind_rows(df, noise_df)
  }

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Multiple clusters generation completed successfully! 🎉")
  return(df)

}
