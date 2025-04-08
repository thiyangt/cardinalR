gen_multicluster <- function(n, p, k, location, spread, shape_vec, bkg_param) {

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
