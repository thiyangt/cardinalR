gen_mobius <- function(n = 500, p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- geozoo::mobius(n = n, p = 3)$points |>
    tibble::as_tibble(.name_repair = "minimal")

  names(df) <- paste0("x", 1:3)

  if (p > 3) {

    noise_df <- gen_noisedims(n = n, p = (p-3), m = rep(0, p-3), s = rep(0.05, p-3))
    names(noise_df) <- paste0("x", 4:p)

    df <- dplyr::bind_cols(df, noise_df)

  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}
