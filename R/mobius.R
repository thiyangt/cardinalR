#' Generate a 3-D Mobius in High Dimensions
#'
#' This function generates a dataset representing a structure with a mobius.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a mobius structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' mobius <- gen_mobius(n = 500, p = 4)
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

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}
