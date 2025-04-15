#' Generate Curvy
#'
#' This function generates a dataset representing a structure with a curvy pattern.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a curvy structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' curvy_data <- gen_curve(n = 500, p = 4)
gen_curve <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- matrix(0, nrow = n, ncol = p)
  # gen the core curvilinear pattern in 2D
  df[, 1] <- stats::runif(n, 0, 2)
  poly_basis <- stats::poly(df[, 1], degree = 2, raw = TRUE)
  df[, 2] <- -poly_basis[, 2] + stats::runif(n, 0, 0.5)

  if (p > 2){

    if(p==3) {
      # Define additional dimensions for 4D
      df[, 3] <- -sin(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)  # A sine-based curve

    } else if (p == 4) {
      df[, 3] <- -sin(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)  # A sine-based curve
      df[, 4] <- cos(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)   # A cosine-based curve

    } else {

      df[, 3] <- -sin(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)  # A sine-based curve
      df[, 4] <- cos(df[, 1] * pi) + stats::runif(n, -0.5, 0.5)   # A cosine-based curve

      noise_df <- gen_noisedims(n = n, p = (p-4), m = rep(0, p-4), s = rep(0.05, p-4)) |>
        as.matrix()
      colnames(noise_df) <- paste0("x", 5:p)

      df <- cbind(df, noise_df)

    }

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)

}
