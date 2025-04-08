#' Generate p-D Triangular Prism
#'
#' This function generates p-D triangular prism datasets.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a triangular prism in p-D.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' triangular_data <- gen_triprism(n = 300, p = 3)
gen_triprism <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  trace_point <- stats::runif(p)
  corner_points <- geozoo::simplex(p=p)$points

  data_list <- lapply(1:p, function(i) rep(0, n))
  names(data_list) <- paste0("x", 1:p)

  df <- tibble::tibble(!!!data_list)

  for (i in 1:n) {
    trace_point <- (corner_points[sample((p + 1), 1), ] + trace_point) / 2
    df[i, ] <- as.list(trace_point)
  }

  cli::cli_alert_success("Data generation completed successfully! 🎉")
  return(df)
}
