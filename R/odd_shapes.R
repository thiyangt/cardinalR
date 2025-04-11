gen_clusteredSpheres <- function(k = 3, p = 4, n_small = 100, n_big = 1000,
                                       r_small = 3, r_big = 15, center_sd = 10 / sqrt(3)) {
  base_sphere <- function(n, p, r) {
    m <- matrix(rnorm(n * p), nrow = n, ncol = p)
    r * m / sqrt(rowSums(m^2))
  }

  d_dim_sphere <- base_sphere(n_small, p, r_small)
  small_spheres <- lapply(seq_len(k), function(i) {
    center <- rnorm(p, sd = center_sd)
    sweep(d_dim_sphere, 2, center, "+")
  })

  big_sphere <- base_sphere(n_big, p, r_big)

  small_labeled <- lapply(seq_along(small_spheres), function(i) {
    cbind(small_spheres[[i]], cluster = paste0("small_", i))
  })

  big_labeled <- cbind(big_sphere, cluster = "big")

  df <- do.call(rbind, c(small_labeled, list(big_labeled)))
  as.data.frame(df)
}
