library(cardinalR)
library(tibble)
library(Rtsne)
library(uwot)

set.seed(20240412)

## To generate mobius clust data

mobiusgau <- make_mobiusgau(n = c(800, 200), p = 4) |>
  dplyr::select(-cluster)

usethis::use_data(mobiusgau, overwrite = TRUE)


## Fit umap with different parameter settings
### Param 1
n_neighbors <- 15
min_dist <- 0.1

UMAP_model <- umap(mobiusgau,
                   n_neighbors = n_neighbors,
                   min_dist = min_dist,
                   n_components =  2,
                   init ="spca")

mobiusgau_umap1 <- UMAP_model |>
  as_tibble(.name_repair = "unique")

names(mobiusgau_umap1) <- c("emb1", "emb2")

usethis::use_data(mobiusgau_umap1, overwrite = TRUE)

### Param 2
n_neighbors <- 30
min_dist <- 0.08

UMAP_model <- umap(mobiusgau,
                   n_neighbors = n_neighbors,
                   min_dist = min_dist,
                   n_components =  2,
                   init ="spca")

mobiusgau_umap2 <- UMAP_model |>
  as_tibble(.name_repair = "unique")

names(mobiusgau_umap2) <- c("emb1", "emb2")

usethis::use_data(mobiusgau_umap2, overwrite = TRUE)

### Param 3
n_neighbors <- 5
min_dist <- 0.9

UMAP_model <- umap(mobiusgau,
                   n_neighbors = n_neighbors,
                   min_dist = min_dist,
                   n_components =  2,
                   init ="spca")

mobiusgau_umap3 <- UMAP_model |>
  as_tibble(.name_repair = "unique")

names(mobiusgau_umap3) <- c("emb1", "emb2")

usethis::use_data(mobiusgau_umap3, overwrite = TRUE)

## Fit tsne with different parameter settings
### Param 1
tsne_fit <- Rtsne(mobiusgau,
  perplexity = 15, pca = FALSE,
  pca_center = FALSE, normalize = FALSE
)

tsne_layout <- tsne_fit$Y

colnames(tsne_layout) <- c("emb1", "emb2")

mobiusgau_tsne1 <- tsne_layout |>
  as_tibble()

usethis::use_data(mobiusgau_tsne1, overwrite = TRUE)

### Param 2
tsne_fit <- Rtsne(mobiusgau,
  perplexity = 30, pca = FALSE,
  pca_center = FALSE, normalize = FALSE
)

tsne_layout <- tsne_fit$Y

colnames(tsne_layout) <- c("emb1", "emb2")

mobiusgau_tsne2 <- tsne_layout |>
  as_tibble()

usethis::use_data(mobiusgau_tsne2, overwrite = TRUE)

### Param 3
tsne_fit <- Rtsne(mobiusgau,
  perplexity = 5, pca = FALSE,
  pca_center = FALSE, normalize = FALSE
)

tsne_layout <- tsne_fit$Y

colnames(tsne_layout) <- c("emb1", "emb2")

mobiusgau_tsne3 <- tsne_layout |>
  as_tibble()

usethis::use_data(mobiusgau_tsne3, overwrite = TRUE)
