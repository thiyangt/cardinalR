
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cardinalR <img src="man/figures/logo.png" align="right" height="150" alt="" />

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/cardinalR)](https://CRAN.R-project.org/package=cardinalR)
[![Downloads](http://cranlogs.r-pkg.org/badges/cardinalR)](https://cran.r-project.org/package=cardinalR)

The `cardinalR` package provides acollection of functions to generate a
large variety of structures in high dimensions.

## Installation

You can install the released version of `cardinalR` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cardinalR") 
```

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JayaniLakshika/cardinalR")
```

## Example

``` r
library(cardinalR)
```

``` r
head(mobius_clust_data, 5)
#> # A tibble: 5 × 4
#>       x1     x2      x3       x4
#>    <dbl>  <dbl>   <dbl>    <dbl>
#> 1  0.304  0.706 -0.152   0.0362 
#> 2  0.916  0.588  0.0260 -0.0436 
#> 3  0.178  1.20   0.185   0.00820
#> 4  0.740 -1.05  -0.148  -0.0462 
#> 5 -0.899  0.304 -0.307  -0.0387
```

<table style="width:100%">
<tr>
<td align="center">
<img src="man/figures/mobius_1.png" height="200" alt="" />
</td>
<td align="center">
<img src="man/figures/mobius_2.png" height="200" alt="" />
</td>
<td align="center">
<img src="man/figures/mobius_3.png" height="200" alt="" />
</td>
</tr>
</table>

You can find the high-dimensional view in
[here](https://youtu.be/731aZxDifCs).

tSNE (t-distributed Stochastic Neighbor Embedding) and UMAP (Uniform
Manifold Approximation and Projection) representations of
`mobius_clust_data` are shown below.

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" style="display: block; margin: auto;" alt="A brief description of the plot or figure shown in the image" />

## About the name

**C**ollection of v**ar**ious high-**d**imens**i**o**nal** data
structures in **R**

## Copyright

This package is licensed under the [MIT
license](https://github.com/JayaniLakshika/cardinalR/tree/main?tab=MIT-2-ov-file).
