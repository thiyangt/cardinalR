# gen_multicluster() works

    Code
      clust_data
    Output
      # A tibble: 1,000 x 5
            x1    x2       x3     x4 cluster 
         <dbl> <dbl>    <dbl>  <dbl> <chr>   
       1  3.52  3.58  9.86     7.25  cluster3
       2  4.99  9.02 -0.0516  -0.381 cluster2
       3  5.00  8.96  0.0357  -0.307 cluster2
       4  2.34  4.36 10.3      6.52  cluster3
       5  2.48  4.50  9.57     6.98  cluster3
       6  4.95  9.05  0.00858  0.240 cluster2
       7  4.96  8.94  0.0683   0.250 cluster2
       8  4.96  8.95  0.0126  -0.113 cluster2
       9  2.78  3.76  9.67     6.39  cluster3
      10  1.99  1.58 -0.623    0.847 cluster1
      # i 990 more rows

---

    Code
      clust_data
    Output
      # A tibble: 1,000 x 5
              x1     x2     x3     x4 cluster 
           <dbl>  <dbl>  <dbl>  <dbl> <chr>   
       1 -2.36    0.729  1.01   0.610 cluster1
       2  3.78    3.98  10.6    7.63  cluster3
       3  0.271   0.746  0.504  1.01  cluster1
       4  0.990   0.647  1.01   0.352 cluster1
       5  0.288   0.786 -0.713 -0.102 cluster1
       6  2.59    3.64  10.4    6.77  cluster3
       7  2.98    4.19   9.79   6.91  cluster3
       8 -0.0248 -0.374 -0.493  1.43  cluster1
       9  2.99    3.66  10.0    7.19  cluster3
      10  2.55    3.90  10.7    7.18  cluster3
      # i 990 more rows

---

    Code
      clust_data
    Output
      # A tibble: 1,000 x 5
              x1     x2      x3     x4 cluster 
           <dbl>  <dbl>   <dbl>  <dbl> <chr>   
       1  0.412  -0.498  0.586   0.222 cluster1
       2  2.98    4.77   9.63    6.71  cluster3
       3  5.00    9.06   0.0277 -0.365 cluster2
       4  5.08    9.00  -0.0762  0.550 cluster2
       5 -0.0507  0.345  0.764   0.919 cluster1
       6  2.17    4.44   9.78    7.38  cluster3
       7  5.02    9.06   0.0208 -0.363 cluster2
       8 -0.600   1.48  -1.67    0.430 cluster1
       9  0.0973  0.224 -0.117  -0.590 cluster1
      10  3.44    4.02   9.47    7.68  cluster3
      # i 990 more rows

