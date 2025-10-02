# gen_gridcube() works

    Code
      gen_gridcube(n = 500, p = 4)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 6,561 x 4
            x1    x2    x3    x4
         <dbl> <dbl> <dbl> <dbl>
       1     0     0     0     0
       2     1     0     0     0
       3     0     1     0     0
       4     1     1     0     0
       5     0     0     1     0
       6     1     0     1     0
       7     0     1     1     0
       8     1     1     1     0
       9     0     0     0     1
      10     1     0     0     1
      # i 6,551 more rows

---

    Code
      gen_gridcube(n = 500, p = 5)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 59,049 x 5
            x1    x2    x3    x4    x5
         <dbl> <dbl> <dbl> <dbl> <dbl>
       1     0     0     0     0     0
       2     1     0     0     0     0
       3     0     1     0     0     0
       4     1     1     0     0     0
       5     0     0     1     0     0
       6     1     0     1     0     0
       7     0     1     1     0     0
       8     1     1     1     0     0
       9     0     0     0     1     0
      10     1     0     0     1     0
      # i 59,039 more rows

# gen_unifcube() works

    Code
      gen_unifcube(n = 500, p = 4)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
             x1    x2     x3     x4
          <dbl> <dbl>  <dbl>  <dbl>
       1 0.906  0.402 0.839  0.0436
       2 0.570  0.742 0.0856 0.134 
       3 0.260  0.172 0.126  0.589 
       4 0.357  0.600 0.904  0.165 
       5 0.0982 0.807 0.785  0.293 
       6 0.808  0.352 0.643  0.420 
       7 0.901  0.684 0.651  0.917 
       8 0.466  0.235 0.205  0.749 
       9 0.647  0.372 0.231  0.0446
      10 0.559  0.923 0.745  0.672 
      # i 490 more rows

---

    Code
      gen_unifcube(n = 500, p = 5)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 5
             x1     x2    x3    x4    x5
          <dbl>  <dbl> <dbl> <dbl> <dbl>
       1 0.126  0.240  0.393 0.255 0.880
       2 0.0170 0.927  0.863 0.554 0.862
       3 0.900  0.369  0.848 0.405 0.407
       4 0.961  0.846  0.618 0.843 0.205
       5 0.0969 0.119  0.116 0.509 0.354
       6 0.570  0.0417 0.389 0.139 0.869
       7 0.585  0.185  0.982 0.946 0.266
       8 0.376  0.666  0.483 0.950 0.703
       9 0.896  0.0926 0.848 0.361 0.693
      10 0.242  0.552  0.109 0.704 0.333
      # i 490 more rows

# gen_unifcubehole() works

    Code
      gen_unifcubehole(n = 1000, p = 4)
    Message
      v Data generation completed successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 677 x 4
             x1     x2     x3    x4
          <dbl>  <dbl>  <dbl> <dbl>
       1 0.906  0.839  0.126  0.393
       2 0.570  0.0856 0.0170 0.863
       3 0.260  0.126  0.900  0.848
       4 0.357  0.904  0.961  0.618
       5 0.0982 0.785  0.0969 0.116
       6 0.901  0.651  0.585  0.982
       7 0.647  0.231  0.896  0.848
       8 0.559  0.745  0.242  0.109
       9 0.439  0.824  0.133  0.688
      10 0.0627 0.602  0.266  0.930
      # i 667 more rows

---

    Code
      gen_unifcubehole(n = 1000, p = 10)
    Message
      v Data generation completed successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 996 x 10
            x1    x2    x3    x4     x5      x6     x7     x8    x9   x10
         <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl>  <dbl>  <dbl> <dbl> <dbl>
       1 0.880 0.341 0.831 0.648 0.520  0.189   0.0359 0.213  0.564 0.984
       2 0.862 0.649 0.461 0.434 0.118  0.00961 0.708  0.0106 0.665 0.844
       3 0.407 0.977 0.398 0.932 0.414  0.419   0.354  0.0458 0.243 0.924
       4 0.205 0.176 0.532 0.416 0.287  0.917   0.356  0.443  0.428 0.628
       5 0.354 0.504 0.560 0.924 0.990  0.564   0.0129 0.512  0.831 0.226
       6 0.869 0.295 0.451 0.349 0.434  0.580   0.692  0.928  0.931 0.701
       7 0.266 0.946 0.486 0.909 0.0502 0.433   0.852  0.280  0.679 0.928
       8 0.703 0.849 0.321 0.382 0.0219 0.607   0.366  0.543  0.881 0.719
       9 0.693 0.612 0.103 0.281 0.362  0.551   0.861  0.648  0.111 0.355
      10 0.333 0.214 0.681 0.905 0.0486 0.374   0.927  0.0144 0.748 0.183
      # i 986 more rows

