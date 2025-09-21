# gen_gridcube() works

    Code
      gen_gridcube(n = 500, p = 4)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
            x1    x2    x3    x4
         <dbl> <dbl> <dbl> <dbl>
       1     0     0  0     0   
       2     0     0  0     0.25
       3     0     0  0     0.5 
       4     0     0  0     0.75
       5     0     0  0     1   
       6     0     0  0.25  0   
       7     0     0  0.25  0.25
       8     0     0  0.25  0.5 
       9     0     0  0.25  0.75
      10     0     0  0.25  1   
      # i 490 more rows

---

    Code
      gen_gridcube(n = 500, p = 10)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 504 x 10
            x1    x2    x3    x4    x5    x6    x7    x8    x9   x10
         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
       1   NaN   NaN   NaN   NaN   NaN     0     0     0   0   0    
       2   NaN   NaN   NaN   NaN   NaN     0     0     0   0   0.167
       3   NaN   NaN   NaN   NaN   NaN     0     0     0   0   0.333
       4   NaN   NaN   NaN   NaN   NaN     0     0     0   0   0.5  
       5   NaN   NaN   NaN   NaN   NaN     0     0     0   0   0.667
       6   NaN   NaN   NaN   NaN   NaN     0     0     0   0   0.833
       7   NaN   NaN   NaN   NaN   NaN     0     0     0   0   1    
       8   NaN   NaN   NaN   NaN   NaN     0     0     0   0.2 0    
       9   NaN   NaN   NaN   NaN   NaN     0     0     0   0.2 0.167
      10   NaN   NaN   NaN   NaN   NaN     0     0     0   0.2 0.333
      # i 494 more rows

# gen_unifcube() works

    Code
      gen_unifcube(n = 500, p = 4)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
              x1      x2     x3      x4
           <dbl>   <dbl>  <dbl>   <dbl>
       1  0.406  -0.0980  0.339 -0.456 
       2  0.0696  0.242  -0.414 -0.366 
       3 -0.240  -0.328  -0.374  0.0892
       4 -0.143   0.1000  0.404 -0.335 
       5 -0.402   0.307   0.285 -0.207 
       6  0.308  -0.148   0.143 -0.0804
       7  0.401   0.184   0.151  0.417 
       8 -0.0344 -0.265  -0.295  0.249 
       9  0.147  -0.128  -0.269 -0.455 
      10  0.0590  0.423   0.245  0.172 
      # i 490 more rows

---

    Code
      gen_unifcube(n = 500, p = 10)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 10
              x1      x2      x3       x4      x5       x6       x7      x8      x9
           <dbl>   <dbl>   <dbl>    <dbl>   <dbl>    <dbl>    <dbl>   <dbl>   <dbl>
       1 -0.374  -0.260  -0.107  -0.245    0.380  -0.469   -0.159    0.0641  0.331 
       2 -0.483   0.427   0.363   0.0537   0.362   0.436    0.149   -0.137  -0.0389
       3  0.400  -0.131   0.348  -0.0949  -0.0931  0.331    0.477    0.0506 -0.102 
       4  0.461   0.346   0.118   0.343   -0.295   0.458   -0.324    0.295   0.0324
       5 -0.403  -0.381  -0.384   0.00938 -0.146   0.00240  0.00397  0.411   0.0595
       6  0.0696 -0.458  -0.111  -0.361    0.369   0.183   -0.205    0.0342 -0.0487
       7  0.0847 -0.315   0.482   0.446   -0.234  -0.281    0.446    0.0249 -0.0136
       8 -0.124   0.166  -0.0175  0.450    0.203  -0.0705   0.349    0.425  -0.179 
       9  0.396  -0.407   0.348  -0.139    0.193   0.166    0.112    0.0856 -0.397 
      10 -0.258   0.0523 -0.391   0.204   -0.167   0.0233  -0.286    0.0653  0.181 
      # i 490 more rows
      # i 1 more variable: x10 <dbl>

# gen_cubehole() works

    Code
      gen_cubehole(n = 1000, p = 4)
    Message
      v Data generation completed successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 687 x 4
              x1     x2      x3     x4
           <dbl>  <dbl>   <dbl>  <dbl>
       1  0.406   0.339 -0.374  -0.107
       2  0.0696 -0.414 -0.483   0.363
       3 -0.240  -0.374  0.400   0.348
       4 -0.143   0.404  0.461   0.118
       5 -0.402   0.285 -0.403  -0.384
       6  0.401   0.151  0.0847  0.482
       7  0.147  -0.269  0.396   0.348
       8  0.0590  0.245 -0.258  -0.391
       9 -0.0613  0.324 -0.367   0.188
      10 -0.437   0.102 -0.234   0.430
      # i 677 more rows

---

    Code
      gen_cubehole(n = 1000, p = 10)
    Message
      v Data generation completed successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 997 x 10
              x1       x2      x3      x4      x5      x6     x7      x8      x9
           <dbl>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl>
       1  0.380  -0.159    0.331   0.148   0.0198 -0.311  -0.464 -0.287   0.0636
       2  0.362   0.149   -0.0389 -0.0664 -0.382  -0.490   0.208 -0.489   0.165 
       3 -0.0931  0.477   -0.102   0.432  -0.0863 -0.0809 -0.146 -0.454  -0.257 
       4 -0.295  -0.324    0.0324 -0.0841 -0.213   0.417  -0.144 -0.0572 -0.0715
       5 -0.146   0.00397  0.0595  0.424   0.490   0.0645 -0.487  0.0117  0.331 
       6  0.369  -0.205   -0.0487 -0.151  -0.0656  0.0801  0.192  0.428   0.431 
       7 -0.234   0.446   -0.0136  0.409  -0.450  -0.0668  0.352 -0.220   0.179 
       8  0.203   0.349   -0.179  -0.118  -0.478   0.107  -0.134  0.0431  0.381 
       9  0.193   0.112   -0.397  -0.219  -0.138   0.0509  0.361  0.148  -0.389 
      10 -0.167  -0.286    0.181   0.405  -0.451  -0.126   0.427 -0.486   0.248 
      # i 987 more rows
      # i 1 more variable: x10 <dbl>

