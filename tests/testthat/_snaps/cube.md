# gen_gridcube() works

    Code
      gen_gridcube(n = 500, p = 4)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
            x1    x2    x3    x4
         <int> <int> <int> <int>
       1     1     1     1     1
       2     1     1     1     2
       3     1     1     1     3
       4     1     1     1     4
       5     1     1     1     5
       6     1     1     2     1
       7     1     1     2     2
       8     1     1     2     3
       9     1     1     2     4
      10     1     1     2     5
      # i 490 more rows

---

    Code
      gen_gridcube(n = 500, p = 10)
    Message
      v Data generation completed successfully!!!
    Output
      # A tibble: 504 x 10
            x1    x2    x3    x4    x5    x6    x7    x8    x9   x10
         <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
       1     1     1     1     1     1     1     1     1     1     1
       2     1     1     1     1     1     1     1     1     1     2
       3     1     1     1     1     1     1     1     1     1     3
       4     1     1     1     1     1     1     1     1     1     4
       5     1     1     1     1     1     1     1     1     1     5
       6     1     1     1     1     1     1     1     1     1     6
       7     1     1     1     1     1     1     1     1     1     7
       8     1     1     1     1     1     1     1     1     2     1
       9     1     1     1     1     1     1     1     1     2     2
      10     1     1     1     1     1     1     1     1     2     3
      # i 494 more rows

# gen_unifcube() works

    Code
      gen_unifcube(n = 500, p = 4)
    Message
      v 1 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
              x1       x2      x3      x4
           <dbl>    <dbl>   <dbl>   <dbl>
       1  0.406   0.0696  -0.240   0.0855
       2 -0.143  -0.402    0.308  -0.0113
       3  0.401  -0.0344   0.147   0.0273
       4  0.0590 -0.0613  -0.0920 -0.0691
       5 -0.437  -0.328    0.152   0.0850
       6  0.0984 -0.179   -0.0313  0.0225
       7  0.499   0.426    0.141   0.0739
       8  0.399  -0.00869 -0.469   0.0253
       9  0.100   0.412   -0.0288  0.0249
      10 -0.434  -0.195    0.157  -0.0598
      # i 490 more rows

---

    Code
      gen_unifcube(n = 500, p = 10)
    Message
      v 7 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 10
              x1     x2      x3       x4        x5       x6       x7       x8       x9
           <dbl>  <dbl>   <dbl>    <dbl>     <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
       1 -0.260   0.427 -0.131  -0.0587   -2.05e-2 -0.0480   0.0190  -0.00249 -0.0442 
       2  0.346  -0.381 -0.458   0.0118    9.97e-2  0.0129   0.0747   0.0109  -0.0102 
       3 -0.315   0.166 -0.407   0.0187    4.97e-4 -0.00749  0.0717  -0.115    0.00812
       4  0.0523 -0.429  0.469   0.0312    8.01e-2  0.00171  0.0666   0.0821  -0.00841
       5 -0.408  -0.459  0.321  -0.0252    1.43e-2  0.0632  -0.0289   0.0177   0.00639
       6  0.431  -0.421 -0.308   0.00537   2.84e-2  0.0334  -0.0922   0.00983 -0.0254 
       7  0.0380  0.108  0.0825  0.00631   3.62e-2 -0.0173  -0.0174   0.00881 -0.0207 
       8  0.239   0.481 -0.466   0.0284    3.31e-2  0.0186  -0.00326 -0.0967   0.0584 
       9 -0.475   0.169  0.354   0.0375   -8.08e-2 -0.0271   0.0680   0.0269   0.0784 
      10  0.249   0.469 -0.457   0.0361    8.44e-5 -0.0266   0.0218  -0.0302  -0.0336 
      # i 490 more rows
      # i 1 more variable: x10 <dbl>

# gen_cubehole() works

    Code
      gen_cubehole(n = 1000, p = 4)
    Message
      v 1 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 494 x 4
              x1       x2     x3       x4
           <dbl>    <dbl>  <dbl>    <dbl>
       1 -0.143  -0.402    0.308 -0.0515 
       2 -0.437  -0.328    0.152 -0.0513 
       3  0.499   0.426    0.141 -0.0740 
       4  0.399  -0.00869 -0.469 -0.0308 
       5 -0.434  -0.195    0.157 -0.00886
       6 -0.403   0.133   -0.487  0.0234 
       7 -0.347   0.409   -0.257 -0.0849 
       8  0.426   0.0439  -0.314  0.0198 
       9  0.0540  0.418   -0.310 -0.00184
      10  0.384   0.395    0.489 -0.0188 
      # i 484 more rows

---

    Code
      gen_cubehole(n = 1000, p = 10)
    Message
      v 7 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 538 x 10
               x1      x2     x3       x4       x5       x6       x7       x8       x9
            <dbl>   <dbl>  <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
       1 -0.159    0.149   0.477 -0.00249 -9.00e-2 -0.00801  0.00365  0.0249  -0.0234 
       2  0.446    0.349   0.112 -0.115   -1.11e-1 -0.0479  -0.0968   0.0196   0.0626 
       3 -0.286    0.215   0.372  0.0821   5.22e-2 -0.0232  -0.0397  -0.0460  -0.0758 
       4 -0.315   -0.447  -0.210  0.00983  6.13e-4  0.0758   0.00497 -0.0225   0.0457 
       5  0.0851  -0.175   0.481  0.0269  -1.89e-2  0.0571  -0.0539  -0.0223   0.0152 
       6  0.473    0.0148  0.335  0.0531  -3.02e-2  0.0772   0.00551 -0.0348  -0.0716 
       7 -0.175    0.441   0.440 -0.0577   2.27e-2 -0.0275  -0.0549  -0.00880 -0.00847
       8  0.00294  0.347  -0.489  0.0728   7.02e-3 -0.0341   0.0154   0.00775 -0.0143 
       9 -0.483   -0.454   0.280 -0.0479   1.58e-2 -0.00131  0.0159  -0.0466   0.0790 
      10  0.286   -0.370   0.181  0.0704   8.28e-2 -0.0228   0.0183  -0.0827   0.0232 
      # i 528 more rows
      # i 1 more variable: x10 <dbl>

