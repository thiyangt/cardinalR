# gen_trefoil4d() works

    Code
      gen_trefoil4d(n = 500, p = 4, steps = 5)
    Message
      v Data generation completed successfully! 🎉
    Output
      # A tibble: 500 x 4
            x1    x2    x3    x4
         <dbl> <dbl> <dbl> <dbl>
       1 1     0         0     0
       2 0.992 0.127     0     0
       3 0.968 0.251     0     0
       4 0.928 0.372     0     0
       5 0.874 0.486     0     0
       6 0.805 0.593     0     0
       7 0.724 0.690     0     0
       8 0.631 0.776     0     0
       9 0.527 0.850     0     0
      10 0.415 0.910     0     0
      # i 490 more rows

# gen_trefoil3d() works

    Code
      gen_trefoil3d(n = 500, p = 4, steps = 5)
    Message
      v Data generation completed successfully! 🎉
      v 1 noise dimensions have been generated successfully! 🎉
      v Data generation completed successfully! 🎉
    Output
      # A tibble: 500 x 4
            x1    x2    x3      x4
         <dbl> <dbl> <dbl>   <dbl>
       1 1     0         0 -0.132 
       2 0.992 0.127     0  0.0642
       3 0.968 0.251     0  0.129 
       4 0.928 0.372     0 -0.128 
       5 0.874 0.486     0 -0.0378
       6 0.805 0.593     0  0.0154
       7 0.724 0.690     0  0.153 
       8 0.631 0.776     0 -0.0391
       9 0.527 0.850     0  0.0464
      10 0.415 0.910     0 -0.298 
      # i 490 more rows

