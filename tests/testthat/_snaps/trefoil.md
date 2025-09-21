# gen_trefoil4d() works

    Code
      gen_trefoil4d(n = 500, p = 4, steps = 5)
    Message
      v Data generation completed successfully!!!
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
      v Data generation completed successfully!!!
      v 1 noise dimensions have been generated successfully!!!
      v Data generation completed successfully!!!
    Output
      # A tibble: 500 x 4
            x1    x2    x3       x4
         <dbl> <dbl> <dbl>    <dbl>
       1 1     0         0 -0.0659 
       2 0.992 0.127     0  0.0321 
       3 0.968 0.251     0  0.0646 
       4 0.928 0.372     0 -0.0642 
       5 0.874 0.486     0 -0.0189 
       6 0.805 0.593     0  0.00771
       7 0.724 0.690     0  0.0766 
       8 0.631 0.776     0 -0.0195 
       9 0.527 0.850     0  0.0232 
      10 0.415 0.910     0 -0.149  
      # i 490 more rows

