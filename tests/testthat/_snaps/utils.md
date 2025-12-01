# gla_load_points snapshot

    Code
      as.data.frame(sf::st_drop_geometry(result))
    Output
        elevation x_meters y_meters       lon     lat
      1       250  1022655   574704 -125.6827 50.1876

# add_las_filename snapshot

    Code
      result_snapshot
    Output
      # A tibble: 1 x 3
        x_meters y_meters las_files      
           <dbl>    <dbl> <chr>          
      1     1000    2000. 1000_2000.5.las

