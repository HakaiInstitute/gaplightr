# gla_extract_horizon_terra matches GRASS reference data

    Code
      list(rmse = round(rmse, 4), mae = round(mae, 4), max_abs_error = round(
        max_abs_error, 4), worst_matches = data.frame(azimuth = horizon_terra$azimuth[
        worst_idx], terra = round(horizon_terra$horizon_height[worst_idx], 4), grass = round(
        dat$horizon_height[worst_idx], 4), diff = round(horizon_diff[worst_idx], 4)))
    Output
      $rmse
      [1] 0.1855
      
      $mae
      [1] 0.1219
      
      $max_abs_error
      [1] 0.6621
      
      $worst_matches
        azimuth   terra   grass    diff
      1     165 10.4160 11.0781 -0.6621
      2     220 10.1896  9.6085  0.5811
      3     225  8.5778  8.0888  0.4891
      4     150  6.0329  6.5087 -0.4759
      5     240  4.4446  4.0806  0.3640
      

