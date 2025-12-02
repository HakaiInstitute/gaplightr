# gla_compute_solar_positions returns expected structure

    Code
      list(solar_mat_dim = dim(solar_data$solar_mat), solar_mat_colnames = colnames(
        solar_data$solar_mat), solar_mat_head = head(solar_data$solar_mat, 3),
      beam_array_dim = dim(solar_data$beam_array), day_mat_dim = dim(solar_data$
        day_mat), Total_rbi = solar_data$Total_rbi)
    Output
      $solar_mat_dim
      [1] 132145     11
      
      $solar_mat_colnames
       [1] "DAY_NUM"           "ZENITH"            "AZIMUTH"          
       [4] "X_SUN"             "Y_SUN"             "EoT_MIN"          
       [7] "TIME_CORR_MIN"     "SOLAR_TIME_HR"     "LOCAL_STD_TIME_HR"
      [10] "EXTRA_Wm2"         "REL_BEAM"         
      
      $solar_mat_head
        DAY_NUM   ZENITH  AZIMUTH    X_SUN      Y_SUN   EoT_MIN TIME_CORR_MIN
      1       1 89.85343 127.9371 1.236848 -0.9641467 -2.904169     -25.63497
      2       1 89.60160 128.3221 1.226893 -0.9697108 -2.904169     -25.63497
      3       1 89.35111 128.7080 1.216922 -0.9752181 -2.904169     -25.63497
        SOLAR_TIME_HR LOCAL_STD_TIME_HR EXTRA_Wm2     REL_BEAM
      1      8.066667          8.493916  3.619521 8.559583e-10
      2      8.100000          8.527249  9.838249 9.035404e-09
      3      8.133333          8.560583 16.023809 4.800495e-08
      
      $beam_array_dim
      [1] 18 72
      
      $day_mat_dim
      [1] 365   7
      
      $Total_rbi
      [1] 24284.55
      

# gla_process_fisheye_photo_single output matches snapshot

    Code
      results_df
    Output
                                              fisheye_photo_path canopy_openness_pct
      1 testdata/R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp            31.84485
        mean_daily_extraterrestrial_irradiance_Wm2
      1                                   284.5328
        mean_daily_direct_irradiation_MJm2d mean_daily_diffuse_irradiation_MJm2d
      1                            4.091251                             6.971384
        mean_daily_global_irradiation_MJm2d transmitted_direct_irradiation_MJm2d
      1                            11.06264                             1.606614
        transmitted_diffuse_irradiation_MJm2d transmitted_global_irradiation_MJm2d
      1                              3.431275                             5.037888
        transmitted_direct_irradiation_pct transmitted_diffuse_irradiation_pct
      1                           39.26949                            49.21942
        transmitted_global_irradiation_pct subcanopy_solar_radiation_MJm2d
      1                           45.53968                        5.037888
        light_penetration_index
      1               0.4553968

# gla_process_fisheye_photos snapshot

    Code
      as.data.frame(sf::st_drop_geometry(result))
    Output
                                              fisheye_photo_path stream     lat
      1 testdata/R2D2_ps10_cex0pt3_600dpi_2800px_polar_cairo.bmp   R2D2 50.1876
              lon elevation canopy_openness_pct
      1 -125.6827    238.44            31.84485
        mean_daily_extraterrestrial_irradiance_Wm2
      1                                   88.19772
        mean_daily_direct_irradiation_MJm2d mean_daily_diffuse_irradiation_MJm2d
      1                            1.268181                             2.160947
        mean_daily_global_irradiation_MJm2d transmitted_direct_irradiation_MJm2d
      1                            3.429128                            0.2141381
        transmitted_diffuse_irradiation_MJm2d transmitted_global_irradiation_MJm2d
      1                              1.063605                             1.277743
        transmitted_direct_irradiation_pct transmitted_diffuse_irradiation_pct
      1                           16.88546                            49.21942
        transmitted_global_irradiation_pct subcanopy_solar_radiation_MJm2d
      1                           37.26147                        1.277743
        light_penetration_index
      1               0.3726147

