

avg_max_temp_year <- data %>% 
  group_by(YEAR) %>% 
  summarise(
    temp_avg_max_year = mean(Max.Temp),
    temp_avg_min_year = mean(Min.Temp)
  )

avg_max_temp_year$temp_dif_avg_max = avg_max_temp_year$temp_avg_max_year - whole_avg_max_temp
avg_max_temp_year$whole_avg_min_temp = avg_max_temp_year$temp_avg_min_year - whole_avg_min_temp
