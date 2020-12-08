library(dplyr)
library(tidyr)  
library(ggplot2)

#Making subset for Provisional Standard Normal Value
pro_sta_nor <- subset(data, YEAR >= 1948 & YEAR <=1967)

total_Min_temp_Region_PSN <- pro_sta_nor %>% 
  group_by(YEAR, Region) %>% 
  summarise(
    total_Min_temp_Region = sum(Min.Temp),
    avg_Min_temp_Region = mean(Min.Temp)
  )

avg_Min_temp_Region_PSN <- total_Min_temp_Region_PSN %>% 
  group_by(Region) %>% 
  summarise(
    total_Min_temp_Region = sum(avg_Min_temp_Region),
    avg_Min_temp_Region = mean(avg_Min_temp_Region)
  )

avg_min_temp_region_psn_compare <- mean(avg_Min_temp_Region_PSN$avg_Min_temp_Region)

#avg_Min_temp_month_psn_compare <- mean(total_Min_temp_month_PSN$avg_Min_temp_month)

#Making subset for Predicting Value
predict_Min_temp <- subset(data, YEAR >= 1968 & YEAR <=2013)


total_Min_temp_Region_predict <- predict_Min_temp %>% 
  group_by(YEAR, Region) %>% 
  summarise(
    total_Min_temp_Region_predict = sum(Min.Temp),
    avg_Min_temp_Region_predict = mean(Min.Temp)
  )

total_and_avg_Min_temp_year_Region_predict <- total_Min_temp_Region_predict %>% 
  group_by(Region) %>% 
  summarise(
    total_Min_temp_year_Region_predict = sum(avg_Min_temp_Region_predict),
    avg_Min_temp_year_Region_predict = mean(avg_Min_temp_Region_predict)
  )

#Calculating the average for the whole predict period
avg_min_temp_region_predict_compare <- mean(total_and_avg_Min_temp_year_Region_predict$avg_Min_temp_year_Region_predict)

#Comparison Column Graph
plot_df_PSN_analysis_Min_temp_Region <- total_and_avg_Min_temp_year_Region_predict
plot_df_PSN_analysis_Min_temp_Region$avg_Min_temp_Region_psn <- avg_Min_temp_Region_PSN$avg_Min_temp_Region

plot_final_df_PSN_analysis_Min_temp_Region <- plot_df_PSN_analysis_Min_temp_Region %>% gather(key = F_Region, value = avg_Min_temp_year_Region_psn_predict, avg_Min_temp_year_Region_predict:avg_Min_temp_Region_psn)


plot_compare_psn_analysis_Min_temp_Region <- ggplot(plot_final_df_PSN_analysis_Min_temp_Region, 
                                                    aes(Region, avg_Min_temp_year_Region_psn_predict, fill = F_Region ))+
  geom_col(position = "dodge") +
  theme_minimal()+
  xlab("Region")+
  ylab("Avg Min Temp")+
  ggtitle("Avg Min Temp vs Region") 


pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Min Temp vs Region Bar Graph.pdf")
print(plot_compare_psn_analysis_Min_temp_Region) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Min Temp vs Region Bar.png", plot = plot_compare_psn_analysis_Min_temp_Region +
         theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')




plot_df_PSN_analysis_Min_temp_Region$diff <- plot_df_PSN_analysis_Min_temp_Region$avg_Min_temp_year_Region_predict - plot_df_PSN_analysis_Min_temp_Region$avg_Min_temp_Region_psn
plot_diff_compare_psn_analysis_Min_temp_Region <- ggplot(plot_df_PSN_analysis_Min_temp_Region, 
                                                    aes(Region, diff, fill = Region ))+
  geom_col(position = "dodge") +
  theme_minimal()+
  xlab("Region")+
  ylab("Average Minimum Temperature (oC)")+
  ggtitle("Average Minimum Temperature Anomaly (Region)")


pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Average Minimum Temperature Anomaly (Region).pdf")
print(plot_diff_compare_psn_analysis_Min_temp_Region) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Average Minimum Temperature Anomaly (Region).png", plot = plot_diff_compare_psn_analysis_Min_temp_Region +
         theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')


