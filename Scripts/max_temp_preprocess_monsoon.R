library(dplyr)
library(tidyr)  
library(ggplot2)

#Making subset for Provisional Standard Normal Value
pro_sta_nor <- subset(data, YEAR >= 1948 & YEAR <=1967)

total_max_temp_monsoon_PSN <- pro_sta_nor %>% 
  group_by(YEAR, Monsoon) %>% 
  summarise(
    total_max_temp_monsoon = sum(Max.Temp),
    avg_max_temp_monsoon = mean(Max.Temp)
  )

avg_max_temp_monsoon_PSN <- total_max_temp_monsoon_PSN %>% 
  group_by(Monsoon) %>% 
  summarise(
    total_max_temp_monsoon = sum(avg_max_temp_monsoon),
    avg_max_temp_monsoon = mean(avg_max_temp_monsoon)
  )


avg_max_temp_monsoon_psn_compare <- mean(avg_max_temp_monsoon_PSN$avg_max_temp_monsoon)


#avg_max_temp_month_psn_compare <- mean(total_max_temp_month_PSN$avg_max_temp_month)

#Making subset for Predicting Value
predict_max_temp <- subset(data, YEAR >= 1968 & YEAR <=2013)


total_max_temp_monsoon_predict <- predict_max_temp %>% 
  group_by(YEAR, Monsoon) %>% 
  summarise(
    total_max_temp_monsoon_predict = sum(Max.Temp),
    avg_max_temp_monsoon_predict = mean(Max.Temp)
  )

total_and_avg_max_temp_year_monsoon_predict <- total_max_temp_monsoon_predict %>% 
  group_by(Monsoon) %>% 
  summarise(
    total_max_temp_year_monsoon_predict = sum(avg_max_temp_monsoon_predict),
    avg_max_temp_year_monsoon_predict = mean(avg_max_temp_monsoon_predict)
  )


avg_max_temp_monsoon_predict_compare <- mean(total_and_avg_max_temp_year_monsoon_predict$avg_max_temp_year_monsoon_predict)

#Comparison Column Graph
plot_df_PSN_analysis_max_temp_monsoon <- total_and_avg_max_temp_year_monsoon_predict
plot_df_PSN_analysis_max_temp_monsoon$avg_max_temp_monsoon_psn <- avg_max_temp_monsoon_PSN$avg_max_temp_monsoon

plot_final_df_PSN_analysis_max_temp_monsoon <- plot_df_PSN_analysis_max_temp_monsoon %>% gather(key = F_Monsoon, value = avg_max_temp_year_monsoon_psn_predict, avg_max_temp_year_monsoon_predict:avg_max_temp_monsoon_psn)


plot_compare_psn_analysis_max_temp_monsoon <- ggplot(plot_final_df_PSN_analysis_max_temp_monsoon, 
                                                     aes(Monsoon, avg_max_temp_year_monsoon_psn_predict, fill = F_Monsoon ))+
  geom_col(position = "dodge") +
  theme_minimal()+
  xlab("Monsoon")+
  ylab("Avg Max Temp")+
  ggtitle("Avg Max Temp vs Monsoon") 


pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Max Temp vs Monsoon Bar Graph.pdf")
print(plot_compare_psn_analysis_max_temp_monsoon) 
dev.off()




plot_df_PSN_analysis_max_temp_monsoon <- total_and_avg_max_temp_year_monsoon_predict
plot_df_PSN_analysis_max_temp_monsoon$avg_max_temp_monsoon_psn <- avg_max_temp_monsoon_PSN$avg_max_temp_monsoon

plot_final_df_PSN_analysis_max_temp_monsoon <- plot_df_PSN_analysis_max_temp_monsoon %>% gather(key = F_Monsoon, value = avg_max_temp_year_monsoon_psn_predict, avg_max_temp_year_monsoon_predict:avg_max_temp_monsoon_psn)

plot_df_PSN_analysis_max_temp_monsoon$diff <- plot_df_PSN_analysis_max_temp_monsoon$avg_max_temp_year_monsoon_predict - plot_df_PSN_analysis_max_temp_monsoon$avg_max_temp_monsoon_psn


plot_diff_compare_psn_analysis_max_temp_monsoon <- ggplot(plot_df_PSN_analysis_max_temp_monsoon, 
                                                     aes(Monsoon, diff, fill = factor(Monsoon) ))+
  geom_col(position = "dodge") +
  theme_minimal()+
  xlab("Monsoon")+
  ylab("Average Maximum Temperature (oC)")+
  ggtitle("Average Maximum Temperature Anomaly (Monsoon)") 


pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Average Maximum Temperature Anomaly (Monsoon).pdf")
print(plot_diff_compare_psn_analysis_max_temp_monsoon) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Average Maximum Temperature Anomaly (Monsoon).png", plot = plot_diff_compare_psn_analysis_max_temp_monsoon +
         theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')


