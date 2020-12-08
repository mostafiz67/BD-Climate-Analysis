library(dplyr)
library(tidyr)  
library(ggplot2)

#Making subset for Provisional Standard Normal Value
pro_sta_nor <- subset(data, YEAR >= 1948 & YEAR <=1967)

total_max_temp_Region_PSN <- pro_sta_nor %>% 
  group_by(YEAR, Region) %>% 
  summarise(
    total_max_temp_Region = sum(Max.Temp),
    avg_max_temp_Region = mean(Max.Temp)
  )

avg_max_temp_Region_PSN <- total_max_temp_Region_PSN %>% 
  group_by(Region) %>% 
  summarise(
    total_max_temp_Region = sum(avg_max_temp_Region),
    avg_max_temp_Region = mean(avg_max_temp_Region)
  )

avg_max_temp_region_psn_compare <- mean(avg_max_temp_Region_PSN$avg_max_temp_Region)

#avg_max_temp_month_psn_compare <- mean(total_max_temp_month_PSN$avg_max_temp_month)

#Making subset for Predicting Value
predict_max_temp <- subset(data, YEAR >= 1968 & YEAR <=2013)


total_max_temp_Region_predict <- predict_max_temp %>% 
  group_by(YEAR, Region) %>% 
  summarise(
    total_max_temp_Region_predict = sum(Max.Temp),
    avg_max_temp_Region_predict = mean(Max.Temp)
  )

total_and_avg_max_temp_year_Region_predict <- total_max_temp_Region_predict %>% 
  group_by(Region) %>% 
  summarise(
    total_max_temp_year_Region_predict = sum(avg_max_temp_Region_predict),
    avg_max_temp_year_Region_predict = mean(avg_max_temp_Region_predict)
  )

avg_max_temp_region_predict_compare <- mean(total_and_avg_max_temp_year_Region_predict$avg_max_temp_year_Region_predict)

#Comparison Column Graph
plot_df_PSN_analysis_max_temp_Region <- total_and_avg_max_temp_year_Region_predict
plot_df_PSN_analysis_max_temp_Region$avg_max_temp_Region_psn <- avg_max_temp_Region_PSN$avg_max_temp_Region

plot_final_df_PSN_analysis_max_temp_Region <- plot_df_PSN_analysis_max_temp_Region %>% gather(key = F_Region, value = avg_max_temp_year_Region_psn_predict, avg_max_temp_year_Region_predict:avg_max_temp_Region_psn)


plot_compare_psn_analysis_max_temp_Region <- ggplot(plot_final_df_PSN_analysis_max_temp_Region, 
                                                     aes(Region, avg_max_temp_year_Region_psn_predict, fill = F_Region ))+
  geom_col(position = "dodge") +
  theme_minimal()+
  xlab("Region")+
  ylab("Average Maximum Temperature (oC)")+
  ggtitle("Average Maximum Temperature Anomaly (Region)") 


pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Average Maximum Temperature Anomaly (Region).pdf")
print(plot_compare_psn_analysis_max_temp_Region) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Average Maximum Temperature Anomaly (Region).png", plot = plot_compare_psn_analysis_max_temp_Region +
         theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')





#Comparison Column Graph
plot_df_PSN_analysis_max_temp_Region <- total_and_avg_max_temp_year_Region_predict
plot_df_PSN_analysis_max_temp_Region$avg_max_temp_Region_psn <- avg_max_temp_Region_PSN$avg_max_temp_Region

# plot_final_df_PSN_analysis_max_temp_Region <- plot_df_PSN_analysis_max_temp_Region %>% gather(key = F_Region, value = avg_max_temp_year_Region_psn_predict, avg_max_temp_year_Region_predict:avg_max_temp_Region_psn)

plot_df_PSN_analysis_max_temp_Region$diff <- plot_df_PSN_analysis_max_temp_Region$avg_max_temp_year_Region_predict - plot_df_PSN_analysis_max_temp_Region$avg_max_temp_Region_psn

#Plotting the difference
diff_plot_diff_year_predict_psn_max_temp_col <- ggplot(data=plot_df_PSN_analysis_max_temp_Region)+
  geom_col(aes(x=Region, y=diff, fill=factor(Region)))+
  theme_bw(base_size = 10, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        plot.title = element_text(color = "black", face = "bold"),
        axis.text.x = element_text(colour="black",size=8,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold")
  )+
  xlab("Region")+
  ylab("Average Maximum Temperature (oC)")+
  ggtitle("Average Maximum Temperature Anomaly (Region)") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Average Maximum Temperature Anomaly (Region).pdf")
print(diff_plot_diff_year_predict_psn_max_temp_col) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Final/Average Maximum Temperature Anomaly (Region).png", plot = diff_plot_diff_year_predict_psn_max_temp_col +
         theme(), width = 10, height = 8, dpi = 350, units = "in", device='png')


