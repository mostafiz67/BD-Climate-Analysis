library(dplyr)
library(tidyr)  
library(ggplot2)

#Making subset for Provisional Standard Normal Value
pro_sta_nor <- subset(data, YEAR >= 1949 & YEAR <=1959)

total_rainfall_Region_PSN <- pro_sta_nor %>% 
  group_by(YEAR, Region, Month) %>% 
  summarise(
    total_rainfall_Region = sum(Rainfall),
    avg_rainfall_region_analysis = mean(Rainfall)
  )

total_rainfall_Region_year_PSN <- total_rainfall_Region_PSN %>% 
  group_by(YEAR, Region) %>% 
  summarise(
    total_rainfall_Region = sum(avg_rainfall_region_analysis),
    avg_rainfall_region_analysis = mean(avg_rainfall_region_analysis)
  )

final_rainfall_Region_PSN <- total_rainfall_Region_year_PSN %>% 
  group_by(Region) %>% 
  summarise(
    total_rainfall_Region = sum(total_rainfall_Region),
    avg_rainfall_region_analysis = mean(avg_rainfall_region_analysis)
  )

avg_rainfall_region_psn_compare <- mean(final_rainfall_Region_PSN$total_rainfall_Region)

#Making subset for Predicting Value
predict_rainfall <- subset(data, YEAR >= 1960 & YEAR <=2013)

total_rainfall_Region_predict <- predict_rainfall %>% 
  group_by(YEAR, Region, Month) %>% 
  summarise(
    total_rainfall_Region_predict = sum(Rainfall),
    avg_rainfall_Region_predict = mean(Rainfall)
  )

total_and_avg_rainfall_year_predict <- total_rainfall_Region_predict %>% 
  group_by(YEAR, Region) %>% 
  summarise(
    total_rainfall_Region_year_predict = sum(avg_rainfall_Region_predict),
    avg_rainfall_Region_predict = mean(avg_rainfall_Region_predict)
  )

final_rainfall_Region_predict <- total_and_avg_rainfall_year_predict %>% 
  group_by(Region) %>% 
  summarise(
    total_rainfall_Region = sum(total_rainfall_Region_year_predict),
    avg_rainfall_region_analysis = mean(avg_rainfall_Region_predict)
  )
avg_rainfall_region_predict_compare <- mean(final_rainfall_Region_predict$total_rainfall_Region)

#Comparison Column Graph
plot_df_PSN_analysis_rainfall_Region <- final_rainfall_Region_predict
plot_df_PSN_analysis_rainfall_Region$avg_rainfall_Region_psn <- final_rainfall_Region_PSN$avg_rainfall_region_analysis

plot_final_df_PSN_analysis_rainfall_Region <- plot_df_PSN_analysis_rainfall_Region %>% gather(key = F_Region, value = avg_rainfall_year_Region_psn_predict, avg_rainfall_region_analysis:avg_rainfall_Region_psn)


#plotting graph for the predicting Rainfall
plot_compare_psn_analysis_rainfall_Region <- ggplot(plot_final_df_PSN_analysis_rainfall_Region, 
                                                     aes(Region, avg_rainfall_year_Region_psn_predict, fill = F_Region))+
  geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.9))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  xlab("Region")+
  ylab("Avg Rainfall")+
  ggtitle("Avg Rainfall vs Region")


plot_df_PSN_analysis_rainfall_Region$diff <- plot_df_PSN_analysis_rainfall_Region$avg_rainfall_region_analysis - plot_df_PSN_analysis_rainfall_Region$avg_rainfall_Region_psn


#plotting graph for the predicting Rainfall
plot_diff_compare_psn_analysis_rainfall_Region <- ggplot(plot_df_PSN_analysis_rainfall_Region, 
                                                    aes(Region, diff, fill = Region))+
  geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.9))+
  theme_bw(base_size = 10, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        plot.title = element_text(color = "black", face = "bold"),
        axis.text.x = element_text(colour="black",size=8,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold")
  )+
  xlab("Region")+
  ylab("Average Region Rainfall (mm)")+
  ggtitle("Average Region Ranfall Anomaly in mm (1960-2013)") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Average Region Ranfall Anomaly in mm (1960-2013).pdf")
print(plot_diff_compare_psn_analysis_rainfall_Region) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Final/Average Region Ranfall Anomaly in mm (1960-2013).png", plot = plot_diff_compare_psn_analysis_rainfall_Region +
         theme(), width = 10, height = 8, dpi = 300, units = "in", device='png')
