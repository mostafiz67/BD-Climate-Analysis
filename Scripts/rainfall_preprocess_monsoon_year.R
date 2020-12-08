library(dplyr)
library(tidyr)  
library(ggplot2)

#Making subset for Provisional Standard Normal Value
pro_sta_nor <- subset(data, YEAR >= 1949 & YEAR <=1959)

total_rainfall_monsoon_PSN <- pro_sta_nor %>% 
  group_by(YEAR, Monsoon, Month) %>% 
  summarise(
    total_rainfall_monsoon = sum(Rainfall),
    avg_rainfall_monsoon = mean(Rainfall)
  )

total_rainfall_monsoon_year_PSN <- total_rainfall_monsoon_PSN %>% 
  group_by(YEAR, Monsoon) %>% 
  summarise(
    total_rainfall_monsoon = sum(avg_rainfall_monsoon),
    avg_rainfall_monsoon = mean(avg_rainfall_monsoon)
  )

final_rainfall_monsoon_PSN <- total_rainfall_monsoon_year_PSN %>% 
  group_by(Monsoon) %>% 
  summarise(
    total_rainfall_monsoon = sum(total_rainfall_monsoon),
    avg_rainfall_monsoon = mean(avg_rainfall_monsoon)
  )

avg_rainfall_monsoon_psn_compare <- mean(final_rainfall_monsoon_PSN$total_rainfall_monsoon)

#Making subset for Predicting Value
predict_rainfall <- subset(data, YEAR >= 1960 & YEAR <=2013)

total_rainfall_monsoon_predict <- predict_rainfall %>% 
  group_by(YEAR, Monsoon, Month) %>% 
  summarise(
    total_rainfall_monsoon_predict = sum(Rainfall),
    avg_rainfall_monsoon_predict = mean(Rainfall)
  )

total_and_avg_rainfall_year_predict <- total_rainfall_monsoon_predict %>% 
  group_by(YEAR, Monsoon) %>% 
  summarise(
    total_rainfall_monsoon_year_predict = sum(avg_rainfall_monsoon_predict),
    avg_rainfall_monsoon_predict = mean(avg_rainfall_monsoon_predict)
  )

final_rainfall_monsoon_predict <- total_and_avg_rainfall_year_predict %>% 
  group_by(Monsoon) %>% 
  summarise(
    total_rainfall_monsoon = sum(total_rainfall_monsoon_year_predict),
    avg_rainfall_monsoon = mean(avg_rainfall_monsoon_predict)
  )
avg_rainfall_monsoon_predict_compare <- mean(final_rainfall_monsoon_predict$total_rainfall_monsoon)

#Comparison Column Graph
plot_df_PSN_analysis_rainfall_monsoon <- final_rainfall_monsoon_predict
plot_df_PSN_analysis_rainfall_monsoon$avg_rainfall_monsoon_psn <- final_rainfall_monsoon_PSN$avg_rainfall_monsoon

plot_final_df_PSN_analysis_rainfall_monsoon <- plot_df_PSN_analysis_rainfall_monsoon %>% gather(key = F_Monsoon, value = avg_rainfall_year_monsoon_psn_predict, avg_rainfall_monsoon:avg_rainfall_monsoon_psn)


#plotting graph for the predicting Rainfall
plot_compare_psn_analysis_rainfall_monsoon <- ggplot(plot_final_df_PSN_analysis_rainfall_monsoon, 
                                                     aes(Monsoon, avg_rainfall_year_monsoon_psn_predict, fill = F_Monsoon))+
  geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.9))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  xlab("Monsoon")+
  ylab("Avg Rainfall")+
  ggtitle("Avg Rainfall vs Monsoon")


plot_df_PSN_analysis_rainfall_monsoon$diff <- plot_df_PSN_analysis_rainfall_monsoon$avg_rainfall_monsoon - plot_df_PSN_analysis_rainfall_monsoon$avg_rainfall_monsoon_psn

#plotting graph for the predicting Rainfall
plot_compare_psn_analysis_rainfall_monsoon <- ggplot(plot_df_PSN_analysis_rainfall_monsoon, 
                                                     aes(Monsoon, diff, fill = Monsoon))+
  geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.9))+
  theme_bw(base_size = 10, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        plot.title = element_text(color = "black", face = "bold"),
        axis.text.x = element_text(colour="black",size=10,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold")
  )+
  theme(panel.grid = element_blank())+
  xlab("Monsoon")+
  ylab("Average Monsoon Rainfall (mm)")+
  ggtitle("Average Monsson Rainfall Anomaly in mm (1960-2013)") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Average Monsson Rainfall Anomaly in mm (1960-2013).pdf")
print(plot_compare_psn_analysis_rainfall_monsoon) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Final/Average Monsson Rainfall Anomaly in mm (1960-2013).png", plot = plot_compare_psn_analysis_rainfall_monsoon +
         theme(), width = 10, height = 8, dpi = 350, units = "in", device='png')
