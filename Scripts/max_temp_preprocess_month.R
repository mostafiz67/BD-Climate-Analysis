library(dplyr)
library(tidyr)  
library(ggplot2)

#Making subset for Provisional Standard Normal Value
pro_sta_nor <- subset(data, YEAR >= 1948 & YEAR <=1959)

total_max_temp_month_for_month_PSN <- pro_sta_nor %>% 
  group_by(Month) %>% 
  summarise(
    total_max_temp_month = sum(Max.Temp),
    avg_max_temp_month = mean(Max.Temp)
  )

avg_max_temp_month_for_month_psn_compare <- mean(total_max_temp_month_for_month_PSN$avg_max_temp_month)

#Making subset for Predicting Value
predict_max_temp <- subset(data, YEAR >= 1960 & YEAR <=2013)

total_max_temp_month_for_month_predict <- predict_max_temp %>% 
  group_by(Month) %>% 
  summarise(
    total_max_temp_month_predict = sum(Max.Temp),
    avg_max_temp_month_predict = mean(Max.Temp)
  )

#plotting graph for the predicting Rainfall
plot_avg_total_max_temp_month_predict <- ggplot(data=total_max_temp_month_for_month_predict,
                                                aes(x=Month, y=avg_max_temp_month_predict))+
  geom_line()+
  geom_point(shape=22, size=3, colour="darkred", fill="pink")+
  theme_minimal()+
  xlab("Month")+
  ylab("Max Temp")+
  ggtitle("Max Temp vs Month") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Max Temp vs Month Line Graph.pdf")
print(plot_avg_total_max_temp_month_predict) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Max Temp vs Month Line Graph.png", plot = plot_avg_total_max_temp_month_predict +
         theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')


#Calculating the average for the whole predict period
avg_max_temp_month_predict_compare <- mean(total_max_temp_month_for_month_predict$avg_max_temp_month_predict)

#Calculating the difference between PSN and predicted
total_max_temp_month_for_month_predict$diff_btn_month_predicted_psn <- total_max_temp_month_for_month_predict$avg_max_temp_month_predict - avg_max_temp_month_for_month_psn_compare

#Plotting the difference
plot_diff_month_predict_psn_max_temp_col <- ggplot(data=total_max_temp_month_for_month_predict)+
  geom_col(aes(x=Month, y=diff_btn_month_predicted_psn, fill=factor(Month)))+
  theme_minimal()+
  xlab("Month")+
  ylab("Average Maximum Temperature (oC)")+
  ggtitle("Average Maximum Temperature Anomaly (Jan-Dec)")

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Average Maximum Temperature Anomaly (Jan-Dec).pdf")
print(plot_diff_month_predict_psn_max_temp_col) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Average Maximum Temperature Anomaly (Jan-Dec).png", plot = plot_diff_month_predict_psn_max_temp_col +
         theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')

