library(dplyr)
library(tidyr)  
library(ggplot2)

#Making subset for Provisional Standard Normal Value
pro_sta_nor <- subset(data, YEAR >= 1948 & YEAR <=1959)

total_Min_temp_month_for_month_PSN <- pro_sta_nor %>% 
  group_by(Month) %>% 
  summarise(
    total_Min_temp_month = sum(Min.Temp),
    avg_Min_temp_month = mean(Min.Temp)
  )

avg_Min_temp_month_for_month_psn_compare <- mean(total_Min_temp_month_for_month_PSN$avg_Min_temp_month)

#Making subset for Predicting Value
predict_Min_temp <- subset(data, YEAR >= 1960 & YEAR <=2013)

total_Min_temp_month_for_month_predict <- predict_Min_temp %>% 
  group_by(Month) %>% 
  summarise(
    total_Min_temp_month_predict = sum(Min.Temp),
    avg_Min_temp_month_predict = mean(Min.Temp)
  )

#plotting graph for the predicting Rainfall
plot_avg_total_Min_temp_month_predict <- ggplot(data=total_Min_temp_month_for_month_predict,
                                                aes(x=Month, y=avg_Min_temp_month_predict))+
  geom_line()+
  geom_point(shape=22, size=3, colour="darkred", fill="pink")+
  theme_minimal()+
  xlab("Month")+
  ylab("Min Temp")+
  ggtitle("Min Temp vs Month") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Min Temp vs Month Line Graph.pdf")
print(plot_avg_total_Min_temp_month_predict) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Min Temp vs Month Line Graph.png", plot = plot_avg_total_Min_temp_month_predict +
         theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')


#Calculating the average for the whole predict period
avg_Min_temp_month_predict_compare <- mean(total_Min_temp_month_for_month_predict$avg_Min_temp_month_predict)

#Calculating the difference between PSN and predicted
total_Min_temp_month_for_month_predict$diff_btn_month_predicted_psn <- total_Min_temp_month_for_month_predict$avg_Min_temp_month_predict - avg_Min_temp_month_for_month_psn_compare

#Plotting the difference
plot_diff_month_predict_psn_Min_temp_col <- ggplot(data=total_Min_temp_month_for_month_predict)+
  geom_col(aes(x=Month, y=diff_btn_month_predicted_psn, fill=factor(Month)))+
  theme_bw(base_size = 10, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        plot.title = element_text(color = "black", face = "bold"),
        axis.text.x = element_text(colour="black",size=10,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold")
  )+
  xlab("Month")+
  ylab("Average Minimum Temperature (oC)")+
  ggtitle("Average Minimum Temperature Anomaly (Jan-Dec)")

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Average Minimum Temperature Anomaly (Jan-Dec).pdf")
print(plot_diff_month_predict_psn_Min_temp_col) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Final/Average Minimum Temperature Anomaly (Jan-Dec).png", plot = plot_diff_month_predict_psn_Min_temp_col +
         theme(), width = 10, height = 8, dpi = 350, units = "in", device='png')

