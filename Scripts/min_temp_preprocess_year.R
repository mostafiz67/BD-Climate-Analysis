library(dplyr)
library(tidyr)  
library(ggplot2)

#Making subset for Provisional Standard Normal Value
pro_sta_nor <- subset(data, YEAR >= 1948 & YEAR <=1967)

total_min_temp_month_PSN <- pro_sta_nor %>% 
  group_by(YEAR, Month) %>% 
  summarise(
    total_min_temp_month = sum(Min.Temp),
    avg_min_temp_month = mean(Min.Temp)
  )

total_min_temp_month_PSN <- total_min_temp_month_PSN %>% 
  group_by(Month) %>% 
  summarise(
    total_min_temp_month = sum(avg_min_temp_month),
    avg_min_temp_month = mean(avg_min_temp_month)
  )

avg_min_temp_month_psn_compare <- mean(total_min_temp_month_PSN$avg_min_temp_month)

#Making subset for Predicting Value
predict_min_temp <- subset(data, YEAR >= 1968 & YEAR <=2013)


total_min_temp_month_predict <- predict_min_temp %>% 
  group_by(YEAR, Month) %>% 
  summarise(
    total_min_temp_month_predict = sum(Min.Temp),
    avg_min_temp_month_predict = mean(Min.Temp)
  )

total_and_avg_min_temp_year_predict <- total_min_temp_month_predict %>% 
  group_by(YEAR) %>% 
  summarise(
    total_min_temp_year_predict = sum(avg_min_temp_month_predict),
    avg_min_temp_year_predict = mean(avg_min_temp_month_predict)
  )

#plotting graph for the predicting Rainfall
plot_avg_total_min_temp_month_predict <- ggplot(data=total_and_avg_min_temp_year_predict,
                                                aes(x=YEAR, y=avg_min_temp_year_predict))+
  geom_line()+
  geom_point(shape=22, size=3, colour="darkred", fill="pink")+
  theme_minimal()+
  xlab("Year")+
  ylab("min Temp")+
  ggtitle("min Temp vs Year") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/min Temp vs Year Line Graph.pdf")
print(plot_avg_total_min_temp_month_predict) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/min Temp vs Year Line Graph.png", plot = plot_avg_total_min_temp_month_predict +
         theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')


#Calculating the average for the whole predict period
avg_min_temp_year_predict_compare <- mean(total_and_avg_min_temp_year_predict$avg_min_temp_year_predict)


#Calculating the difference between PSN and predicted
total_and_avg_min_temp_year_predict$diff_btn_year_predicted_psn <- total_and_avg_min_temp_year_predict$avg_min_temp_year_predict - avg_min_temp_month_psn_compare

#Plotting the difference
plot_diff_year_predict_psn_min_temp_col <- ggplot(data=total_and_avg_min_temp_year_predict)+
  geom_col(aes(x=YEAR, y=diff_btn_year_predicted_psn, fill=factor(YEAR)))+
  theme_bw(base_size = 10, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        plot.title = element_text(color = "black", face = "bold"),
        axis.text.x = element_text(colour="black",size=10,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold")
  )+
  xlab("Year")+
  ylab("Average Minimum Temperature (oC)")+
  ggtitle("Average Minimum Temperature Anomaly (1968-2013)")

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Average Minimum Temperature Anomaly (1968-2013).pdf")
print(plot_diff_year_predict_psn_min_temp_col) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Final/Average Minimum Temperature Anomaly (1968-2013).png", plot = plot_diff_year_predict_psn_min_temp_col +
         theme(), width = 10, height = 8, dpi = 350, units = "in", device='png')


