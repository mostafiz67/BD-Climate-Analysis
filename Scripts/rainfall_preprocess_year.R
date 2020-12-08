library(dplyr)
library(tidyr)  
library(ggplot2)

#Making subset for Provisional Standard Normal Value
pro_sta_nor <- subset(data, YEAR >= 1949 & YEAR <=1959)

total_rainfall_month_PSN <- pro_sta_nor %>% 
  group_by(YEAR, Month) %>% 
  summarise(
    total_rainfall_month = sum(Rainfall),
    avg_rainfall_month = mean(Rainfall)
  )

total_rainfall_year_PSN <- total_rainfall_month_PSN %>% 
  group_by(YEAR) %>% 
  summarise(
    avg_rainfall_year = sum(avg_rainfall_month),
    avg_rainfall_month = mean(avg_rainfall_month)
  )

avg_rainfall_year_psn_compare <- mean(total_rainfall_year_PSN$avg_rainfall_year)
avg_rainfall_month_psn_compare <- mean(total_rainfall_year_PSN$avg_rainfall_month)

#Making subset for Predicting Value
predict_rainfall <- subset(data, YEAR >= 1960 & YEAR <=2013)

total_rainfall_month_predict <- predict_rainfall %>% 
  group_by(YEAR, Month) %>% 
  summarise(
    total_rainfall_month_predict = sum(Rainfall),
    avg_rainfall_month_predict = mean(Rainfall)
  )

total_and_avg_rainfall_year_predict <- total_rainfall_month_predict %>% 
  group_by(YEAR) %>% 
  summarise(
    avg_rainfall_year_predict = sum(avg_rainfall_month_predict),
    avg_rainfall_month_predict = mean(avg_rainfall_month_predict)
  )

#plotting graph for the predicting Rainfall

plot_avg_total_rainfall_predict <- ggplot(data=total_and_avg_rainfall_year_predict,
                                          aes(x=YEAR, y=avg_rainfall_year_predict))+
  geom_line()+
  geom_point(shape=22, size=3, colour="darkred", fill="pink")+
  theme_minimal()+
  xlab("Year")+
  ylab("Rainfall")+
  ggtitle("Rainfall vs Year") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Rainfall vs Year Line Graph.pdf")
print(plot_avg_total_rainfall_predict) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Rainfall vs Year Line Graph.png", plot = plot_avg_total_rainfall_predict +
         theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')


#Calculating the average for the whole predict period
avg_rainfall_year_predict_compare <- mean(total_and_avg_rainfall_year_predict$avg_rainfall_year_predict)
avg_rainfall_month_predict_compare <- mean(total_and_avg_rainfall_year_predict$avg_rainfall_month_predict)

#Calculating the difference between PSN and predicted
total_and_avg_rainfall_year_predict$diff_btn_predicted_psn <- total_and_avg_rainfall_year_predict$avg_rainfall_year_predict - avg_rainfall_year_psn_compare

#Plotting the difference
plot_diff_predict_psn_rainfall_col <- ggplot(data=total_and_avg_rainfall_year_predict)+
  geom_col(aes(x=YEAR, y=diff_btn_predicted_psn, fill=factor(YEAR)))+
  theme_bw(base_size = 10, base_family = "Times New Roman")+
  theme(panel.grid = element_blank(),
        plot.title = element_text(color = "black", face = "bold"),
        axis.text.x = element_text(colour="black",size=10,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold")
  )+
  xlab("Year")+
  ylab("Average Annual Rainfall (mm)")+
  ggtitle("Annual Average Rainfall Anomaly in mm (1960-2013)") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Annual Average Rainfall Anomaly in mm (1960-2013)")
print(plot_diff_predict_psn_rainfall_col) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Final/Annual Average Rainfall Anomaly in mm (1960-2013)", plot = plot_diff_predict_psn_rainfall_col +
         theme(), width = 10, height = 8, dpi = 350, units = "in", device='png')

