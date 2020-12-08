library(dplyr)
library(tidyr)  
library(ggplot2)

#Making subset for Provisional Standard Normal Value
pro_sta_nor <- subset(data, YEAR >= 1949 & YEAR <=1959)

total_monsoon_rainfall <- pro_sta_nor %>% 
  group_by(Monsoon) %>% 
  summarise(
    total_monsoon_rainfall_year = sum(Rainfall),
    avg_monsoon_rainfall_year = mean(Rainfall)
  )

total_monsoon_rainfall_PSN <- total_monsoon_rainfall %>% 
  group_by(Monsoon) %>% 
  summarise(
    total_monsoon_rainfall_PSN = sum(total_monsoon_rainfall_year),
    avg_monsoon_rainfall_PSN = mean(avg_monsoon_rainfall_year)
  )

avg_monsoon_rainfall_psn_compare <- mean(total_monsoon_rainfall_PSN$total_monsoon_rainfall_PSN)

#Making subset for Predicting Value
predict_rainfall <- subset(data, YEAR >= 1960 & YEAR <=2013)

total_monsoon_rainfall_month_predict <- predict_rainfall %>% 
  group_by(YEAR, Monsoon) %>% 
  summarise(
    total_monsoon_rainfall_month_predict = sum(Rainfall),
    avg_monsoon_rainfall_month_predict = mean(Rainfall)
  )

total_and_avg_monsoon_rainfall_year_predict <- total_monsoon_rainfall_month_predict %>% 
  group_by(Monsoon) %>% 
  summarise(
    avg_monsoon_rainfall_year_predict = sum(avg_monsoon_rainfall_month_predict),
    avg_monsoon_rainfall_month_predict = mean(avg_monsoon_rainfall_month_predict)
  )

#plotting graph for the predicting Rainfall

plot_avg_total_rainfall_predict <- ggplot(data=total_and_avg_monsoon_rainfall_year_predict,
                                          aes(x=Monsoon, y=avg_monsoon_rainfall_year_predict))+
  geom_line()+
  geom_point(shape=22, size=3, colour="darkred", fill="pink")+
  theme_minimal()+
  xlab("Monsoon")+
  ylab("Rainfall")+
  ggtitle("Rainfall vs Monsoon") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Rainfall vs Year Line Graph.pdf")
print(plot_avg_total_rainfall_predict) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Rainfall vs Year Line Graph.png", plot = plot_avg_total_rainfall_predict +
         theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')


#Calculating the average for the whole predict period
avg_monsoon_rainfall_year_predict_compare <- mean(total_and_avg_monsoon_rainfall_year_predict$avg_monsoon_rainfall_year_predict)


#Calculating the difference between PSN and predicted
total_and_avg_monsoon_rainfall_year_predict$diff_btn_predicted_psn <- total_and_avg_monsoon_rainfall_year_predict$avg_monsoon_rainfall_year_predict - avg_monsoon_rainfall_psn_compare

#Plotting the difference
plot_diff_predict_psn_rainfall_col <- ggplot(data=total_and_avg_monsoon_rainfall_year_predict)+
  geom_col(aes(x=Monsoon, y=diff_btn_predicted_psn, fill=factor(Monsoon)))+
  theme_minimal()+
  xlab("Monsoon")+
  ylab("Rainfall")+
  ggtitle("Diff of PSN with Monsoon average Rainfall") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Diff of PSN with Year average Rainfall")
print(plot_diff_predict_psn_rainfall_col) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Final/Diff of PSN with Year average Rainfall", plot = plot_diff_predict_psn_rainfall_col +
         theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')

