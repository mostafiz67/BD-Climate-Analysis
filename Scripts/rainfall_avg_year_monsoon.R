#Creating average rainfall based on the year 

library(dplyr)
library(tidyr)  

avg_data_by_year_and_monsoon <- data %>% 
  group_by(YEAR, Monsoon) %>% 
  summarise(
    year_wise_rain = mean(Rainfall)
  )

plot_avg_rain_vs_year_by_season_bar <- ggplot(data=avg_data_by_year_and_monsoon[avg_data_by_year_and_monsoon$YEAR %in% c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2013),], 
                                          aes(x=factor(YEAR), y=year_wise_rain, fill=factor(Monsoon)))+
  geom_bar(stat="identity", width = 0.5, position = position_dodge(width = 0.5))+
  geom_text(aes(label=sprintf("%0.2f", round(year_wise_rain, digits = 2))), position=position_dodge(width=0.9), vjust=-0.50)+
  theme_minimal()+
  xlab("Year")+
  ylab("Rain")+
  ggtitle("Rain according to Monsoon (10 Year's period) ") 
#Dinamics
ggplotly(plot_avg_rain_vs_year_by_season_bar)

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Measurement of Avg Rain with Monsoon Bar Graph.pdf")
print(plot_avg_rain_vs_year_by_season_bar) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Measurement of Avg Rain with Monsoon Bar Graph.png", plot = plot_avg_rain_vs_year_by_season_bar +
         geom_point(size=2, shape=23) + theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')



plot_rain_vs_season_by_year_bar <- ggplot(data=avg_data_by_year_and_monsoon[avg_data_by_year_and_monsoon$YEAR %in% c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2013),], 
                                          aes(x=factor(Monsoon), y=year_wise_rain, fill=factor(YEAR)))+
  geom_bar(stat="identity", width = 0.5, position = position_dodge(width = 0.8))+
  geom_text(aes(label=sprintf("%0.2f", round(year_wise_rain, digits = 2))), position=position_dodge(width=0.9), vjust=-0.50)+
  theme_minimal()+
  xlab("Year")+
  ylab("Rain")+
  ggtitle("Rain according to Monsoon by Year (10 Year's period) ") 
#Dinamics
ggplotly(plot_rain_vs_season_by_year_bar)

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Measurement of Avg Rain vs Monson by Year Bar Graph.pdf")
print(plot_rain_vs_season_by_year_bar) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Measurement of Avg Rain vs Monson by Year Bar Graph.png", plot = plot_rain_vs_season_by_year_bar +
         geom_point(size=2, shape=23) + theme_bw(base_size = 10), width = 10, height = 8, dpi = 150, units = "in", device='png')
