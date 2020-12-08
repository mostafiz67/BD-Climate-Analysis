#Creating average MAX and MIN temperature based on the year 

library(dplyr)
library(tidyr)  
library(ggplot2)

avg_temp_data_by_year_and_region <- data %>% 
  group_by(YEAR, Region) %>% 
  summarise(
    year_wise_region_max_temp = mean(Max.Temp),
    year_wise_region_min_temp = mean(Min.Temp)
  )

plot_max_temp_vs_year_region_bar <- ggplot(data=avg_temp_data_by_year_and_region[avg_temp_data_by_year_and_region$YEAR %in% c(1948, 1960, 1972, 1984, 1996, 2008, 2013),], 
                                    aes(x=factor(Region), y=year_wise_region_max_temp, fill=factor(YEAR)))+
  geom_bar(stat="identity", width = 0.5, position = position_dodge(width = 0.9))+
  geom_text(aes(label=sprintf("%0.2f", round(year_wise_region_max_temp, digits = 2))), position=position_dodge(width=0.9), vjust=-0.70)+
  theme_minimal()+
  xlab("Region")+
  ylab("Temperature")+
  ggtitle("Avg. Max Temp with 12 years moving ") 
#Dinamics
ggplotly(plot_max_temp_vs_year_region_bar)

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Measurement of Max Temperature 12 years moving Region Bar Graph.pdf")
print(plot_max_temp_vs_year_region_bar) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Measurement of Max Temperature 12 years moving Region Bar Graph.png", plot = plot_max_temp_vs_year_region_bar,
       width = 12, height = 8, dpi = 300, units = "in", device='png')


plot_max_temp_vs_year_region_line <- ggplot(data=avg_temp_data_by_year_and_region[avg_temp_data_by_year_and_region$YEAR %in% c(1948, 1960, 1972, 1984, 1996, 2008, 2013),], 
                                         aes(x=YEAR, y=year_wise_region_max_temp, colour=Region))+
  
  geom_line()+
  geom_point(shape=22, size=3, colour="darkred", fill="pink")+
  geom_text(aes(label=sprintf("%0.2f", round(year_wise_region_max_temp, digits = 2))), position=position_dodge(width=0.9), vjust=-0.50)+
  theme_minimal()+
  xlab("Year")+
  ylab("Temperature")+
  ggtitle("Avg Max Temp with 12 years moving ") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Measurement of Avg Max Temperature 12 years moving Region Line Graph.pdf")
print(plot_max_temp_vs_year_region_line) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Measurement of Avg Max Temperature 12 years moving Region Line Graph.png", plot = plot_max_temp_vs_year_region_line, 
       width = 12, height = 8, dpi = 300, units = "in", device='png')




