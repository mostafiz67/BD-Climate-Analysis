#Creating average MIN temperature based on the year 

library(dplyr)
library(tidyr)  
library(plotly)

avg_temp_data_by_year_and_monsoon <- data %>% 
  group_by(YEAR, Monsoon) %>% 
  summarise(
    year_wise_max_temp = mean(Max.Temp),
    year_wise_min_temp = mean(Min.Temp)
  )

plot_min_temp_vs_year_bar <- ggplot(data=avg_temp_data_by_year_and_monsoon[avg_temp_data_by_year_and_monsoon$YEAR %in% c(1948, 1960, 1972, 1984, 1996, 2008, 2013),], 
                                    aes(x=factor(YEAR), y=year_wise_min_temp, fill=factor(Monsoon)))+
  geom_bar(stat="identity", width = 0.5, position = position_dodge(width = 0.8))+
  geom_text(aes(label=sprintf("%0.2f", round(year_wise_min_temp, digits = 2))), position=position_dodge(width=0.9), vjust=-0.50)+
  theme_minimal()+
  xlab("Year")+
  ylab("Temperature")+
  ggtitle("Avg Min Temp with 12 years moving ") 
#Dinamics
ggplotly(plot_min_temp_vs_year_bar)

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Measurement of Avg Min Temperature  12 years moving Bar Graph.pdf")
print(plot_min_temp_vs_year_bar) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Measurement of Avg Min Temperature 12 years moving Bar Graph.png", plot = plot_min_temp_vs_year_bar,
       width = 12, height = 8, dpi = 300, units = "in", device='png')



plot_min_temp_vs_moonsoon_bar <- ggplot(data=avg_temp_data_by_year_and_monsoon[avg_temp_data_by_year_and_monsoon$YEAR %in% c(1948, 1960, 1972, 1984, 1996, 2008, 2013),], 
                                        aes(x=factor(Monsoon), y=year_wise_min_temp, fill=factor(YEAR)))+
  geom_bar(stat="identity", width = 0.5, position = position_dodge(width = 0.8))+
  geom_text(aes(label=sprintf("%0.2f", round(year_wise_min_temp, digits = 2))), position=position_dodge(width=0.9), vjust=-0.50)+
  theme_minimal()+
  xlab("Year")+
  ylab("Temperature")+
  ggtitle("Avg Min Temp with 12 years moving ") 
#Dinamics
ggplotly(plot_min_temp_vs_moonsoon_bar)

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Measurement of Avg min Temperature 12 years moving Monsoon Bar Graph.pdf")
print(plot_min_temp_vs_moonsoon_bar) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Measurement of Avg min Temperature 12 years moving Monsoon Bar Graph.png", plot = plot_min_temp_vs_moonsoon_bar,
       width = 12, height = 8, dpi = 300, units = "in", device='png')

plot_min_temp_vs_moonsoon_line <- ggplot(data=avg_temp_data_by_year_and_monsoon[avg_temp_data_by_year_and_monsoon$YEAR %in% c(1948, 1960, 1972, 1984, 1996, 2008, 2013),], 
                                         aes(x=YEAR, y=year_wise_min_temp, colour=Monsoon))+
  
  geom_line()+
  geom_point(shape=22, size=3, colour="darkred", fill="pink")+
  geom_text(aes(label=sprintf("%0.2f", round(year_wise_min_temp, digits = 2))), position=position_dodge(width=0.9), vjust=-0.50)+
  theme_minimal()+
  xlab("Year")+
  ylab("Temperature")+
  ggtitle("MIn Temp with 12 years moving ") 

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Measurement of Avg Min Temperature 12 years moving Line Graph.pdf")
print(plot_min_temp_vs_moonsoon_line) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Measurement of Avg Min Temperature 12 years moving Line Graph.png", plot = plot_min_temp_vs_moonsoon_line, 
       width = 12, height = 8, dpi = 300, units = "in", device='png')


