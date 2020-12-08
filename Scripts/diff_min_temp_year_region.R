#Creating average Min and MIN temperature based on the year 

library(dplyr)
library(tidyr)  
library(ggplot2)

whole_avg_max_temp <- mean(data$Max.Temp)
whole_avg_min_temp <- mean(data$Min.Temp)

diff_max_temp <- data %>% 
  group_by(YEAR, Region) %>% 
  summarise(
    temp_avg_max = mean(Min.Temp),
    temp_avg_min = mean(Min.Temp)
  )

diff_max_temp$temp_dif_avg_max = temp$temp_avg_max - whole_avg_max_temp
diff_max_temp$whole_avg_min_temp = temp$temp_avg_min - whole_avg_min_temp

plot_diff_min_temp_vs_year_region_col <- ggplot(data=diff_max_temp[diff_max_temp$YEAR %in% c(1948, 1960, 1972, 1984, 1996, 2008, 2013),])+
  geom_col(aes(x=Region, y=whole_avg_min_temp, fill=factor(YEAR)))+
  theme_minimal()+
  xlab("Region")+
  ylab("Temperature")+
  ggtitle("Diff of Min Temp with Whole Avg. Min Temp (12 years moving)") 
#Dinamics
ggplotly(plot_diff_min_temp_vs_year_region_col)

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Diff of Min Temp with Whole Avg. Min Temp (12 years moving) Region Col Graph.pdf")
print(plot_diff_min_temp_vs_year_region_col) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Diff of Min Temp with Whole Avg. Min Temp (12 years moving) Region Col Graph.png", plot = plot_diff_min_temp_vs_year_region_col, 
       width = 12, height = 8, dpi = 300, units = "in", device='png')




#line graph

plot_diff_min_temp_vs_year_region_line <- ggplot(data=diff_max_temp[diff_max_temp$YEAR %in% c(1948, 1960, 1972, 1984, 1996, 2008, 2013),], 
                                                 aes(x=YEAR, y=whole_avg_min_temp, colour=Region))+
  geom_line()+
  geom_point(shape=22, size=3, colour="darkred", fill="pink")+
  geom_text(aes(label=sprintf("%0.2f", round(inwhole_avg_min_temp, digits = 2))), position=position_dodge(width=0.9), vjust=-0.70)+
  theme_minimal()+
  xlab("Region")+
  ylab("Temperature")+
  ggtitle("Diff of Min Temp with Whole Avg. Min Temp (12 years moving)") 
#Dinamics
ggplotly(plot_diff_min_temp_vs_year_region_line)

pdf("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Documents/Diff of Min Temp with Whole Avg. Min Temp (12 years moving) Region Line Graph.pdf")
print(plot_diff_min_temp_vs_year_region_line) 
dev.off()

ggsave("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Figures/Diff of Min Temp with Whole Avg. Min Temp (12 years moving) Region Line Graph.png", plot = plot_diff_min_temp_vs_year_region_line, 
       width = 12, height = 8, dpi = 300, units = "in", device='png')

