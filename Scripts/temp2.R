#Creating average Min and MIN temperature based on the year 

library(dplyr)
library(tidyr)  
library(ggplot2)

whole_avg_min_temp <- mean(data$Min.Temp)

diff_min_temp <- data %>% 
  group_by(YEAR, Region, Monsoon) %>% 
  summarise(
    temp_avg_min = mean(Min.Temp)
  )

diff_min_temp$dif_avg_temp_min = diff_min_temp$temp_avg_min - whole_avg_min_temp

plot_diff_min_temp_vs_year_region_col_new <- ggplot(data=diff_min_temp[diff_min_temp$YEAR %in% c(1948, 1960, 1972, 1984, 1996, 2008, 2013),])+
  geom_col(aes(x=Region, y=dif_avg_temp_min, fill=factor(YEAR)))+
  theme_minimal()+
  xlab("Region")+
  ylab("Temperature")+
  ggtitle("Diff of Min Temp with Whole Avg. Min Temp (12 years moving)") 
#Dinamics
ggplotly(plot_diff_min_temp_vs_year_region_col_new)