#adding Monsoon 
data$Monsoon <- ifelse(data$Month >= 3 & data$Month <= 5, "Pre-Monsoon Hot Summer",
                              ifelse(data$Month >= 6 & data$Month <= 9, "Rainy",
                                     ifelse(data$Month >= 10 & data$Month <= 11, "Post-Monsoon Autumn",
                                            ifelse(data$Month >=12 | data$Month <= 2, "Winter", "NA")
                                            
                                     )
                              )
)


#Adding Season in terms of Bangladesh
data$Season <- ifelse(data$Month == 1, "Winter",
                             ifelse(data$Month == 2, "Winter",
                                    ifelse(data$Month == 3, "Spring",
                                           ifelse(data$Month == 4, "Spring",
                                                  ifelse(data$Month == 5, "Summer",
                                                         ifelse(data$Month == 6, "Summer",
                                                                ifelse(data$Month == 7, "Rainy",
                                                                       ifelse(data$Month == 8, "Rainy",
                                                                              ifelse(data$Month == 9, "Autumn",
                                                                                     ifelse(data$Month == 10, "Autumn",
                                                                                            ifelse(data$Month == 11, "Late Autumn",
                                                                                                   ifelse(data$Month == 12, "Late Autumn", "NA")
                                                                                            )
                                                                                     )
                                                                              )
                                                                       )
                                                                )
                                                         )
                                                  )
                                           )
                                    )
                             )
)


write.csv(data,"~/Dropbox/MSc/Course_Work/Big_Data/Project/Project_2_Final/Data/65 Years of Weather Data Bangladesh.csv", row.names = TRUE)

#adding Monsoon using dplyer
library(tidyverse)
data %>% 
  select(X:Region) %>%
  mutate(
    test = case_when(
      Month >= 3 & Month <= 5 ~ "Pre-Monsoon Hot Summer",
      Month >= 6 & Month <= 9 ~ "Rainy",
      Month >= 10 & Month <= 11 ~ "Post-Monsoon Autumn",
      Month >=12 | Month <= 2 ~ "Winter",
      TRUE                      ~ "other"
    )
  )
