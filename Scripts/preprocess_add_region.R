#Finding NUmber of Unique Stations Name
length(unique(data$Station.Names))

#Finding name of the using stations
unique(data$Station.Names)

#adding Regions 
data$Region <- ifelse(data$Station.Names == 'Bogra' | data$Station.Names == 'Dinajpur' | data$Station.Names == 'Rangpur' | data$Station.Names == 'Syedpur', "Northern Region",
                       ifelse(data$Station.Names == 'Chuadanga' | data$Station.Names == 'Faridpur' | data$Station.Names == 'Ishurdi' | data$Station.Names == 'Rajshahi', "Northwestern Region",
                              ifelse(data$Station.Names == 'Jessore' | data$Station.Names == 'Khulna' | data$Station.Names == 'Mongla' | data$Station.Names == 'Patuakhali' | data$Station.Names == 'Satkhira' | data$Station.Names == 'Khepupara' , "Northsouthern Region",
                                     ifelse(data$Station.Names == 'Dhaka' | data$Station.Names == 'Tangail' | data$Station.Names == 'Mymensingh', "Central Region",
                                            ifelse(data$Station.Names == 'Barisal' | data$Station.Names == 'Bhola' | data$Station.Names == 'Chandpur' | data$Station.Names == 'Comilla' | data$Station.Names == 'Feni' | data$Station.Names == 'Maijdee Court' | data$Station.Names == 'Madaripur' | data$Station.Names == 'Hatiya', "Southern Region",
                                                   ifelse(data$Station.Names == 'Chittagong (City-Ambagan)' | data$Station.Names == 'Chittagong (IAP-Patenga)' | data$Station.Names == "Cox's Bazar" | data$Station.Names == 'Kutubdia' | data$Station.Names == 'Rangamati' | data$Station.Names == 'Sandwip' | data$Station.Names == 'Sitakunda' | data$Station.Names == 'Teknaf', "Southeastern Region",
                                                          ifelse(data$Station.Names == 'Sylhet' | data$Station.Names == 'Srimangal', "Eastern Region", "NAA")
                                                   )
                                            )
                                     )
                              )
                       )
)



#Checking Errors
unique(data$Region)
