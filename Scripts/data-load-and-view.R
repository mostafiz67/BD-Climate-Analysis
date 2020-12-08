#load the data from the dataset
data <- read.csv("~/Dropbox/MSc/Course_Work/Big_Data/Project/bd-weather/Data/65 Years of Weather Data Bangladesh.csv")

#Checking the first 10 records of our data
head(data, n=10)

#Checking the last 10 records of our data
tail(data, n=10)

#Check the structure of our object
str(data)

#Check if object (our data) contains any NULL value
is.null(data)

#Our dataset contains 1380 row but print option can not print all of them by default
print(data())

#So we have to write maximize our print option
options(max.print=10000) 
print(data)

#get the overall information of our dataset
introduce(data)

#Calculating some basic statistics related information
summary(data)

#Calculating the stander deviation You can also find the stander deviation from the summary function 
stander_deviation <- colMeans(data, na.rm = TRUE)
print(stander_deviation)

#Create the overview documentation of your data
#if the package doesn't exist 
#install.packages("DataExplorer")
library(DataExplorer)
create_report(data,
              output_dir = "~/Dropbox/MSc/Course_Work/Big_Data/Project/Project_2_Final/Documents/")

#if toy want you can change every points of your report using this way
# create_report(
#   data,
#   output_format = html_document(toc = TRUE, toc_depth = 6, theme = "yeti"),
#   output_file = "report.html",
#   output_dir = getwd(),
#   y = NULL,
#   config = configure_report(),
#   report_title = "Data Profiling Report",
#   ...
# )

#theme args should be one of “default”, “cerulean”, “journal”, “flatly”, “darkly”, “readable”, “spacelab”, “united”, “cosmo”, “lumen”, “paper”, “sandstone”, “simplex”, “yeti”

