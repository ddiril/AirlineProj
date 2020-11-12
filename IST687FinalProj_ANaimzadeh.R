################################################
# IST687 Final Project
#
# Student name: Abdullah Naimzadeh
# Date due: October 4, 2020
#
# Attribution statement: (choose only one)

# 2. I did this homework with help from the book and the professor and these Internet sources:
file.choose()


# Run these three functions to get a clean test of homework code
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear user objects from the environment

file.choose()
setwd("/Users/princeabu/Desktop/Grad School/Intro to Data Science/") # directory found from file.choose, this is where it is stored in my machine

# Packages to install and run: 
install.packages(("RCurl"))
library(RCurl)
library(jsonlite)
library(ggplot2)
library(tidyverse)
library(ggplot2)
install.packages("rjson")

# Follow Data Science process: Clean, Prep, Analyze, Explore

# -------------------------------------------------------------------------------------------------- #
# Part 1: 
# ----------------------------Obtain Clean and Prep AirSurvey Data---------------------------------- #

# From jsonfile, read into R as a dataframe and call it 'airData'
# airsurveyLink = 'https://s3.amazonaws.com/blackboard.learn.xythos.prod/5956621d575cd/11264935?response-cache-control=private%2C%20max-age%3D21600&response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27airsurvey.json&response-content-type=application%2Fjson&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20201029T120000Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAYDKQORRYTKBSBE4S%2F20201029%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=682bf130d2e0d58d8453116e5e8d58712b1be64e406cb791d492ed635806a94e'
# textOutput <- getURL(airsurveyLink) # using the getURL function to request the data from the web and source it to R as text
# airSurveylite <- jsonlite::fromJSON(textOutput) # using the from JSON function to parse through the human readable text and make a data frame
# airSurvey <- data.frame(airSurveylite)

# Load airsurvey Data into R from local directory and rename to airData, convert to data frame
df <-'airsurvey.json'
airData	<- jsonlite::fromJSON(df)
airData <- data.frame(airData)
str(airData)
# Identify missing values
# Data has many columns, We can use the colSums function to count how many missing values there are per column
colSums(is.na(airData))

# The following columns contain null values: 
#                      | Departure.Delay.in.Minutes | Arrival.Delay.in.Minutes  |  Flight.time.in.minutes  |
# # of missing Values:           90                           96                             96            

# If we choose to omit these values, we would be getting rid of about 2% of data (90/5000)
# I think we should replace NA values with the median, since things like average and stdev are not sensitive to median

median(airData$Departure.Delay.in.Minutes,na.rm = TRUE) # median is 0 anyway so itd be the same if we omitted
airData$Departure.Delay.in.Minutes <-  replace_na(airData$Departure.Delay.in.Minutes, median(airData$Departure.Delay.in.Minutes,na.rm = TRUE))

# Repeat for the other two attriutes
median(airData$Arrival.Delay.in.Minutes,na.rm = TRUE) # median is 0 anyway so itd be the same if we omitted
airData$Arrival.Delay.in.Minutes <-replace_na(airData$Arrival.Delay.in.Minutes, median(airData$Arrival.Delay.in.Minutes,na.rm = TRUE)) 

median(airData$Flight.time.in.minutes,na.rm = TRUE) # median is 91
airData$Flight.time.in.minutes <- replace_na(airData$Flight.time.in.minutes, median(airData$Flight.time.in.minutes,na.rm = TRUE))

length(which(airData$Flight.time.in.minutes + airData$Arrival.Delay.in.Minutes > 360))

# --------------------------------------Additioanl attribute to add------------------------------------- #

# Could potentially be use for later models

# total time in flight is the arrival delay plus flight time ?
airData$total.flight.time <- airData$Arrival.Delay.in.Minutes + airData$Flight.time.in.minutes
# Labeling flights short, medium or long in terms of time; Less than 3 hours, 3-6 hours, greater than 6 hours
airData <- airData %>% mutate(Duration_Flight_Type = cut(total.flight.time, breaks = c(0,60,360, 450), labels = c("Short", "Medium", "Long")))

# creating new vector that identifies better those that are promotoer, detractors or passive in recommendations
airData <- airData %>% mutate(recommend_name = cut(Likelihood.to.recommend, breaks = c(0,6,8, 11), labels = c("detractor", "passive", "promoter")))

# interesting things to note
summary(airData$Shopping.Amount.at.Airport)

# identify number of flights leaving a particular state
sort(table(airData$Origin.State),decreasing = TRUE)

#identify number of flights arriving at a particular state
sort(table(airData$Destination.State),decreasing = TRUE)

# Most active to least active airline partner
sort(table((airData$Partner.Name)),decreasing = TRUE)

# create NPS calc function
# "NPS determine by -> %promotors - %detractors
# take specific attribute (age,gender, etc) 
# retrun NPS score
nps_score <- function(subgroup,df_subset){
    
    sub_data <- length(which(subgroup == df_subset))
    p <- length(which(airData$recommend_name[which(subgroup == df_subset)] == 'promoter'))
    # create detractors
    d <- length(which(airData$recommend_name[which(subgroup == df_subset)] == 'detractor'))
    #determine overall generic score
    score <-  (p -d)/sub_data
    return(score)
}
# Test function
nps_score(airData$Gender, 'Male')


unique(airData$Price.Sensitivity)
str(airData)
airData <- data.frame(lapply(airData, as.factor))
airData$Airline.Status <- as.factor(airData$Airline.Status)
airData$Gender <- as.factor(airData$Gender)
airData$Type.of.Travel <- as.factor(airData$Type.of.Travel)
airData$Class <- as.factor(airData$Class)



#-------------------------------------------Graphs---------------------------------------#
ggplot(airData) + geom_bar(aes(x = Age, fill = Likelihood.to.recommend)) + theme_classic()
ggplot(airData) + geom_bar(aes( x = Airline.Status, fill = Likelihood.to.recommend), position = "fill")

# Merge Attribute data and Likelihood Data

# see missing values : sum(is.na(x))
# replace missing mean 
# mydata$mean<- ifelse(is.na(mydata$mean), mydata$median, mydata$mean

# Need 

us <- map_data("state")
us_map <- ggplot(airData)
us_map <- us_map + geom_map(data = us, map = us, aes(map_id = region),fill = 'grey', colour = 'black') 
us_map <- us_map + geom_map(map = us, aes(map_id = Origin.State, fill = Likelihood.to.recommend),colour = 'black')
us_map <- us_map + expand_limits(x=us$long, y= us$lat)
us_map
str(airData)

# ------------------------------------- Linear Modeling ---------------------------------#
library(arules)
library(arulesViz)
rules1 <- apriori(airData,
                  parameter=list(supp=0.0055, conf=0.75), # confidence of at least 75% that both lhs and rhs combinations occur when lhs is present
                  # and at least 0.55% of the time certain lhs combinations occur compared to the whole data set
                  control=list(verbose=F), # Report progress set to false
                  appearance=list(default="lhs",rhs=("recommend_name=promoter")))

