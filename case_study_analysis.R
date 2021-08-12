#
#Load and install packages for data cleaning, analysis, and visualization
#
library(tidyverse)
library(ggplot2)
library(lubridate)
#####
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
####
#### Find and set a directory to import data
getwd()
setwd("C:/Users/RandW/Desktop/case_study_data/quartly_data")
q2_2019<-read.csv("C:/Users/RandW/Desktop/case_study_data/quartly_data/Divvy_Trips_2019_Q2")
q3_2019<-read.csv("C:/Users/RandW/Desktop/case_study_data/quartly_data/Divvy_Trips_2019_Q3.csv")
q4_2019<-read.csv("C:/Users/RandW/Desktop/case_study_data/quartly_data/Divvy_Trips_2019_Q4.csv")
q1_2020<-read.csv("C:/Users/RandW/Desktop/case_study_data/quartly_data/Divvy_Trips_2020_Q1.csv")
#### 
#### Look at column names to find out what needs to be renamed in order to
#aggregate data on q1_2020 because it has most up to date naming conventions
####
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)
####
#### Creating uniform column names so data can cleanly be aggregated
(q3_2019 <-rename(q3_2019
                 ,ride_id = trip_id
                 ,rideable_type = bikeid
                 ,started_at = start_time
                 ,ended_at = end_time
                 ,start_station_name = from_station_name
                 ,start_station_id = from_station_id
                 ,end_station_name = to_station_name
                 ,end_station_id = to_station_id
                 ,member_casual = usertype))
#### 
(q4_2019 <-rename(q4_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,start_station_name = from_station_name
                  ,start_station_id = from_station_id
                  ,end_station_name = to_station_name
                  ,end_station_id = to_station_id
                  ,member_casual = usertype))


colnames(q2_2019)     #double checked column names because these were different
#column names from the above tables
####
(q2_2019 <-rename(q2_2019
                  ,ride_id = "X01...Rental.Details.Rental.ID"
                  ,rideable_type = "X01...Rental.Details.Bike.ID"
                  ,started_at = "X01...Rental.Details.Local.Start.Time"
                  ,ended_at = "X01...Rental.Details.Local.End.Time"
                  ,start_station_name = "X03...Rental.Start.Station.Name"
                  ,start_station_id = "X03...Rental.Start.Station.ID"
                  ,end_station_name = "X02...Rental.End.Station.Name"
                  ,end_station_id = "X02...Rental.End.Station.ID"
                  ,member_casual = "User.Type"))
####
#### Change rider_id into character data type so they can all be used as a
# common point to bind rows from q2,q3,q4 2019 to q1 2020
####
q4_2019<- mutate(q4_2019, ride_id = as.character(ride_id), 
                 rideable_type = as.character(rideable_type))
q3_2019<- mutate(q3_2019, ride_id = as.character(ride_id), 
                 rideable_type = as.character(rideable_type))
q2_2019<- mutate(q2_2019, ride_id = as.character(ride_id), 
                 rideable_type = as.character(rideable_type))
####
#### aggregate data with bind_rows, connects data by adding rows from other data
#set into another to bring all data together
####
all_trips <- bind_rows(q2_2019,q3_2019,q4_2019,q1_2020)
####
#### Drop the rows that don't have any relevance to the analysis
####
all_trips <- all_trips %>%
  select(-c(start_lat,start_lng,end_lat,end_lng,birthyear,gender,
            "X01...Rental.Details.Duration.In.Seconds.Uncapped",
            "X05...Member.Details.Member.Birthday.Year", "Member.Gender",
            tripduration))
####
#### Look at new table to see what columns have what data types 
####
str(all_trips)
####
#### Change old naming conventions to updated 2020 convention which are members
#and casual riders
####
table(all_trips$member_casual)
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,
                                "Customer" = "casual",
                                "Subscriber" = "member"))
table(all_trips$member_casual)
####
#### separate each part of the date, from started_at, into new columns
####
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
####
#### Find ride length for each trip by finding the difference between ended_at
#and started_at. Difftime finds that difference and createsa new column with 
#that difference and measurement will be in seconds.
###
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
####
####Change ride_length into a numeric value so it can be used in analysis
####
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
####
#### Drop the rows where time was negative or bikes were being taken out of
#circulation
####
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HR QR" | all_trips$ride_length < 0),] 
###########################################################
################ Analysis begins ##########################
###########################################################
####
#### Confirm profitability
####all_trips_v2 %>%
group_by(all_trips_v2$member_casual) %>%
  summarise(number_of_rides = n())
2973861 * (9*12)
# Members brought in about $321,176,988 within a year
casual_riders <- filter(all_trips_v2,
         member_casual == "casual") 
sum(casual_riders$ride_length)
 x<- 3205275120/60   # to make sum ride time in minutes
   (x)* .11          # $0.11 per minute is how much Cyclistic charges for a single
 #ride, which is 30 minutes  for $3.30.
# On the low end, casual riders brought in $5,876,338 within a year
# We can confidently say that members do bring in more revenue
####
#### view statistical summary of the data frame
####
summary(all_trips_v2$ride_length)

####
####mode of the top 3 days with most rides
####
names(sort(-table(all_trips_v2$day_of_week)))[1]
names(sort(-table(all_trips_v2$day_of_week)))[2]
names(sort(-table(all_trips_v2$day_of_week)))[3]
####
####check that the mode is correct
####
all_trips_v2 %>%
  group_by(all_trips_v2$day_of_week) %>%
  summarise(num_weekday = n()) %>%
  arrange(-num_weekday)
####
#### summarize ride_length by member type (member and casual)
tapply(all_trips_v2$ride_length, all_trips_v2$member_casual, summary, na.rm=TRUE)
####
#### Find mean of average ride_length for member type by day of the week and by 
#month 
####
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$month, FUN = mean)
####
#### Find number of rides in past year by member type 
####
all_trips_v2 %>%
  group_by(all_trips_v2$member_casual) %>%
  summarise(number_of_rides = n())
####
#### Number of rides per day by member type
####
all_trips_v2 %>%
  group_by(all_trips_v2$member_casual, all_trips_v2$day_of_week) %>%
  summarise(number_of_rides = n()) 
####
#### Average ride length per day by member type in seconds
####
all_trips_v2 %>%
  group_by(all_trips_v2$member_casual, all_trips_v2$day_of_week) %>%
  summarise(number_of_rides = n(),
            avg_ride_length_per_day = mean(ride_length))
####
#### Visualization for Number For Rides by Member Type per Weekday
#### 
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							
           ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual))+
  geom_col(position='dodge') +
  scale_fill_manual("legend", values = c("member" = "seagreen", "casual" = "gold1")) + 
  ggtitle("Number of Rides For Memeber Type by Weekday")
####
#### Average Ride Length by Member Status and Weekday
####
all_trips_v2 %>% 
  mutate(weekday = wday(all_trips_v2$started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							 
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday,y=average_duration,fill=member_casual))+
  geom_col(position = "dodge") +
  scale_fill_manual("legend", values = c("member" = "seagreen", "casual" = "gold1")) +
  ggtitle("Average Ride Length For Member Status by Weekday")

####
#### Visualization for average ride length by month by member type
####
all_trips_v2%>%
  group_by(member_casual,month) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x=month,y=average_duration,fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual("legend", values = c("member" = "seagreen", "casual" = "gold1")) +
  ggtitle("Average Ride Length For Member Status by Month")


