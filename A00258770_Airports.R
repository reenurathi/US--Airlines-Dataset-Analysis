#library we are using
install.packages("dplyr")
install.packages("plyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("usmap")
install.packages("sqldf", dependencies = T)
install.packages("Hmisc")
library(Hmisc)
library(sqldf)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(usmap)

#Loading csv file into variables
USAirlines <- read.csv("US_Airlines.csv")
USAirport <- read.csv("US_Airports.csv")
USAirRecords <- read.csv("US_AirRecords.csv")

#check the structure of data and identify if any manipulation is required
str(USAirlines)
str(USAirport)
str(USAirRecords)

#Identify missing values using summary and remove them
summary(USAirport)
#Found 3 NA values, removing them from the dataset
USAirport <- USAirport %>%
  filter(!is.na(LATITUDE))
#verify omitted Null values
summary(USAirport)

#We will be utilizing Arival Delays for most of the analyses performed
#Hence, we need to remove NA values from ARRIVAL_DELAY
#After having a quick look at the data we realized that all Diverted flights have NA values for Delay
#hence we are removing all diverted flights from dataset
Diverted <- USAirRecords %>% 
  filter(DIVERTED==1 & CANCELLED==0) %>% 
  tally()

percent <- (Diverted/520718)*100 #Diverted flights are only 0.29% hence removing rows
percent
USAirRecords <- USAirRecords %>% 
  filter(DIVERTED==0)

#after removing Diverted flight we realized that there are still NA values in ARRIVAL_DELAY
#which are only because of CANCELLED FLIGHT, below summary shows that there are no NA values for non-cancelled flights.
#note we are not removing rows for cancelled flights, we will just put a filter in analysis
summary(USAirRecords %>% 
          filter(CANCELLED==0))

#-------Plot Data and Find point of interest------#
#-------Flight Delays and Number of flights by weekday------------------#
days <- data.frame(Day=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

WeekDay_Delays <- USAirRecords %>% 
  group_by(DAY_OF_WEEK) %>% 
  dplyr::summarise(AirlineDelay = sum(ARRIVAL_DELAY,na.rm = T))

WeekDay_Count <- USAirRecords %>% 
  group_by(DAY_OF_WEEK) %>% 
  tally()

WeekDay_Delays <- cbind(WeekDay_Delays, days)
WeekDay_Count <- cbind(WeekDay_Count, days)

barplot(WeekDay_Delays$AirlineDelay,
        names.arg=WeekDay_Delays$Day,
        main="Delays by WeekDay", 
        xlab="Weekday",
        ylab="Arrival Delay") 

barplot(WeekDay_Count$n,
        names.arg=WeekDay_Count$Day,
        main="Flight Count by WeekDay", 
        xlab="Weekday",
        ylab="Arrival Delay") 

#------Most frequent cancelaation reason--------#
Reason <- data.frame(Reason=c("Airline/Carrier", "Weather", "National Air System", "Security"))

CancellationReason <- USAirRecords %>% 
  filter(CANCELLED==1) %>% 
  group_by(CANCELLATION_REASON) %>% 
  tally()
  

CancellationReason <- cbind(CancellationReason, Reason)
print(c(toString(CancellationReason[which.max(CancellationReason$n),3]),"is the reson for maximum cancellations in US"))

#------Average delay due to Weather in non-cancelled flights--------------#
AvgWeatherDelay <- USAirRecords %>% 
  select(FLIGHT_NUMBER, WEATHER_DELAY, CANCELLED) %>% 
  filter(CANCELLED==0 & WEATHER_DELAY != 0)
  
#removing outliers that is exception cases, where delay is too high
summary(AvgWeatherDelay)
AvgWeatherDelay <- AvgWeatherDelay %>% 
  filter(WEATHER_DELAY > 13 & WEATHER_DELAY < 50)

#plotting the box plot of data
boxplot(AvgWeatherDelay$WEATHER_DELAY)

#calculating average delay due to weather in general.
print(c("Average delay due to weather is",toString(mean(AvgWeatherDelay$WEATHER_DELAY))))

#-------Find out counties with maximum number of flights flying out--------#

plot_usmap(regions = "counties") + 
  labs(title = "US Counties", subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"))

FIPS_DF <- usmap::us_map()
FIPS <- sqldf("SELECT DISTINCT abbr, fips
              FROM FIPS_DF
              ")
USFlightCount <- sqldf("SELECT ORIGIN_Airport, Count(FLIGHT_NUMBER) CNT
                       FROM USAirRecords
                       Where CANCELLED = 0
                       GROUP BY ORIGIN_Airport"
)
USMapVal <- sqldf("SELECT DISTINCT UA.STATE, SUM(UFC.CNT) AS SUM
              FROM USFlightCount UFC
              Left JOin USAirport UA On UFC.ORIGIN_AIRPORT = UA.IATA_CODE
              GROUP BY UA.STATE")
USMapFips <- sqldf("SELECT U.*, F.fips
                    FROM USMapVal U
                    JOIN FIPS F On F.abbr = U.STATE
                   ")


plot_usmap(data=USMapFips, values = "SUM") +
  scale_fill_gradientn( colours = topo.colors(7),
                        name = "Flight Counts", label = scales::comma
  ) + theme(legend.position = "right")





##------Airlines with most Delays--------##
Airline_Most_Delays <- USAirRecords %>% 
  group_by(AIRLINE) %>% 
  dplyr::summarise(AirlineDelay = sum(ARRIVAL_DELAY,na.rm = T))

MaxDelay <- Airline_Most_Delays[which.max(Airline_Most_Delays$AirlineDelay),1]


barplot(Airline_Most_Delays$AirlineDelay,
        names.arg=Airline_Most_Delays$AIRLINE,
        main="Sum of Arrival Delay by Airline", 
        xlab="Airline Code",
        ylab="Sum of Arrival Delay") 

print(c("Airline with Maximum Delays is",toString(USAirlines[USAirlines$IATA_CODE== toString(MaxDelay$AIRLINE), "AIRLINE"])))



##------Identify Relation Between Distance and Flying Time--------##

#remove outliers from Air Time column
summary(USAirRecords$AIR_TIME)
USAirRecordsFilt <- USAirRecords %>% 
  filter(USAirRecords$AIR_TIME>61.0  )

#remove outliers from Distance column
summary(USAirRecordsFilt$DISTANCE)
USAirRecordsFilt <- USAirRecordsFilt %>% 
  filter(USAirRecordsFilt$DISTANCE>596 & USAirRecordsFilt$DISTANCE<3000)

#Plot a scatterplot to see the relation between Time And Distance
scatter.smooth(x = USAirRecordsFilt$DISTANCE,y = USAirRecordsFilt$AIR_TIME,
               xlab = "Distance",
               ylab = "Flying Time",
               main = "Distance vs Flying Time"
)

#Find the correalatio value
cor(USAirRecordsFilt$DISTANCE, USAirRecordsFilt$AIR_TIME) #0.984 correlation



##------Identify the Busiest Route--------##
Busiest_Route <- USAirRecords %>% 
  select(ORIGIN_AIRPORT,DESTINATION_AIRPORT) %>% 
  group_by(ORIGIN_AIRPORT,DESTINATION_AIRPORT) %>% 
  tally %>% 
  arrange(desc(n)) 
Busiest_Route <- head(Busiest_Route,1)
Busiest_Route
print(c("Busiest Route is",toString(USAirport[USAirport$IATA_CODE=="SFO","AIRPORT"]), "to", toString(USAirport[USAirport$IATA_CODE=="LAX","AIRPORT"])))





#-------Analysis of choice-------#
FLightsData <- sqldf("SELECT ORIGIN_Airport, DESTINATION_AIRPORT, Count(FLIGHT_NUMBER) CNT
                       FROM USAirRecords
                       Where CANCELLED = 0
                       GROUP BY ORIGIN_Airport,DESTINATION_AIRPORT
                       ORDER BY Count(FLIGHT_NUMBER) DESC")
                       
FLightsData <- head(FLightsData,1)

Data <- sqldf("SELECT *
                       FROM USAirRecords
                       Where ORIGIN_Airport IN (SELECT ORIGIN_Airport FROM FLightsData)
                       AND DESTINATION_AIRPORT IN (SELECT DESTINATION_AIRPORT FROM FLightsData)")


Data_Subset <- Data %>% 
  select(ARRIVAL_DELAY, AIR_TIME, WHEELS_OFF, TAXI_IN, SCHEDULED_DEPARTURE, SCHEDULED_ARRIVAL)



#creating the correation between the parameters in the dataset
pairs(~ .,data = Data_Subset)

#defining training and testing dataset


#Then we create the training set data 70% of data
Data_Train <- Data_Subset[0:955, ]
#Then we create the training set data 30% of data
Data_Test <- Data_Subset[0:408, ]


#Model Generation
LMmodel<- lm(ARRIVAL_DELAY ~., data = Data_Train)
summary(LMmodel)

#ARRIVAL_DELAY = -1.78e+01 + 4.02e-01(AIR_TIME) - 5.46e-02(DISTANCE) + 5.74e-03(WHEELS_OFF) + 7.57e-01(TAXI_IN) + 6.69e-03(SCHEDULED_DEPARTURE) + 3.53e-03(SCHEDULED_ARRIVAL)

#Predictions and Evaluations of the test data
confint(LMmodel)

