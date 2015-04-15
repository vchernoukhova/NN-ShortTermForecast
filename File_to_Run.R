#install.packages('RODBC')
#install.packages('lubridate')
#install.packages('brnn')
rm(list = ls())

#CREATE FOLDER U:\_Load Forecasting\Victoria\Short Term NN model\Output results\Models\ YOUR BATCH NUMBER

require (RODBC)
require (lubridate)
source('U:/_Load Forecasting/Victoria/Short Term NN model/Codes/brnn_function_code.R')

MarketIdentifier <- 'NY_PWR'
ForecastMonthVector <- c('06/01/2014','07/01/2014','08/01/2014','09/01/2014')
#ForecastMonthVector <- c('09/01/2014')
#ForecastMonth    <- '06/01/2014'
NumberOfValidationMonths <- 4
NumberOfGapMonths <- 4
NumberOfRuns      <- 3
BatchNo           <- 1
InputType         <- 'Second'  #'Default'
Comment           <- '4 months|exl Sep28-Oct19,2013|+season'


StartExcluding <- '09/28/2013' #NULL
EndExcluding   <- '10/19/2013' #NULL

if (is.null(Comment)){
  Comment <- InputType
}

##### BatchNo ########
# Victoria 1
# Grace    2
# Alex     3
# Daniel   4
# Extra    5
#############


channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")
if (!exists("data_all")) { 
    data_all <- sqlQuery(channel,"  SELECT 
                                          [BookOID]
                                         ,[DeliveryPointIdentifier]
                                         ,[LoadProfileGroupIdentifier]
                                         ,[DayTypeIdentifier]
                                         ,[StartDate]
                                         ,[DayTypeCode]
                                         ,[DayOfWeek]
                                         ,[Month]
                                         ,[ForecastHour]
                                         ,[Volume]
                                         ,[CloudCover]
                                         ,[WindSpeed]
                                         ,[DewPointTemperature]
                                         ,[WetBulbTemperature]
                                         ,[WeightedTemperatureHumidityIndex]
                                         ,[LagVolume]
                                         ,[RCECount]
                                         ,[CustCount]
                                         ,[ActualLoad]
                                         ,[LagSystemLoad]
                                         ,[Season]
                                    FROM
                                          [LoadForecastingAnalytics].[dbo].[vw_NeuralNetworkInputNYISO]
                                    WHERE YEAR(StartDate) = 2013 OR YEAR(StartDate) = 2014 
                                    ORDER BY
                                          [BookOID]
                                         ,[DeliveryPointIdentifier]
                                         ,[LoadProfileGroupIdentifier]
                                         ,[StartDate]
                                         ,[ForecastHour]
                                        ")

##data formatting 
data_all$BookOID                    <- as.character(data_all$BookOID)
data_all$LoadProfileGroupIdentifier <- as.character(data_all$LoadProfileGroupIdentifier)
data_all$DeliveryPointIdentifier    <- as.character(data_all$DeliveryPointIdentifier)
data_all$DeliveryPointIdentifier[data_all$DeliveryPointIdentifier=='FALSE'] <- 'F' 

data_all$StartDate <- as.Date(data_all$StartDate, format = '%m/%d/%Y')
##

}
channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")
combinations <- sqlQuery(channel,"  SELECT
                                        DISTINCT 
                                    		BookOID
                                    ,		LoadProfileGroupIdentifier
                                    ,		DeliveryPointIdentifier
                                    FROM
                                    		LoadForecastingAnalytics.dbo.StageNYISOTrainingData
                                    ORDER BY
                                    		BookOID
                                    ,		LoadProfileGroupIdentifier
                                    ,		DeliveryPointIdentifier
                                   
                                    ")


input <- sqlQuery(channel, paste("SELECT
                                          *
                                  FROM
                                        [LoadForecastingAnalytics].[dbo].[R_NeuralNetwork_inputs]
                                  WHERE
                                        InputType = '", InputType,"'", sep= '' ))

  #Run everything
index <- c(1:nrow(combinations))

#Run Hudson only
index <- c(1:2) 
#Run JE only
#index <- c(21:39)
#Exclude observation
#index <-  index[-which(index==9)]

#Run particular combination
#i=3
#Or type manually
#Current_BookOID                    <- 'Hudson'
#Current_LoadProfileGroupIdentifier <- 'OR'
#Current_DeliveryPointIdentifier    <- 'G'


for (i in index){
    Current_BookOID                    <- combinations$BookOID[i]
    Current_LoadProfileGroupIdentifier <- combinations$LoadProfileGroupIdentifier[i]
    Current_DeliveryPointIdentifier    <- combinations$DeliveryPointIdentifier[i]
    
    for (ForecastMonth in ForecastMonthVector){
      source('Z:/ShortTermForecast/Main_code.R')
    }
}

