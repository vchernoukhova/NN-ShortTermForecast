##April 20

#install.packages('RODBC')
#install.packages('lubridate')
#install.packages('brnn')
rm(list = ls())

source('U:/_Load Forecasting/Victoria/Short Term NN model/Codes/DataUpload.R')

MarketIdentifier         <- 'NY_PWR'
NumberOfValidationMonths <- 4
NumberOfGapMonths        <- 4
NumberOfRuns             <- 3
BatchNo                  <- 5
Show_Iterations          <- FALSE

noise                    <- FALSE
noise_factor             <- 5 # vector*factor/50

##### BatchNo ########
# Victoria 1
# Grace    2
# Alex     3
# Daniel   4
# Extra    5
#############


ForecastMonthVector <- c('06/01/2014','07/01/2014','08/01/2014','09/01/2014')
InputType           <- 'Current Best'
Comment             <- 'OR Testing|6 delays'



channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")
combinations <- sqlQuery(channel, paste("SELECT
                                                 DISTINCT
                                                 BookOID
                                         ,       LoadProfileGroupIdentifier
                                         ,       DeliveryPointIdentifier
                                         FROM
                                                [LoadForecastingAnalytics].[dbo].[R_NeuralNetwork_inputs]
                                         WHERE
                                                InputType = '", InputType,"'", sep= '' ))

combinations$DeliveryPointIdentifier[combinations$DeliveryPointIdentifier=='FALSE'] <- 'F' 

index_zone  <- c(1:nrow(combinations))

for (i_zone in index_zone){
    Current_BookOID                    <- combinations$BookOID[i_zone]
    Current_LoadProfileGroupIdentifier <- combinations$LoadProfileGroupIdentifier[i_zone]
    Current_DeliveryPointIdentifier    <- combinations$DeliveryPointIdentifier[i_zone]
    

    channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")
    input_all <- sqlQuery(channel, paste ("SELECT
                                                  *
                                           FROM
                                                 [LoadForecastingAnalytics].[dbo].[R_NeuralNetwork_inputs]
                                           WHERE
                                                 InputType = '", InputType,
                                                 "' AND BookOID = '", Current_BookOID,
                                                 "' AND LoadProfileGroupIdentifier = '", Current_LoadProfileGroupIdentifier,
                                                 "' AND DeliveryPointIdentifier = '", Current_DeliveryPointIdentifier,"'",
                                                 sep= '' ))
    
    index_input <- c(1:nrow(input_all))
    
    for (i_input in index_input){
      input <- input_all[i_input,]
      input$DeliveryPointIdentifier[input$DeliveryPointIdentifier=='FALSE'] <- 'F' 
      for (ForecastMonth in ForecastMonthVector){
        source('U:/_Load Forecasting/Victoria/Short Term NN model/Codes/Main_code_last.R')
      }
    }
  
}
