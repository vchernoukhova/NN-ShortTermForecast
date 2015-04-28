## April 20 where we can run loops of inputs, and All utilities/zones
## Fixed output data, it shows correctly all utilities instead of "all"
## Added Train_Weighted_Mape
## Best run is based on Gamma
## Alpha and Beta are now parameters
## Iteration history to file_to_run
## April 21
## Beta can be NULL now, aplha is always NULL
## "Alpha" means mu, mu_dec = 1, mu_inc = 1
##April 24 adding start as parameter

condition <- data_all$BookOID == Current_BookOID

if (Current_LoadProfileGroupIdentifier!= 'ALL'){
  condition <- condition &  (data_all$LoadProfileGroupIdentifier == Current_LoadProfileGroupIdentifier)
}

if (Current_DeliveryPointIdentifier!= 'ALL'){
  condition <- condition &  (data_all$DeliveryPointIdentifier == Current_DeliveryPointIdentifier)
}

data <- subset(data_all, condition)


parameters <- input[as.character(input$BookOID)                    == Current_BookOID                    &
                    as.character(input$LoadProfileGroupIdentifier) == Current_LoadProfileGroupIdentifier &
                    as.character(input$DeliveryPointIdentifier)    == Current_DeliveryPointIdentifier,]

output <- parameters[,c('BookOID','LoadProfileGroupIdentifier','DeliveryPointIdentifier','NumberOfNeurons', 'NumberOfDelays',
                       'Count', 'Alpha', 'Beta', 'Date_StartTrain')]

output$Factors <- paste (parameters$ForecastHour,
                         parameters$DayOfWeek,
                         parameters$Month,
                         parameters$WetBulbTemperature,
                         parameters$DewPointTemperature,
                         parameters$WindSpeed,
                         parameters$CloudCover,
                         parameters$WeightedTemperatureHumidityIndex,
                         parameters$ActualLoad,
                         parameters$LagSystemLoad,
                         parameters$Season,
                         parameters$Temperature,
                         parameters$NormalTemperature,
                         parameters$WindChill,
                         parameters$HeatIndex,
                         parameters$RelativeHumidity,
                         parameters$DayTypeCode,
                         parameters$CoolingDegreeHours,
                         parameters$HeatingDegreeHours,
                         sep = '')

if (is.null(output$Date_StartTrain)|is.na(output$Date_StartTrain)){
  Date_StartTrain <- min(data$StartDate)
} else {
  Date_StartTrain <- as.Date(output$Date_StartTrain, format = '%Y-%m-%d')
}

ForecastMonth <- as.Date(ForecastMonth, format = '%m/%d/%Y')

Date_StartTest   <- ForecastMonth
Date_EndTest     <- ForecastMonth + months(1) - days(1)

Date_EndValidation   <- Date_StartTest - months(NumberOfGapMonths) - days(1)
Date_StartValidation <- Date_StartTest - months(NumberOfGapMonths) - months(NumberOfValidationMonths)

Date_EndTrain        <- Date_StartValidation - days(1)


output$Date_StartTrain       <- Date_StartTrain
output$Date_EndTrain         <- Date_EndTrain
output$Date_StartTest        <- Date_StartTest
output$Date_EndTest          <- Date_EndTest
output$Date_StartValidation  <- Date_StartValidation
output$Date_EndValidation    <- Date_EndValidation

channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")

#for assigning ID

sql_output <- sqlQuery(channel, paste("SELECT *  FROM [LoadForecastingAnalytics].[dbo].[R_NeuralNetwork_output] WHERE BatchNo = ", BatchNo, sep= '' ))

if (nrow(sql_output)==0){
    Max_Index <- 0
} else {
    Max_Index <- max (sql_output$RunOID)
}

output$RunOID  <- Max_Index + 1
output$BatchNo <- BatchNo

#scale parameters
scale_parameters <- c (
    'WetBulbTemperature',
    'DewPointTemperature',
    'WindSpeed',
    'CloudCover',
    'WeightedTemperatureHumidityIndex',
    'ActualLoad',
    'LagSystemLoad',
    'Temperature',
    'NormalTemperature',
    'WindChill',
    'HeatIndex',
    'RelativeHumidity',
    'CoolingDegreeHours',
    'HeatingDegreeHours'
)

#factor parameters
factor_parameters <- c (
    'ForecastHour',
    'DayTypeCode',
    'DayOfWeek',
    'Month',
    'Season',
    'LoadProfileGroupIdentifier',
    'DeliveryPointIdentifier'
)

# if it has "1" in input file (we want to use it as a parameter) and it's a factor/scale
factor_inputs <- names(parameters)[parameters[1,]== 1 & is.element(names(parameters), factor_parameters)]
factor_inputs <- union(factor_inputs, c('LoadProfileGroupIdentifier','DeliveryPointIdentifier'))
scale_inputs  <- names(parameters)[parameters[1,]== 1 & is.element(names(parameters), scale_parameters)]


if (is.na(parameters$Count) | is.null(parameters$Count)){
    data$Volume_PerCount <- data$Volume
} else {
    Count <- as.character(parameters$Count)
    data$Volume_PerCount <- data$Volume/data[,Count]
}

n_delays <- output$NumberOfDelays

to_nn_data <- function(data,n_delays) {
  # time series
  ts_columns <- scale_inputs 
  data_ts <- as.data.frame(embed(data.matrix(data[,ts_columns]),n_delays+1))
  names(data_ts) <- 
    paste0(rep(ts_columns,n_delays+1),rep(0:n_delays,rep(length(ts_columns),n_delays+1)))
  
  # dummmies
  data_dummies              <- as.data.frame(data[-(1:n_delays),factor_inputs])
  
  for (i in factor_inputs) {
      data_dummies[,i] <- factor(data_dummies[,i])   
  }
  
  data_out <- as.data.frame(data[-(1:n_delays),c('StartDate','Volume_PerCount', 'BookOID')])
  
  return(cbind(data_out,data_dummies,data_ts))
  
}

data_nn_not_clean <- to_nn_data(data,n_delays)

#Day light savings: exclude them and all delays related to them 
daylight_obs <- which(   data_nn_not_clean$ForecastHour == 2 &
                       ( 
                           data_nn_not_clean$StartDate == as.Date('03/10/2013', format = '%m/%d/%Y') |
                           data_nn_not_clean$StartDate == as.Date('03/11/2013', format = '%m/%d/%Y') | 
                           data_nn_not_clean$StartDate == as.Date('03/09/2014', format = '%m/%d/%Y') |
                           data_nn_not_clean$StartDate == as.Date('03/10/2014', format = '%m/%d/%Y') |
                           data_nn_not_clean$StartDate == as.Date('03/11/2012', format = '%m/%d/%Y') |
                           data_nn_not_clean$StartDate == as.Date('03/12/2012', format = '%m/%d/%Y') |
                           data_nn_not_clean$StartDate == as.Date('03/14/2011', format = '%m/%d/%Y') |
                           data_nn_not_clean$StartDate == as.Date('03/13/2011', format = '%m/%d/%Y')
                       )
)   


#if (is.null(StartExcluding) & is.null(EndExcluding)){
#  
#	null_obs <- daylight_obs
#} else if (!is.null(StartExcluding) & !is.null(EndExcluding)){
#  bad_obs <- which (data_nn_not_clean$StartDate >= as.Date(StartExcluding, format = '%m/%d/%Y') &
#                      data_nn_not_clean$StartDate <= as.Date(EndExcluding, format = '%m/%d/%Y'))
#  
#  null_obs <- union(daylight_obs, bad_obs) 
#
#} else{
#  print("Fix start and end dates for excluding outliers!")
#}

outliers1 <- which (data_nn_not_clean$BookOID == 'Hudson' & data_nn_not_clean$LoadProfileGroupIdentifier == 'NIMO' & data_nn_not_clean$DeliveryPointIdentifier == 'D' &
		data_nn_not_clean$StartDate >= as.Date('05/03/2013', format = '%m/%d/%Y') &
	        data_nn_not_clean$StartDate <= as.Date('05/25/2013', format = '%m/%d/%Y')
		)


outliers2 <- which (data_nn_not_clean$BookOID == 'Hudson' & data_nn_not_clean$LoadProfileGroupIdentifier == 'OR' & data_nn_not_clean$DeliveryPointIdentifier == 'G' &
		data_nn_not_clean$StartDate >= as.Date('09/28/2013', format = '%m/%d/%Y') &
	        data_nn_not_clean$StartDate <= as.Date('10/19/2013', format = '%m/%d/%Y')
		)


outliers3 <- which (data_nn_not_clean$BookOID == 'Just Energy' & data_nn_not_clean$LoadProfileGroupIdentifier == 'NIMO' &
                      data_nn_not_clean$StartDate >= as.Date('07/18/2014', format = '%m/%d/%Y') &
                      data_nn_not_clean$StartDate <= as.Date('07/22/2014', format = '%m/%d/%Y')
)

null_obs <- union(daylight_obs, union(outliers1, union(outliers3,outliers2) ))

all_null_obs <- null_obs

for (i in 1:length(null_obs)) {
  null_obs_subseq <- 1:n_delays + null_obs[i]
  all_null_obs <- union(all_null_obs,null_obs_subseq)
}

data_nn <- data_nn_not_clean[-all_null_obs,]

data_nn_train            <- subset (data_nn, StartDate >= Date_StartTrain       & StartDate <= Date_EndTrain)
data_nn_test             <- subset (data_nn, StartDate >= Date_StartTest        & StartDate <= Date_EndTest)
data_nn_validation       <- subset (data_nn, StartDate >= Date_StartValidation  & StartDate <= Date_EndValidation)


range(data_nn_train$StartDate)
range(data_nn_test$StartDate)
range(data_nn_validation$StartDate)

NumberOfNeurons <- output$NumberOfNeurons
Alpha           <- output$Alpha

#if (is.na(output$Beta) | is.null(output$Beta)){
#    Beta <- NULL
#} else {
#    Beta <- output$Beta
#}

#have to create this dataframe, because need to exclude some columns for training (utility and zone),
#But will need them later. So I will delete them only in this copied table and will train on it
for_training <- data_nn_train

for (rm_factor in c('BookOID', 'LoadProfileGroupIdentifier','DeliveryPointIdentifier')){
  if  (length(unique(for_training[,rm_factor]))==1){
    for_training[,rm_factor] <- list(NULL)
  }
}

if (noise==TRUE){
  for_training$Volume_PerCount <- jitter(for_training$Volume_PerCount, factor = noise_factor)
}

run <- as.data.frame(x=list())
brnn_fit_list <- list()

for (version in 1:NumberOfRuns){

brnn_fit <- brnn_.formula(Volume_PerCount~.-StartDate, data=for_training, neurons=NumberOfNeurons, verbose=Show_Iterations,
                          init_lim=c(-0.5,0.5), alpha_fixed=0.1,beta_fixed=1)
brnn_fit_list <- c(brnn_fit_list,list(brnn_fit))

Train_Mape          <- round(mean((abs(data_nn_train$Volume_PerCount-predict(brnn_fit,data_nn_train))/abs(data_nn_train$Volume_PerCount))[data_nn_train$Volume_PerCount!=0])*100,2) #mape
Train_Weighted_Mape <- round(100*sum(abs(data_nn_train$Volume_PerCount-predict(brnn_fit,data_nn_train)))/sum(abs(data_nn_train$Volume_PerCount)[data_nn_train$Volume_PerCount!=0]),2)
Validation_Mape     <- round(mean((abs(data_nn_validation$Volume_PerCount-predict(brnn_fit,data_nn_validation))/abs(data_nn_validation$Volume_PerCount))[data_nn_validation$Volume_PerCount!= 0])*100,2)  #mape

run[version,'Train_Mape' ]              = Train_Mape
run[version,'Train_Weighted_Mape' ]     = Train_Weighted_Mape
run[version,'Validation_Mape' ]         = Validation_Mape
run[version,'version' ]                 = version
run[version,'gamma' ]                   = brnn_fit$gamma
run[version,'npar' ]                    = brnn_fit$npar
run[version,'effect_numberofneurons' ]  = brnn_fit$gamma/(brnn_fit$npar/NumberOfNeurons)

}

best_run <- run[min(which(run$Validation_Mape==min(run$Validation_Mape))),]
output <- cbind(output, best_run)

brnn_fit_best <- brnn_fit_list[[best_run$version]]

#mypath <- file.path(paste('U:/_Load Forecasting/Victoria/Short Term NN model/Output results/Models/', BatchNo, '/brnn_fit BatchNo ',BatchNo,' RunOID ',output$RunOID,'.RData',sep =''))
#save(brnn_fit_best, file = mypath) 

Test_Mape  <- round(mean((abs(data_nn_test$Volume_PerCount-predict(brnn_fit_best,data_nn_test))/abs(data_nn_test$Volume_PerCount))[data_nn_test$Volume_PerCount!= 0])*100,2)  #mape
output$Test_Mape <- Test_Mape

#############Save testing results################
output_data_test <- as.data.frame(x=list())

output_data_test[1:nrow(data_nn_test),'ActualVolumePerCount' ]      <- data_nn_test$Volume_PerCount
output_data_test[,'ForecastedVolumePerCount' ]  <- predict(brnn_fit_best,data_nn_test)      
output_data_test[,'FlowDate' ]                  <- data_nn_test$StartDate
output_data_test[,'ForecastHour']               <- data_nn_test$ForecastHour
    
output_data_test[,'BatchNo' ]           <- output$BatchNo    
output_data_test[,'RunOID' ]            <- output$RunOID
output_data_test[,'Entity' ]            <- data_nn_test$BookOID 
output_data_test[,'Zone' ]              <- data_nn_test$DeliveryPointIdentifier    
output_data_test[,'Utility' ]           <- data_nn_test$LoadProfileGroupIdentifier

##############Save training results#############
output_data_train <- as.data.frame(x=list())

output_data_train[1:nrow(data_nn_train),'ActualVolumePerCount' ]      <- data_nn_train$Volume_PerCount
output_data_train[,'ForecastedVolumePerCount' ]  <- predict(brnn_fit_best,data_nn_train)      
output_data_train[,'FlowDate' ]                  <- data_nn_train$StartDate
output_data_train[,'ForecastHour']               <- data_nn_train$ForecastHour

output_data_train[,'BatchNo' ]           <- output$BatchNo    
output_data_train[,'RunOID' ]            <- output$RunOID
output_data_train[,'Entity' ]            <- data_nn_train$BookOID 
output_data_train[,'Zone' ]              <- data_nn_train$DeliveryPointIdentifier    
output_data_train[,'Utility' ]           <- data_nn_train$LoadProfileGroupIdentifier

###################Save validation results########

output_data_validation <- as.data.frame(x=list())

output_data_validation[1:nrow(data_nn_validation),'ActualVolumePerCount' ]  <- data_nn_validation$Volume_PerCount
output_data_validation[,'ForecastedVolumePerCount' ]  <- predict(brnn_fit_best,data_nn_validation)      
output_data_validation[,'FlowDate' ]                  <- data_nn_validation$StartDate
output_data_validation[,'ForecastHour']               <- data_nn_validation$ForecastHour

output_data_validation[,'BatchNo' ]           <- output$BatchNo    
output_data_validation[,'RunOID' ]            <- output$RunOID
output_data_validation[,'Entity' ]            <- data_nn_validation$BookOID 
output_data_validation[,'Zone' ]              <- data_nn_validation$DeliveryPointIdentifier    
output_data_validation[,'Utility' ]           <- data_nn_validation$LoadProfileGroupIdentifier

######################################################

output$Comment <- Comment
# put them in correct order
output <- output[,c('BatchNo','RunOID','BookOID', 'LoadProfileGroupIdentifier', 'DeliveryPointIdentifier',
                    'NumberOfNeurons','NumberOfDelays', 'Count', 'Factors', 'Date_StartTrain', 'Date_EndTrain', 
                    'Date_StartValidation', 'Date_EndValidation', 'Date_StartTest', 'Date_EndTest',
                    'Train_Mape', 'Validation_Mape', 'Test_Mape','Comment','Alpha', 'Beta')]


channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")
sqlSave(channel,output, tablename = 'R_NeuralNetwork_output', rownames = FALSE,
        varTypes=c(Date_EndTrain        ="Date",
                   Date_StartTest       ="Date",
                   Date_EndTest         ="Date",
                   Date_StartValidation ="Date",
                   Date_EndValidation   ="Date",
                   Date_StartTrain      ="Date" ),append=TRUE)

#################
output_data_test <- output_data_test[,c('BatchNo','RunOID','Entity','Zone','Utility','FlowDate',
                                        'ForecastHour','ActualVolumePerCount','ForecastedVolumePerCount')]

channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")
sqlSave(channel,output_data_test, tablename = 'R_NeuralNetwork_testdata', rownames = FALSE,
        varTypes=c(FlowDate ="Date" ),append=TRUE)

#################
output_data_train <- output_data_train[,c('BatchNo','RunOID','Entity','Zone','Utility','FlowDate',
                                        'ForecastHour','ActualVolumePerCount','ForecastedVolumePerCount')]

channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")
sqlSave(channel,output_data_train, tablename = 'R_NeuralNetwork_traindata', rownames = FALSE,
        varTypes=c(FlowDate ="Date" ),append=TRUE)
#################

output_data_validation <- output_data_validation[,c('BatchNo','RunOID','Entity','Zone','Utility','FlowDate',
                                        'ForecastHour','ActualVolumePerCount','ForecastedVolumePerCount')]

channel <- odbcDriverConnect(connection = "DRIVER={SQL Server}; SERVER=DBACM\\ARCHIMEDES; DATABASE=LoadForecastingAnalytics")
sqlSave(channel,output_data_validation, tablename = 'R_NeuralNetwork_validationdata', rownames = FALSE,
        varTypes=c(FlowDate ="Date" ),append=TRUE)
