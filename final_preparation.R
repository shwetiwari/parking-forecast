if (!require("stats")) install.packages("stats", dependencies = TRUE)
if (!require("aTSA")) install.packages("aTSA", dependencies = TRUE)
if (!require("forecast")) install.packages("forecast", dependencies = TRUE)
if (!require("astsa")) install.packages("astsa", dependencies = TRUE)
if (!require("fasttime")) install.packages("fasttime", dependencies = TRUE)
if (!require("imputeTS")) install.packages("imputeTS", dependencies = TRUE)
if (!require("lubridate")) install.packages("lubridate", dependencies = TRUE)
if (!require("keras")) install.packages("keras", dependencies = TRUE)
if (!require("tensorflow")) install.packages("tensorflow", dependencies = TRUE)
if (!require("data.table")) install.packages("data.table", dependencies = TRUE)
if (!require("R.utils")) install.packages("R.utils", dependencies = TRUE)
if (!require("Metrics")) install.packages("R.utils", dependencies = TRUE)

library (stats)
library (aTSA)
library (forecast)
library (astsa)
library (fasttime)
library (imputeTS)
library (lubridate)
library(keras)
library(tensorflow)
library (data.table)
library (R.utils)
library (Metrics)

###Data preparation functions###

#Making the path and global variables filenames
PATH = "/Users/shweta/Documents/Project/parkingdata"
filenames <- list.files(PATH, full.names=TRUE)
filenames2 <- list.files(PATH, full.names=FALSE)

#This function reads the data of a specific garage and returns it
startReading <- function(i){
  showConnections(all=TRUE)
  zz = gzfile(filenames[i], 'rt' )  
  dat = read.csv(zz,header=TRUE, sep = ';')
  check = colnames(dat)
  head(dat)
  check == colnames(dat)
  close(zz)
  length(filenames)
  dat['NameOfParkingSpace'] = filenames2[1]
  return (dat)
}

#Function that fills in all the missing holes in a given dataset
fillMissingValues <- function(dataset, observationsPerDataFrame = 5000){
  dataset$pubdate = as.character(dataset$pubdate)
  
  #creates new vector with as size the difference in minutes between the first and last date of the dataset
  fillVector = as.data.frame(matrix(nrow = as.integer(round(difftime(fastPOSIXct(dataset$pubdate[length(dataset$pubdate)]), fastPOSIXct(dataset$pubdate[1]), units = c("mins")))), ncol = length(dataset[1, ])))
  colnames(fillVector) = colnames(dataset)
  fillVectorIndex = 1
  
  #Working with a big dataset is comparably very slow, so the data is split up in smaller dataframes (standard length 5000)
  for (datasetNumber in 1:(as.integer((length(dataset$pubdate) - 1)/observationsPerDataFrame) + 1)){
    #when it isn't the last datasubset yet
    if (datasetNumber < as.integer((length(dataset$pubdate) - 1)/observationsPerDataFrame) + 1){
      dataSubset = dataset[((datasetNumber - 1)*observationsPerDataFrame + 1):(datasetNumber*observationsPerDataFrame),] #the subset is the complete length of a subset
      endDate = dataset[datasetNumber * observationsPerDataFrame + 1,]$pubdate #the correct length after missing values are filled up is the difference in minutes between the first value of this subset and the first value of the next subset
    }else{ #otherwise
      dataSubset = dataset[((datasetNumber - 1)*observationsPerDataFrame + 1):length(dataset$pubdate), ] #the subset is the remaing values
      endDate = dataSubset[length(dataSubset$pubdate),]$pubdate #the correct length after missing values are filled up is the difference in minutes between the first value of this subset and the last value of this subset
    }
    
    #to avoid memory reallocation, the vector with added values is already initialized with the correct length
    times = fastPOSIXct(dataSubset$pubdate, tz="GMT")
    lengthAddingVector = as.integer(round(difftime(fastPOSIXct(endDate), times[1], units = c("mins"))))
    addingVector = as.data.frame(matrix(nrow = lengthAddingVector, ncol = length(dataset[1, ])))
    colnames(addingVector)=colnames(dataset)
    
    indexDataSubset=1
    indexAddingVector=1
    
    #loops through the whole subset
    while (indexDataSubset < length(dataSubset$pubdate)){
      correctValues = 0
      #searches untill a uncorrect (more than 1.65 minute between 2 observations) values is found
      
      while((difftime(times[indexDataSubset + correctValues +1 ], times[indexDataSubset + correctValues], units = c("mins")) <= 1.65) & (indexDataSubset + correctValues + 1) < length(dataSubset$pubdate)){
        correctValues = correctValues + 1
      }
      #and copies every value untill then into the new vector
      addingVector[indexAddingVector:(indexAddingVector + correctValues), ] = dataSubset[indexDataSubset:(indexDataSubset + correctValues), ]
      indexAddingVector = indexAddingVector + correctValues
      indexDataSubset = indexDataSubset + correctValues
      
      #when the uncorrect values was the end of the subset
      if (indexDataSubset == length(dataSubset$pubdate)){
        addingVector[indexAddingVector + 1, ] = dataSubset[indexDataSubset + 1, ] #the last value is added tot the vector
      }else{ #otherwise
        #searches and adds the missing row(s) untill the next correct timestamp is reached.
        while(difftime(times[indexDataSubset + 1], fastPOSIXct(addingVector$pubdate[indexAddingVector]), units = c("mins")) > 1.65){
          copy = data.frame(addingVector[indexAddingVector, ])
          copy$pubdate = as.character(fastPOSIXct(copy$pubdate, tz="GMT") + 60) #adds one minute to the last known time
          copy$state = 1 #the missing value is treated as an error
          addingVector[indexAddingVector + 1,]=copy
          indexAddingVector = indexAddingVector + 1
        }
      }
      indexAddingVector = indexAddingVector + 1
      indexDataSubset = indexDataSubset + 1
    }
    if (indexDataSubset == length(dataSubset$pubdate)){
      addingVector[indexAddingVector, ] = dataSubset[indexDataSubset, ] #the last value is added tot the vector
    }
    #when there is a timegap between the last value of this subset and the first of the next subset, this gap will be filled
    while(indexAddingVector < length(addingVector$pubdate)){
      copy = data.frame(addingVector[indexAddingVector, ])
      copy$pubdate = as.character(fastPOSIXct(copy$pubdate, tz="GMT") + 60) #adds one minute to the last known time
      copy$state = 1 #the missing value is treated as an error
      addingVector[indexAddingVector+1, ] = copy
      indexAddingVector = indexAddingVector + 1
    }
    #the subset is added to the complete filled dataset
    fillVector[fillVectorIndex:(fillVectorIndex + length(addingVector$pubdate) -1 ), ] = addingVector
    fillVectorIndex = fillVectorIndex + length(addingVector$pubdate)
  }
  return(fillVector)
}
#Function that fills in all the missing holes in all datasets
CreateFilledInUncleanDataset <- function(range){
  #creates list where datasets are stored in. Dataset i can be called with filledInUncleanDataset[[i]]
  filledInUncleanDataset = list()
  #for all datasets
  for (datasetIndex in range){
    #logs time to keep track on progress (will take about an hours)
    star_time = Sys.time()
    uncleanDataset = startReading(datasetIndex)
    if (datasetIndex==47){
      uncleanDataset = uncleanDataset[3794:length(uncleanDataset$pubdate),]
    }
    filledInUncleanDataset[[datasetIndex]] = uncleanDataset
    print(paste("Dataset", datasetIndex, "/57 has finished filling in", round(difftime(Sys.time(), star_time, units = c("sec")),2), "seconds."))
  }
  return(filledInUncleanDataset)
}

KalmanNanSolver<-function(dfp){
  #does a kalman filter NaN replacement
  dfp =(na_kalman(dfp, type="level"))
}

#prepares the data for predictions
final_data_prep<-function(df, range, approach = "median"){
  cleanDataset = list()
  for (i in range){
    print(i)
    #set everything except correct values to NaN
    df[[i]]$freespaceshort[df[[i]]$state!=2] = NaN
    #go from freespaces to occupiedspaces
    minuteData = df[[i]]$shortcapacity - df[[i]]$freespaceshort
    minuteCapacity = df[[i]]$shortcapacity
    #uses Kalman to replace the NaN values
    if(sum(i == c(47,48,51,52))==1){
      #one of the datasets with only NaN
      minuteData[1:length(minuteData)] = 0
    }else if(sum(i == c(23,56))==1){
      #fixes bug with datasets 23 and 56
      minuteData[1:(length(minuteData)-1)] = KalmanNanSolver(minuteData[1:(length(minuteData)-1)])
    }else{
      minuteData = KalmanNanSolver(minuteData)
    }
    
    j = 1
    #make sure that we start in the first minute of an hour
    while(minute(df[[i]]$pubdate[j]) != 0 && j < length(minuteData)){
      j = j+1
    }
    if (j != 1){
      #determines the amount of hours left
      cleanData = matrix(nrow = ceiling((length(minuteData)-j)/60)+1)
      capacity = matrix(nrow = ceiling((length(minuteData)-j)/60)+1)
      #gives a value to the first hour
      if (approach == "mean"){
        cleanData[1]=mean(minuteData[1:(j-1)], na.rm = TRUE)
        capacity[1]=mean(minuteCapacity[1:(j-1)], na.rm = TRUE)
      }else if(approach == "max"){
        cleanData[1]=max(minuteData[1:(j-1)], na.rm = TRUE)
        capacity[1]=max(minuteCapacity[1:(j-1)], na.rm = TRUE)
      }else if(approach == 'median'){
        cleanData[1]=median(minuteData[1:(j-1)], na.rm = TRUE)
        capacity[1]=median(minuteCapacity[1:(j-1)], na.rm = TRUE)
      }
      else{
        print('approach need to be in set: [median, max, or mean]')
      }
      #calculates for the rest of the hourly statistic
      for (k in 2:(length(cleanData))){
        startHour = (j+60*(k-2))
        endHour = startHour +60-1
        if (approach == "mean"){
          cleanData[k]=mean(minuteData[startHour:endHour], na.rm = TRUE)
          capacity[k]=mean(minuteCapacity[startHour:endHour], na.rm = TRUE)
        }else if(approach == "max"){
          cleanData[k]=max(minuteData[startHour:endHour], na.rm = TRUE)
          capacity[k]=max(minuteCapacity[startHour:endHour], na.rm = TRUE)
        }else if(approach == 'median'){
          cleanData[k]=median(minuteData[startHour:endHour], na.rm = TRUE)
          capacity[k]=median(minuteCapacity[startHour:endHour], na.rm = TRUE)
        }
        else{
          print('approach need to be in set: [median, max, or mean]')
        }
      }
    }else{
      #determines the amount of hours left
      cleanData = matrix(nrow = ceiling((length(minuteData)-j)/60))
      #calculates the statistic for the following hours
      for (k in 1:(length(cleanData))){
        startHour = (j+60*(k-1))
        endHour = startHour +60-1
        if (approach == "mean"){
          cleanData[k]=mean(minuteData[startHour:endHour], na.rm = TRUE)
          capacity[k]=mean(minuteCapacity[startHour:endHour], na.rm = TRUE)
        }else if(approach == "max"){
          cleanData[k]=max(minuteData[startHour:endHour], na.rm = TRUE)
          capacity[k]=max(minuteCapacity[startHour:endHour], na.rm = TRUE)
        }else if (approach == 'median'){
          cleanData[k]=median(minuteData[startHour:endHour], na.rm = TRUE)
          capacity[k]=median(minuteCapacity[startHour:endHour], na.rm = TRUE)
        }
        else{
          print('approach need to be in set: [median, max, or mean]')
        }
      }
    }
    cleanDataset[[i]]=cleanData
    capacities[[i]]<<-capacity[(length(capacity)-167):length(capacity)]
  }
  return(cleanDataset)
}
cleanedMinuteSet = CreateFilledInUncleanDataset(c(1:57))
capacities = list()
prep_data  = final_data_prep(cleanedMinuteSet, c(1:57))
capacityMatrix = matrix(nrow = 3780, ncol = 57)
for (index in 1:57){
  capacityMatrix[,index]=rep(capacities[[index]], length.out = 3780)
}

prep_data_training = list()
prep_data_test = list()

#splits the data in a training (80%) en test (20%) set.
for (i in 1:57){
  N = length(prep_data[[i]])
  n = round(N *0.8, digits = 0)
  prep_data_training[[i]] = prep_data[[i]][1:n]
  prep_data_test[[i]] = prep_data[[i]][(n+1):N]
}

#SARIMA models
predSARIMA <- function(datasetComplete, range){
  if (findfrequency(datasetComplete[[i]])>1){
    tsdataset = ts(datasetComplete[[i]], frequency = 168)
    model = auto.arima(tsdataset, D=1, seasonal = TRUE, stepwise = TRUE, max.p = 3, max.q =3)
  }else{
    tsdataset = ts(datasetComplete[[i]], frequency = 1)
    model = auto.arima(tsdataset, D=0, stepwise = TRUE, max.p = 3, max.q =3)
  }
  model
}

SARIMAModels = list()
for (i in 1:57){
  SARIMAModels[[i]] = predSARIMA(prep_data_training, i)
  save.image()
}

#dsHW models
#HWModels = list()
for (i in 1:57){
  print(i)
  HWModels[[i]]=dshw(prep_data_training[[i]]+100, period1 = 24, period2 = 168, h = length(prep_data_test[[i]]))  
  save.image()
}

#seasonal Naive models
SNaiveModels = list()
for(index in 1:57){
  SNaiveTS = ts(prep_data_training[[index]], frequency=168)
  SNaiveModels[[index]] = snaive(SNaiveTS, h = length(prep_data_test[[index]]))
}

#Calculate Weighted Mean Absolute Percentage Error
WMAPE <- function(actual, predicted){
  absoluteerror = sum(abs(actual-predicted))
  ratioerror = absoluteerror/sum(predicted)
}

#SARIMA prediction and metric scores
SARIMAModelPredictions = list()
SARIMAPerformance = list()
for(index in 1:57){
  if (!is.null(SARIMAModels[[index]])){
    SARIMAModelPredictions[[index]] = forecast(SARIMAModels[[index]], h = length(prep_data_test[[index]]))
    SARIMAMAE = mae(prep_data_test[[index]], SARIMAModelPredictions[[index]]$mean)
    SARIMAWAPE = WMAPE(prep_data_test[[index]], SARIMAModelPredictions[[index]]$mean)
    SARIMARMSE = rmse(prep_data_test[[index]], SARIMAModelPredictions[[index]]$mean)
  }else{
    SARIMAModelPredictions[[index]] = numeric(length(prep_data_test[[index]]))
    SARIMAMAE = mae(prep_data_test[[index]], SARIMAModelPredictions[[index]])
    SARIMAWAPE = WMAPE(prep_data_test[[index]], SARIMAModelPredictions[[index]])
    SARIMARMSE = rmse(prep_data_test[[index]], SARIMAModelPredictions[[index]])
  }
  SARIMAPerformance[[index]] = data.frame(SARIMAMAE, SARIMAWAPE, SARIMARMSE)
  print(SARIMAPerformance[[index]])
}

#dsHW prediction and metric scores
HWModelPredictions = list()
HWPerformance = list()
for(index in 1:57){
  HWModelPredictions[[index]] = forecast(HWModels[[index]], h = length(prep_data_test[[index]]))
  HWModelPredictions[[index]]$mean = HWModelPredictions[[index]]$mean-100
  HWMAE = mae(prep_data_test[[index]], HWModelPredictions[[index]]$mean)
  HWWAPE = WMAPE(prep_data_test[[index]], HWModelPredictions[[index]]$mean)
  HWRMSE = rmse(prep_data_test[[index]], HWModelPredictions[[index]]$mean)
  print(index)
  HWPerformance[[index]] = data.frame(HWMAE, HWWAPE, HWRMSE)
  print(HWPerformance[[index]])
}

#Seasonal naive prediction and metric scores
SNaiveModelPredictions = list()
SNaivePerformance = list()
for(index in 1:57){
  SNaiveModelPredictions[[index]] = forecast(SNaiveModels[[index]], h = length(prep_data_test[[index]]))
  SNaiveModelPredictions[[index]]$mean = SNaiveModelPredictions[[index]]$mean
  SNaiveMAE = mae(prep_data_test[[index]], SNaiveModelPredictions[[index]]$mean)
  SNaiveWAPE = WMAPE(prep_data_test[[index]], SNaiveModelPredictions[[index]]$mean)
  SNaiveRMSE = rmse(prep_data_test[[index]], SNaiveModelPredictions[[index]]$mean)
  print(index)
  SNaivePerformance[[index]] = data.frame(SNaiveMAE, SNaiveWAPE, SNaiveRMSE)
  print(SNaivePerformance[[index]])
}

#prints best metric score for eacht method per dataset
for(index in 1:57){
  print(paste("location", index))
  print(which.min(c(abs(SARIMAPerformance[[index]][1]), abs(HWPerformance[[index]][1]), abs(SNaivePerformance[[index]][1]))))
  print(which.min(c(abs(SARIMAPerformance[[index]][2]), abs(HWPerformance[[index]][2]), abs(SNaivePerformance[[index]][2]))))
  print(which.min(c(abs(SARIMAPerformance[[index]][3]), abs(HWPerformance[[index]][3]), abs(SNaivePerformance[[index]][3]))))
}

#Calculates final predictions
final_predictions = matrix(nrow = 3780, ncol=57*4)
for(index in 1:57){
  model = SARIMAModels[[index]]
  p = length(model$phi)
  q = length(model$theta)
  D = 1
  final_model = arima(prep_data[[index]], order = c(p,0,q), seasonal = list(order = c(0,D,0), period = 168))
  prediction = forecast(final_model, h = 3780)
  for(predictionIndex in 1:3780){
    final_predictions[predictionIndex,((index-1)*4+1)] = median(c(0, prediction$lower[predictionIndex,1], capacityMatrix[predictionIndex,i]))
    final_predictions[predictionIndex,((index-1)*4+2)] = median(c(0, prediction$mean[predictionIndex], capacityMatrix[predictionIndex,i]))
    final_predictions[predictionIndex,((index-1)*4+3)] = median(c(0, prediction$upper[predictionIndex,1], capacityMatrix[predictionIndex,i]))
    final_predictions[predictionIndex,((index-1)*4+4)] = capacityMatrix[predictionIndex,i]
  }
}

#get colnames of dataset
names = data.frame(matrix(nrow = 1, ncol=4*57))
for(index in 1:57){
  names[1,((index-1)*4+1):(index*4)] = startReading(index)$name[1]
}

#the latitudes and longitudes from the dataset can be wrong, so these are hardcoded
latitudes = c(52.377864, 52.357382, 52.353435, 52.352791, 52.403327, 52.367738, 52.36242, 52.39009, 52.343, 52.37788, 52.37907, 52.36988, 52.3619, 52.35726, 52.37639, 52.37382, 52.36758, 52.36921, 52.36981, 52.3681, 52.38462, 52.31409, 52.31086, 52.313, 52.312, 52.3125, 52.3078, 52.31522, 52.31413, 52.31765, 52.37601, 52.3849, 52.3362, 52.37933, 52.37135, 52.35694, 52.38482, 52.31542, 52.3444, 52.31409, 52.31476, 52.3338, 52.36988, 52.31477, 52.334, 52.337235, 52.339792, 52.366209, 52.374168, 52.355636, 52.366096, 52.346638, 52.341251, 52.358753, 52.372163, 52.366058, 52.367258)
longitudes = c(4.915205, 4.892389, 4.890097, 4.890476, 4.93404, 4.868062, 4.891446, 4.83811, 4.8536, 4.91739, 4.89722, 4.87663, 4.88001, 4.87944, 4.89459, 4.89508, 4.90041, 4.90437, 4.90857, 4.91368, 4.86627, 4.94105, 4.93985, 4.93866, 4.93866, 4.94441, 4.9413, 4.94731, 4.95059, 4.95546, 4.90901, 4.88464, 4.8609, 4.84537, 4.96043, 4.92911, 4.89475, 4.93131, 4.8536, 4.943, 4.95268, 4.864, 4.96447, 4.9548, 4.8595, 4.873558, 4.873351, 4.89179, 4.894958, 4.887128, 4.894027, 4.918427, 4.873056, 4.882659, 4.959019, 4.898584, 4.890751)

coordinates = data.frame(matrix(nrow = 2, ncol=4*57))
for(index in 1:57){
  coordinates[1,((index-1)*4+1):(index*4)] = latitudes[index]
  coordinates[2,((index-1)*4+1):(index*4)] = longitudes[index]
}

#saves begin date
dateStart = data.frame(matrix(nrow = 1, ncol=4*57, data = as.integer(as.POSIXct("2019-07-31 18:00", origin="1970-01-01"))))


parking_predictions = t(t(rbind(coordinates, dateStart)))
parking_predictions = rbind(parking_predictions, final_predictions)
colnames(parking_predictions) = names
write.csv2(parking_predictions, 'Parking Prediction.csv', )


### LSTM for short term predictions ###
#LSTM
# creates a lagged dataset for supervised learning
lags <- function(x, k){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}


## scale data
normalizeData <- function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}


## inverse-transform
inverter = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  n = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(n)
  
  for( i in 1:n){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}


## fit the model

fitModel <- function(x_train) {
  dim(x_train) <- c(length(x_train), 1, 1)
  dim(x_train)
  X_shape2 = dim(x_train)[2]
  X_shape3 = dim(x_train)[3]
  batch_size = 1
  units = 1
  
  model <- keras_model_sequential()
  model%>%
    layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam( lr= 0.02 , decay = 1e-6 ),
    metrics = c('accuracy')
  )
  
  summary(model)
  
  nb_epoch = 50
  for(i in 1:nb_epoch ){
    model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
    model %>% reset_states()
  }
  return(model)
}

#predict
getPredictions <- function(model, Scaled, x_test){
  L = length(x_test)
  dim(x_test) = c(length(x_test), 1, 1)
  
  scaler = Scaled$scaler
  
  predictions = numeric(L)
  for(i in 1:L){
    X = x_test[i , , ]
    dim(X) = c(1,1,1)
    # forecast
    yhat = model %>% predict(X, batch_size=batch_size)
    
    # invert scaling
    yhat = inverter(yhat, scaler,  c(-1, 1))
    
    # invert differencing
    yhat  = yhat + Series[(n+i)]
    
    # save prediction
    predictions[i] <- yhat
  }
  return(predictions)
}


#lstm calculations

#remove hardcoded df
df = loadSmallDataset(filenames[23:24])
df = df[c(1:100000),]
Series = df$freespaceshort
times = df$pubdate

# transform data to stationarity
diffed = diff(Series, differences = 1)
supervised = lags(diffed, 1)

# split into train and test sets

N = nrow(supervised)
n = round(N *0.66, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]

Scaled = normalizeData(train, test, c(-1, 1))

y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]

y_test = Scaled$scaled_test[, 2]
x_test = Scaled$scaled_test[, 1]

# fit the model
model = fitModel(x_train)

predictions = getPredictions(model, Scaled, x_test)

### Other functions that are used during the project ###
PATH2 = 'C:/Users/shweta/Downloads/Prep_Data'
PATH3 = 'C:/Users/shweta/Downloads/parking_part2'
filenames3 <- list.files(PATH2, full.names=TRUE)
filenames4 <- list.files(PATH3, full.names=TRUE)

begin = 48219
middle = 123775
end = 177550

#These values are up to exactly a week before the last prediction of the different dates
beginFilled = 39199
middleFilled = 115759
endFilled = 173659

#old data
garage23 = startReading(23)
garage56 = startReading(56)
garage29 = startReading(29)
garage44 = startReading(44)

garage23 = fillMissingValues(garage23)
garage56 = fillMissingValues(garage56)
garage29 = fillMissingValues(garage29)
garage44 = fillMissingValues(garage44)

garage23[garage23$state!=2,]$freespaceshort = NaN
garage56[garage56$state!=2,]$freespaceshort = NaN
garage29[garage29$state!=2,]$freespaceshort = NaN
garage44[garage44$state!=2,]$freespaceshort = NaN

garage23[, c(11)] = KalmanNanSolver2(garage23[,(c(11))])
garage56[, c(11)] = KalmanNanSolver2(garage56[,(c(11))])
garage29[, c(11)] = KalmanNanSolver2(garage29[,(c(11))])
garage44[, c(11)] = KalmanNanSolver2(garage44[,(c(11))])


#new data
unpreparedgarage23new = startReading2(23)
unpreparedgarage56new = startReading2(56)
unpreparedgarage29new = startReading2(29)
unpreparedgarage44new = startReading2(44)

garage23new = fillMissingValues(unpreparedgaragegarage23new)
garage56new = fillMissingValues(unpreparedgaragegarage56new)
garage29new = fillMissingValues(unpreparedgaragegarage29new)
garage44new = fillMissingValues(unpreparedgaragegarage44new)

garage23new[garage23new$state!=2,]$freespaceshort = NaN
garage56new[garage56new$state!=2,]$freespaceshort = NaN
garage29new[garage29new$state!=2,]$freespaceshort = NaN
garage44new[dgarage44new$state!=2,]$freespaceshort = NaN

garage23new[, c(11)] = KalmanNanSolver2(garage23new[,(c(11))])
garage56new[, c(11)] = KalmanNanSolver2(garage56new[,(c(11))])
garage29new[, c(11)] = KalmanNanSolver2(garage29new[,(c(11))])
garage44new[, c(11)] = KalmanNanSolver2(garage44new[,(c(11))])

KalmanNanSolver2<-function(dfp){
  #does a kalman filter NaN replacement
  dfp_scaled <- scales:::rescale(dfp, c(0, 1))
  dfp_scaled <- na_kalman(dfp_scaled, type ='level')
  dfp <- scales:::rescale(dfp_scaled, c(min(dfp, na.rm = T), max(dfp, na.rm = T)))
  return(dfp)
}


final_data_prep<-function(){
  star_time = Sys.time()
  df = startReading(1)
  df = fillMissingValues(df)
  df[df$state!=2,]$freespaceshort = NaN
  df = KalmanNanSolver(df)
  print(Sys.time()-star_time)
  for (i in 2:length(filenames)){
    temp_df = startReading(1)
    temp_df = fillMissingValues(temp_df)
    temp_df[temp_df$state!=2,]$freespaceshort = NaN
    temp_df = KalmanNanSolver(temp_df)
    df = rbind(df, temp_df)
    print(Sys.time()-star_time)
    
  }
  return(df)  
}


#This function gives us some basic statistics and the occupancy rate
descriptiveStatistics <- function(dataFrame){
  #stat = summary(dataFrame$shortcapacity)
  #st.err = sd(dataFrame$shortcapacity)
  stat = summary(dataFrame$freespaceshort)
  st.err = sd(dataFrame$freespaceshort)
  dataFrame['occupancyRate'] = 1 - dataFrame$freespaceshort/dataFrame$shortcapacity
  occupancy = (dataFrame['occupancyRate'])
  realmean = mean(dataFrame[["occupancyRate"]], na.rm = TRUE)
  return(list(stat, st.err, realmean))
  #occupancy[1:50,] to make it shorty
}  

#This function can plot the freespaceshort, and capacity
plotFunction <- function(dataframe, begin, ending){
  plot(strptime(x = as.character(dataframe$pubdate[begin:ending]), format = "%Y-%m-%d %H:%M"),
       main = dataframe$name[1], dataframe$freespaceshort[begin:ending], xlab = '', ylab = 'parking spots', col = 'green', ylim = c(0,max(dataframe$shortcapacity)))
  lines(strptime(x = as.character(dataframe$pubdate[begin:ending]),
                 format = "%Y-%m-%d %H:%M"), dataframe$shortcapacity[begin:ending], col = 'red')
  legend("bottomleft", legend=c("Availability", "Capacity"),
         col=c("green", "red"), lty=1:1, cex=0.8)
}

#This function calculates the Mean Absolute Error
MAE <- function(array1, array2){
  absoluteerror = abs(array1-array2)
  mae = mean(absoluteerror)
  return (mae)
}  
#This function calculates the Mean Absolute Error
MAE <- function(array1, array2){
  absoluteerror = abs(array1-array2)
  mae = mean(absoluteerror)
  return (mae)
}  
#This function calculates the Mean Absolute Percentage Error
MAPE <- function(array1, array2){
  absoluteerror = abs(array1-array2)
  ratioerror = absoluteerror/array2
  return (mean(ratioerror)*100)
}
#This function calculates the Root Mean Squared Error
RMSE <- function(array1, array2){
  meansquareerror = mean((array1-array2)^2)
  rmse = sqrt(meansquareerror)
  return (rmse)
}

#This function prints all the criteria and takes as input, two arrays. It calculates WAPE instead of MAPE
modelComparison <- function(predictions, realvalues){
  realvalues1 = realvalues
  realvalues1[which(realvalues1 >= 0)] = mean(realvalues1)
  meaned = realvalues1
  mae = MAE(predictions, realvalues)
  wape = MAPE(predictions, meaned)
  rmse = RMSE(predictions, realvalues)
  #cat("\n", "MAE =",mae, "\n", "WAPE =",wape,"\n", "RMSE =",rmse)
  return (c(mae, wape, rmse))
}

modelsAnalysis <- function(dataframe, lastvalue, weekbefore){
  naive = JunNaivePrediction(dataframe, weekbefore)
  exponential = exponentialSmoothing(dataframe, lastvalue)
  Sarima = arimaModel(dataframe, lastvalue)
  cat("\n", "MAE =",naive[1], "\n", "WAPE =",naive[2],"\n", "RMSE =",naive[3], "For Naive")
  cat("\n", "MAE =",exponential[1], "\n", "WAPE =",exponential[2],"\n", "RMSE =",exponential[3], "For Exponential")
  cat("\n", "MAE =",Sarima[1], "\n", "WAPE =",Sarima[2],"\n", "RMSE =",Sarima[3], "Sarima")
  criteria = matrix(c(naive[1],exponential[1], Sarima[1],naive[2],exponential[2],Sarima[2],naive[3],exponential[3], Sarima[3]), nrow = 3, ncol = 3)
  barplot(criteria, main="Model Comparison - Naive | Exponential | Sarima",
          xlab="Criteria", col=c("darkblue","grey", "darkgreen"), names.arg=c("MAE", "WAPE", "RMSE"), beside=TRUE)
  legend("topleft", legend =c("naive", "exponential", "sarima"), fill=c("darkblue", "grey", "darkgreen"), cex=0.8)
}


exponentialSmoothing <- function(dataframe, lastvalue){
  trainingData = dataframe[(1):(lastvalue),c(11)]
  validation = dataframe[((lastvalue +1):(lastvalue+60)),c(11)]
  modelES = forecast(ts(trainingData), h = 60)
  forecasts = modelES$mean
  #availability
  write.csv(forecasts, 'forecastexp.csv', row.names = FALSE)
  return(modelComparison(forecasts, validation))
}


JunNaive <- function(dataframe, k, z){
  lengthweek = 7*60*24
  d = dataframe%>% select(11)
  for (j in (1:z)){
    lastValue = k + j  - (lengthweek)
    d[(k+j ),] = d[lastValue,]
  }
  predictions = as.numeric(unlist(d[(k+1):(k+z),]))
  predictions = matrix(predictions, z, 1)
  return(predictions)
}

NaiveAlternative <- function(dataframe){
  hourlyforecast = array((1:(3780)))
  forecast = JunNaive(dataframe, nrow(dataframe), (3780*60 ))
  for (i in (1:2)){
    hourlyforecast[i] = mean(forecasts[((i-1)*60+i:(i*60)+i)])
  }
  totalforecast = data.frame(hourlyforecast)
  return(totalforecast)
}

Arima <- function(dataframe){
  hourlyforecast = array((1:3780))
  cleanedData = fillInOverall(c1)
  trainingData = cleanedData[, c(10)] - cleanedData[,c(11)]    
  modelSAR = auto.arima(ts(trainingData))
  forecasts = forecast(modelSAR, h = 3780*60)$mean
  for (i in (1:3780)){
    hourlyforecast[i] = forecasts[(i-1)*60+1]
  }
  totalforecast = data.frame(hourlyforecast)
  return(totalforecast)
}
'
'

snaivePrediction <- function(dataframe, lastvalue){
  trainingData = dataframe[(1):(lastvalue),c(11)]
  validation = dataframe[((lastvalue +1):(lastvalue+60)),c(11)]
  modelnaive = snaive(ts(trainingData), h = 60)
  forecasts = modelnaive$mean
  return(modelComparison(forecasts, validation))
}


findParameters <- function(dataframe, lastvalue, functionfill, P, D, Q){
  full = addMissingRows(dataframe)
  count = nrow(full[(1:which(rownames(full) == lastvalue)),]) - nrow(dataframe[(1:lastvalue),])
  cleanedData = fillInOverall(full)
  trainingData = cleanedData[(1):(lastvalue+count),c(11)]
  modelAIC = array(1:(P*D*Q), dim = c(P,Q, D))
  for (p in (1:P)){
    for (q in (1:Q)){
      for (d in (1:D)){
        modelAIC[P,Q,D] = sarima(trainingData, 60, 0, 60, S = (7*24*60))$mean
      }
    }
  }
  best = arg.max
  model = sarima
}
