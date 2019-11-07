################################################################
#IoT Analytics
#Evaluate Techniques for WiFi Locationing 

#Model for determining latitude

#Created by Eirik Espe
################################################################

source("Model for longitude.R")


#--- Creating the first model ----

#Set seed
set.seed(123)

# Create a sample for the dataset
sampleLat <- createDataPartition(data$LATITUDE,
                               p = .30,
                               list = FALSE)
sampleDataLat <- data[sampleLat,]

#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(sampleDataLat$LATITUDE, 
                                  p = .80, 
                                  list = FALSE)
trainingLat <- sampleDataLat[inTraining,]
testingLat <- sampleDataLat[-inTraining,]

#---Cross validation----

#Setting a 4-fold cross validation to use on the model
Control <- trainControl(method = "repeatedcv", 
                        number = 4, 
                        repeats = 1)

#---Training model----

#Train a kNN model with tunelength = 5
mod_knnLat1 <- train(LATITUDE~., data = trainingLat %>% 
                       select(LATITUDE, starts_with("WAP")),
                  method = "knn",
                  trControl = Control, 
                  tuneLength = 5)



#---Results 1st kNN model----

#Check results on the training set
train_resultsLat_1st_knn <- predict(object = mod_knnLat1, newdata = trainingLat)
postResample(train_resultsLat_1st_knn, trainingLat$LATITUDE)
# RMSE  7.860
# R2    98.60 %
# MAE   3.939


#Results on testing set
test_resultsLat_1st_knn <- predict(object = mod_knnLat1, newdata = testingLat)
postResample(test_resultsLat_1st_knn, testingLat$LATITUDE)
# RMSE  10.193
# R2    97.76 %
# MAE   5.043


# Results on validation set
validation_resultsLat_1st_knn <- predict(object = mod_knnLat1, 
                                         newdata = validation)
postResample(validation_resultsLat_1st_knn, validation$LATITUDE)
# RMSE  20.819
# R2    91.36 %
# MAE   10.799



#--- 2nd kNN Training model----

# Train a kNN model on pre-processed data, using trainingB5 dataset.
mod_knnLat2 <- train(LATITUDE~., data = trainingB5 %>% 
                       select(LATITUDE, starts_with("WAP")),
                  method = "knn",
                  trControl = Control, 
                  tuneLength = 5)


#---Results 2nd kNN model----

#Check results on the training set
train_resultsLat_2nd_knn <- predict(object = mod_knnLat2, newdata = trainingB5)
postResample(train_resultsLat_2nd_knn, trainingB5$LATITUDE)
# RMSE  3.752
# R2    99.69 %
# MAE   2.146


#Results on testing set
test_resultsLat_2nd_knn <- predict(object = mod_knnLat2, newdata = testingB5)
postResample(test_resultsLat_2nd_knn, testingB5$LATITUDE)
# RMSE  4.518
# R2    99.57 %
# MAE   2.749


# Results on validation set
validation_resultsLat_2nd_knn <- predict(object = mod_knnLat2, 
                                         newdata = validation_cleaned)
postResample(validation_resultsLat_2nd_knn, validation_cleaned$LATITUDE)
# RMSE  10.699
# R2    97.70 %
# MAE   5.822



#--- 3rd kNN Training model----

#Train a kNN model on the training set that has been scaled by row.
mod_knnLat3 <- train(LATITUDE~., data = trainingNorm %>% 
                       select(LATITUDE, starts_with("WAP")),
                     method = "knn",
                     trControl = Control, 
                     tuneLength = 5)


#---Results 3rd kNN model----

#Check results on the training set
train_resultsLat_3rd_knn <- predict(object = mod_knnLat3, newdata = trainingNorm)
postResample(train_resultsLat_3rd_knn, trainingNorm$LATITUDE)
# RMSE  3.747
# R2    99.69 %
# MAE   2.279


# Results on validation set
validation_resultsLat_3rd_knn <- predict(object = mod_knnLat3, 
                                         newdata = ValidationNorm)
postResample(validation_resultsLat_3rd_knn, ValidationNorm$LATITUDE)
# RMSE  7.850
# R2    98.76 %
# MAE   5.240



#--- 4th kNN Training model----

# Train a kNN model using the training set that has been scaled by row, and
# adding predictions for building to the data
mod_knnLat4 <- train(LATITUDE~., data = trainingNorm %>% 
                       select(LATITUDE, starts_with("WAP"), PredBuilding),
                     method = "knn",
                     trControl = Control, 
                     tuneLength = 5)


#---Results 4th kNN model----

#Check results on the training set
train_resultsLat_4th_knn <- predict(object = mod_knnLat4, newdata = trainingNorm)
postResample(train_resultsLat_4th_knn, trainingNorm$LATITUDE)
# RMSE  3.690
# R2    99.70 %
# MAE   2.270


# Results on validation set
validation_resultsLat_4th_knn <- predict(object = mod_knnLat4, 
                                         newdata = ValidationNorm)
postResample(validation_resultsLat_4th_knn, ValidationNorm$LATITUDE)
# RMSE  7.667
# R2    98.82 %
# MAE   5.179



#--- 5th kNN Training model----

# Train a kNN model using the training set that has been scaled by row, adding 
# predictions for building to the data and removing five WAPs
mod_knnLat5 <- train(LATITUDE~., data = trainingNorm %>% 
                       select(LATITUDE, starts_with("WAP"), PredBuilding,
                              -c(WAP084, WAP085, WAP184, WAP248, WAP286)),
                     method = "knn",
                     trControl = Control, 
                     tuneLength = 5)


#---Results 5th kNN model----

#Check results on the training set
train_resultsLat_5th_knn <- predict(object = mod_knnLat5, newdata = trainingNorm)
postResample(train_resultsLat_5th_knn, trainingNorm$LATITUDE)
# RMSE  3.726
# R2    99.69 %
# MAE   2.299


# Results on validation set
validation_resultsLat_5th_knn <- predict(object = mod_knnLat5, 
                                         newdata = ValidationNorm)
postResample(validation_resultsLat_5th_knn, ValidationNorm$LATITUDE)
# RMSE  7.646
# R2    98.83 %
# MAE   5.127



#--- 1st RF Training model----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a RF model using the training set that has been scaled by row, adding 
# predictions for building to the data and removing five WAPs
mod_rfLat1 <- train(LATITUDE~., data = trainingNorm %>%
                      select(LATITUDE, starts_with("WAP"), PredBuilding),
                    method = "rf",
                    trControl = Control,
                    allowParallel = TRUE,
                    tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#---Results 1st RF model----

#Check results on the training set
train_resultsLat_1st_rf <- predict(object = mod_rfLat1, newdata = trainingNorm)
postResample(train_resultsLat_1st_rf, trainingNorm$LATITUDE)
# RMSE  2.427
# R2    99.87 %
# MAE   1.458


# Results on validation set
validation_resultsLat_1st_rf <- predict(object = mod_rfLat1,
                                        newdata = ValidationNorm)
postResample(validation_resultsLat_1st_rf, ValidationNorm$LATITUDE)
# RMSE  8.989
# R2    98.51 %
# MAE   6.105



#--- 1st GBM Training model----

# Train a GBM model using the training set that has been scaled by row, adding 
# predictions for building to the data and removing five WAPs
mod_gbmLat1 <- train(LATITUDE~., data = trainingNorm %>% 
                       select(LATITUDE, starts_with("WAP"), PredBuilding,
                              -c(WAP084, WAP085, WAP184, WAP248, WAP286)),
                     method = "gbm",
                     trControl = Control, 
                     tuneLength = 5,
                     verbose = FALSE)


#---Results 1st GBM model----

#Check results on the training set
train_resultsLat_1st_gbm <- predict(object = mod_gbmLat1, newdata = trainingNorm)
postResample(train_resultsLat_1st_gbm, trainingNorm$LATITUDE)
# RMSE  6.609
# R2    99.02 %
# MAE   5.131


# Results on validation set
validation_resultsLat_1st_gbm <- predict(object = mod_gbmLat1, 
                                         newdata = ValidationNorm)
postResample(validation_resultsLat_1st_gbm, ValidationNorm$LATITUDE)
# RMSE  11.715
# R2    97.49 %
# MAE   8.637