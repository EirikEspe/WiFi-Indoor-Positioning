################################################################
#IoT Analytics
#Evaluate Techniques for WiFi Locationing 

#Model for determining longitude

#Created by Eirik Espe
################################################################

source("Model for building.R")

#--- Creating the first model ----

#Set seed
set.seed(123)

# Create a sample for the dataset
sampleLon <- createDataPartition(data$LONGITUDE,
                               p = .30,
                               list = FALSE)
sampleDataLon <- data[sampleLon,]

#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(sampleDataLon$LONGITUDE, 
                                  p = .80, 
                                  list = FALSE)
trainingLon <- sampleDataLon[inTraining,]
testingLon <- sampleDataLon[-inTraining,]


#---Cross validation----

#Setting a 4-fold cross validation to use on the model
Control <- trainControl(method = "repeatedcv", 
                        number = 4, 
                        repeats = 1)

#---Training model----

#Train a kNN model with tunelength = 5
mod_knnLon1 <- train(LONGITUDE~., data = trainingLon %>%
                       select(LONGITUDE, starts_with("WAP")), 
                  method = "knn",
                  trControl = Control, 
                  tuneLength = 5)


#---Results 1st kNN model----

# Check results on the training set
train_resultsLon_1st_knn <- predict(object = mod_knnLon1, newdata = trainingLon)
postResample(train_resultsLon_1st_knn, trainingLon$LONGITUDE)
# RMSE  11.220
# R2    99.18 %
# MAE   4.968


# Results on testing set
test_resultsLon_1st_knn <- predict(object = mod_knnLon1, newdata = testingLon)
postResample(test_resultsLon_1st_knn, testingLon$LONGITUDE)
# RMSE  15.598
# R2    98.41 %
# MAE   6.805


# Results on validation set
validation_resultsLon_1st_knn <- predict(object = mod_knnLon1, 
                                         newdata = validation)
postResample(validation_resultsLon_1st_knn, validation$LONGITUDE)
# RMSE  28.929
# R2    94.30 %
# MAE   13.561


#--- 2nd kNN Training model----

# Train a kNN model on pre-processed data, where the records and WAPs with no
# detected signal have been removed . In addition, outliers have been removed.
mod_knnLon2 <- train(LONGITUDE~., data = trainingB5 %>%
                       select(LONGITUDE, starts_with("WAP")),
                     method = "knn",
                     trControl = Control, 
                     tuneLength = 5)


#---Results 2nd kNN model----

# Check results on the training set
train_resultsLon_2nd_knn <- predict(object = mod_knnLon2, newdata = trainingB5)
postResample(train_resultsLon_2nd_knn, trainingB5$LONGITUDE)
# RMSE  4.905
# R2    99.84 %
# MAE   2.505


# Results on testing set
test_resultsLon_2nd_knn <- predict(object = mod_knnLon2, newdata = testingB5)
postResample(test_resultsLon_2nd_knn, testingB5$LONGITUDE)
# RMSE  5.864
# R2    99.78 %
# MAE   3.267


# Results on validation set
validation_resultsLon_2nd_knn <- predict(object = mod_knnLon2, 
                                         newdata = validation_cleaned)
postResample(validation_resultsLon_2nd_knn, validation_cleaned$LONGITUDE)
# RMSE  13.268
# R2    98.78 %
# MAE   6.435



#--- 1st SVM Training model----

# Train a SVM model on pre-processed data
mod_svmLon1 <- train(LONGITUDE~., data = trainingB5 %>%
                       select(LONGITUDE, starts_with("WAP")),
                     method = "svmRadial",
                     trControl = Control, 
                     tuneLength = 5)


#---Results 1st SVM model----

# Check results on the training set
train_resultsLon_1st_svm <- predict(object = mod_svmLon1, newdata = trainingB5)
postResample(train_resultsLon_1st_svm, trainingB5$LONGITUDE)
# RMSE  19.847
# R2    97.63 %
# MAE   12.474


# Results on testing set
test_resultsLon_1st_svm <- predict(object = mod_svmLon1, newdata = testingB5)
postResample(test_resultsLon_1st_svm, testingB5$LONGITUDE)
# RMSE  17.748
# R2    98.15 %
# MAE   12.077


# Results on validation set
validation_resultsLon_1st_svm <- predict(object = mod_svmLon1, 
                                         newdata = validation_cleaned)
postResample(validation_resultsLon_1st_svm, validation_cleaned$LONGITUDE)
# RMSE  27.349
# R2    95.84 %
# MAE   19.364



#--- 3rd kNN Training model----

# Training model using kNN and the training set that has been scaled by row.
mod_knnLon3 <- train(LONGITUDE~., data = trainingNorm %>%
                       select(LONGITUDE, starts_with("WAP")),
                     method = "knn",
                     trControl = Control, 
                     tuneLength = 5)


#---Results 3rd kNN model----

# Check results on the training set
train_resultsLon_3rd_knn <- predict(object = mod_knnLon3, newdata = trainingNorm)
postResample(train_resultsLon_3rd_knn, trainingNorm$LONGITUDE)
# RMSE  4.708
# R2    99.86 %
# MAE   2.624


# Results on testing set
test_resultsLon_3rd_knn <- predict(object = mod_knnLon3, newdata = testingNorm)
postResample(test_resultsLon_3rd_knn, testingNorm$LONGITUDE)
# RMSE  5.788
# R2    99.78 %
# MAE   3.453


# Results on validation set
validation_resultsLon_3rd_knn <- predict(object = mod_knnLon3, 
                                         newdata = ValidationNorm)
postResample(validation_resultsLon_3rd_knn, ValidationNorm$LONGITUDE)
# RMSE  9.116
# R2    99.43 %
# MAE   5.617



#--- 4th kNN Training model----

# Adding predictions for building to the training set that is scaled by row
trainingNorm$PredBuilding <- predict(object = mod_svmB8, newdata = trainingNorm)

# Checking if the predictions are equal to the actual values
identical(trainingNorm$PredBuilding, trainingNorm$BUILDINGID)
# Yes, they are identical

# Training model using kNN and the training set that has been scaled by row, and
# adding predictions for building to the data.
mod_knnLon4 <- train(LONGITUDE~., data = trainingNorm %>%
                       select(LONGITUDE, starts_with("WAP"), PredBuilding),
                     method = "knn",
                     trControl = Control, 
                     tuneLength = 5)


# Check results on the training set
train_resultsLon_4th_knn <- predict(object = mod_knnLon4, newdata = trainingNorm)
postResample(train_resultsLon_4th_knn, trainingNorm$LONGITUDE)
# RMSE  4.340
# R2    99.88 %
# MAE   2.602


# Results on testing set

## Adding predictions for building to the testing set
testingNorm$PredBuilding <- predict(object = mod_svmB8, newdata = testingNorm)

identical(testingNorm$PredBuilding, testingNorm$BUILDINGID)
## Yes the predictions are identical to the actual values

## Check the predictions for longitude against actual values
test_resultsLon_4th_knn <- predict(object = mod_knnLon4, newdata = testingNorm)
postResample(test_resultsLon_4th_knn, testingNorm$LONGITUDE)
# RMSE  5.261
# R2    99.82 %
# MAE   3.388


# Results on validation set

## Adding predictions for building to the tvalidation set
ValidationNorm$PredBuilding <- predict(object = mod_svmB8, newdata = ValidationNorm)

## Checking if the predictions are equal to the actual values
identical(ValidationNorm$PredBuilding, ValidationNorm$BUILDINGID)
## Yes, they are identical


## Check the predictions for longitude against actual values
validation_resultsLon_4th_knn <- predict(object = mod_knnLon4, 
                                         newdata = ValidationNorm)
postResample(validation_resultsLon_4th_knn, ValidationNorm$LONGITUDE)
# RMSE  8.033
# R2    99.56 %
# MAE   5.467



#--- 5th kNN Training model----

# Training model using kNN on training set that has been scaled by row, adding 
# predictions for building to the data and removing five WAPs
mod_knnLon5 <- train(LONGITUDE~., data = trainingNorm %>%
                       select(LONGITUDE, starts_with("WAP"), PredBuilding,
                              -c(WAP084, WAP085, WAP184, WAP248, WAP286)),
                     method = "knn",
                     trControl = Control, 
                     tuneLength = 5)


#---Results 5th kNN model----

# Check results on the training set
train_resultsLon_5th_knn <- predict(object = mod_knnLon5, newdata = trainingNorm)
postResample(train_resultsLon_5th_knn, trainingNorm$LONGITUDE)
# RMSE  4.425
# R2    99.87 %
# MAE   2.645


# Results on testing set
test_resultsLon_5th_knn <- predict(object = mod_knnLon5, newdata = testingNorm)
postResample(test_resultsLon_5th_knn, testingNorm$LONGITUDE)
# RMSE  5.395
# R2    99.81 %
# MAE   3.439


# Results on validation set
validation_resultsLon_5th_knn <- predict(object = mod_knnLon5, 
                                         newdata = ValidationNorm)
postResample(validation_resultsLon_5th_knn, ValidationNorm$LONGITUDE)
# RMSE  8.178
# R2    99.54 %
# MAE   5.508



#--- 1st RF Training model----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Training model using Random Forest and the training set that has been scaled by row.
mod_rfLon1 <- train(LONGITUDE~., data = trainingNorm %>%
                       select(LONGITUDE, starts_with("WAP"), PredBuilding),
                     method = "rf",
                     trControl = Control,
                     allowParallel = TRUE,
                     tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#---Results 1st RF model----

# Check results on the training set
train_resultsLon_1st_rf <- predict(object = mod_rfLon1, newdata = trainingNorm)
postResample(train_resultsLon_1st_rf, trainingNorm$LONGITUDE)
# RMSE  2.983
# R2    99.94 %
# MAE   1.726


# Results on testing set
test_resultsLon_1st_rf <- predict(object = mod_rfLon1, newdata = testingNorm)
postResample(test_resultsLon_1st_rf, testingNorm$LONGITUDE)
# RMSE  6.166
# R2    99.75 %
# MAE   3.946


# Results on validation set
validation_resultsLon_1st_rf <- predict(object = mod_rfLon1, 
                                        newdata = ValidationNorm)
postResample(validation_resultsLon_1st_rf, ValidationNorm$LONGITUDE)
# RMSE  9.380
# R2    99.41 %
# MAE   6.753



#--- 1st GBM Training model----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a GBM model on the training set that has been scaled by row, adding
# predictions for building and removing five WAPs.
mod_gbmLon1 <- train(LONGITUDE~., data = trainingNorm %>%
                       select(LONGITUDE, starts_with("WAP"), PredBuilding,
                              -c(WAP084, WAP085, WAP184, WAP248, WAP286)),
                    method = "gbm",
                    trControl = Control,
#                   allowParallel = TRUE,
                    tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#---Results 1st GBM model----

# Check results on the training set
train_resultsLon_1st_gbm <- predict(object = mod_gbmLon1, newdata = trainingNorm)
postResample(train_resultsLon_1st_gbm, trainingNorm$LONGITUDE)
# RMSE  7.962
# R2    99.59 %
# MAE   6.075


# Results on testing set
test_resultsLon_1st_gbm <- predict(object = mod_gbmLon1, newdata = testingNorm)
postResample(test_resultsLon_1st_gbm, testingNorm$LONGITUDE)
# RMSE  9.332
# R2    99.43 %
# MAE   7.000


# Results on validation set
validation_resultsLon_1st_gbm <- predict(object = mod_gbmLon1, 
                                         newdata = ValidationNorm)
postResample(validation_resultsLon_1st_gbm, ValidationNorm$LONGITUDE)
# RMSE  12.349
# R2    98.96 %
# MAE   9.331



#--- 2nd SVM Training model----

# Using linear svm

# Train a SVM model for predictions of longitude
mod_svmLon2 <- train(LONGITUDE~., data = trainingB5 %>%
                       select(LONGITUDE, starts_with("WAP")),
                     method = "svmLinear",
                     trControl = Control, 
                     tuneLength = 5)        #RMSE 30.588     Rsquared = 93.99 %



#--- 2nd RF Training model----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a Random Forest model on the training set that has been scaled by row, 
# adding predictions for building and removing five WAPs.
mod_rfLon2 <- train(LONGITUDE~., data = trainingNorm %>%
                       select(LONGITUDE, starts_with("WAP"), PredBuilding,
                              -c(WAP084, WAP085, WAP184, WAP248, WAP286)),
                     method = "rf",
                     trControl = Control,
                     allowParallel = TRUE,
                     tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#---Results 2nd RF model----

# Check results on the training set
train_resultsLon_2nd_rf <- predict(object = mod_rfLon2, newdata = trainingNorm)
postResample(train_resultsLon_2nd_rf, trainingNorm$LONGITUDE)
# RMSE  2.985
# R2    99.94 %
# MAE   1.731


# Results on testing set
test_resultsLon_2nd_rf <- predict(object = mod_rfLon2, newdata = testingNorm)
postResample(test_resultsLon_2nd_rf, testingNorm$LONGITUDE)
# RMSE  6.144
# R2    99.75 %
# MAE   3.956


# Results on validation set
validation_resultsLon_2nd_rf <- predict(object = mod_rfLon2, 
                                        newdata = ValidationNorm)
postResample(validation_resultsLon_2nd_rf, ValidationNorm$LONGITUDE)
# RMSE  9.456
# R2    99.40 %
# MAE   6.692

