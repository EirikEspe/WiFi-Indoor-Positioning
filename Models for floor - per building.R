################################################################
#IoT Analytics
#Evaluate Techniques for WiFi Locationing 

#Models for determining which floor, used on separate buildings

#Created by Eirik Espe
################################################################

source("Pre-processing.R")

library(doParallel)


#--- Training models for floors building 0 ----

# Extract dataset for building 0
Building0 <- v4data %>% filter(BUILDINGID == 0) %>% droplevels()


## Create a training and testing set

#Set seed
set.seed(123)

#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(Building0$FLOOR, 
                                  p = .80, 
                                  list = FALSE)
trainingF_B0 <- Building0[inTraining,]
testingF_B0 <- Building0[-inTraining,]


#---Cross validation----

#Setting a 4-fold cross validation to use on the model
Control3 <- trainControl(method = "repeatedcv", 
                        number = 4, 
                        repeats = 1,
                        allowParallel = TRUE)


#--- 1st Training model for building 0----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a Random Forest model for floors in building 0
mod_B0rf1 <- train(FLOOR~., data = trainingF_B0 %>% 
                    select(FLOOR, starts_with("WAP")),
                  method = "rf",
                  trControl = Control3,
                  tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster.   
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Save model
saveRDS(mod_B0rf1, file = "RFmodel1_B0_Floor.rds")


#Check results on the training set
train_resultsF_B0_1st_rf <- predict(object = mod_B0rf1, newdata = trainingF_B0)
postResample(train_resultsF_B0_1st_rf, trainingF_B0$FLOOR)
# Accuracy  1
# Kappa     1


# Results on validation set
validation_resultsF_B0_1st_rf <- predict(object = mod_B0rf1,
                                         newdata = VBuilding0)
postResample(validation_resultsF_B0_1st_rf, VBuilding0$FLOOR)
# Accuracy  96.45 %
# Kappa     0.9498




#--- 1st SVM Training model for building 0----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a SVM model for floors in building 0
mod_B0svm1 <- train(FLOOR~., data = trainingF_B0 %>% 
                     select(FLOOR, starts_with("WAP")),
                   method = "svmRadial",
                   trControl = Control3,
                   tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_B0_1st_svm <- predict(object = mod_B0svm1, newdata = trainingF_B0)
postResample(train_resultsF_B0_1st_svm, trainingF_B0$FLOOR)
# Accuracy  99.86 %
# Kappa     0.9981


# Results on validation set
validation_resultsF_B0_1st_svm <- predict(object = mod_B0svm1,
                                          newdata = VBuilding0)
postResample(validation_resultsF_B0_1st_svm, VBuilding0$FLOOR)
# Accuracy  97.20 %
# Kappa     0.9604



#--- 1st kNN Training model for building 0----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a kNN model for floors in building 0
mod_B0knn1 <- train(FLOOR~., data = trainingF_B0 %>% 
                     select(FLOOR, starts_with("WAP")),
                   method = "knn",
                   trControl = Control3,
                   tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_B0_1st_knn <- predict(object = mod_B0knn1, newdata = trainingF_B0)
postResample(train_resultsF_B0_1st_knn, trainingF_B0$FLOOR)
# Accuracy  99.67 %
# Kappa     0.9955


# Results on validation set
validation_resultsF_B0_1st_knn <- predict(object = mod_B0knn1,
                                          newdata = VBuilding0)
postResample(validation_resultsF_B0_1st_knn, VBuilding0$FLOOR)
# Accuracy  97.39 %
# Kappa     0.9631


 
# Normalize training set by rows
trainingF_B0sc <- cbind(t(apply(select(trainingF_B0, 
                                        starts_with('WAP')), 1, normalize)), 
                         trainingF_B0[,466:474])


#--- 2nd SVM Training model for building 0----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a SVM model on the scaled data
mod_B0svm2 <- train(FLOOR~., data = trainingF_B0sc %>% 
                      select(FLOOR, starts_with("WAP")),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster. 
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_B0_2nd_svm <- predict(object = mod_B0svm2, newdata = trainingF_B0sc)
postResample(train_resultsF_B0_2nd_svm, trainingF_B0sc$FLOOR)
# Accuracy  99.83 %
# Kappa     0.9978


# Results on validation set
validation_resultsF_B0_2nd_svm <- predict(object = mod_B0svm2,
                                          newdata = ValidationNormB0)
postResample(validation_resultsF_B0_2nd_svm, ValidationNormB0$FLOOR)
# Accuracy  97.76 %
# Kappa     0.9683



#--- 3rd SVM Training model for building 0----

# Train a SVM model on the scaled data and remove six WAPs
mod_B0svm3 <- train(FLOOR~., data = trainingF_B0sc %>% 
                      select(FLOOR, starts_with("WAP"), -c(WAP008, WAP046, 
                                                           WAP151, WAP184, 
                                                           WAP185, WAP248)),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)


#Save model
saveRDS(mod_B0svm3, file = "Floors_building0_best_model(mod_B0svm3).rds")


#Check results on the training set
train_resultsF_B0_3rd_svm <- predict(object = mod_B0svm3, newdata = trainingF_B0sc)
postResample(train_resultsF_B0_3rd_svm, trainingF_B0sc$FLOOR)
# Accuracy  99.81 %
# Kappa     0.9974


# Results on validation set
validation_resultsF_B0_3rd_svm <- predict(object = mod_B0svm3,
                                          newdata = ValidationNormB0)
postResample(validation_resultsF_B0_3rd_svm, ValidationNormB0$FLOOR)
# Accuracy  97.76 %
# Kappa     0.9683



#--- 1st GBM Training model for building 0----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a GBM model on the scaled data and remove six WAPs
mod_B0gbm1 <- train(FLOOR~., data = trainingF_B0sc %>% 
                      select(FLOOR, starts_with("WAP"), -c(WAP008, WAP046, 
                                                           WAP151, WAP184, 
                                                           WAP185, WAP248)
                      ),
                    method = "gbm",
                    trControl = Control3,
                    tuneLength = 5,
                    verbose = FALSE)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_B0_1st_gbm <- predict(object = mod_B0gbm1, newdata = trainingF_B0sc)
postResample(train_resultsF_B0_1st_gbm, trainingF_B0sc$FLOOR)
# Accuracy  1
# Kappa     1


# Results on validation set
validation_resultsF_B0_1st_gbm <- predict(object = mod_B0gbm1,
                                          newdata = ValidationNormB0)
postResample(validation_resultsF_B0_1st_gbm, ValidationNormB0$FLOOR)
# Accuracy  96.08 %
# Kappa     0.9445



#--- Training models for floors building 1 ----

# Extract dataset
Building1 <- v4data %>% filter(BUILDINGID == 1) %>% droplevels()

#Set seed
set.seed(123)

## Create a training and testing set

#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(Building1$FLOOR, 
                                  p = .80, 
                                  list = FALSE)
trainingF_B1 <- Building1[inTraining,]
testingF_B1 <- Building1[-inTraining,]


#--- 1st SVM Training model for building 1----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a SVM model for floors in building 1
mod_B1svm1 <- train(FLOOR~., data = trainingF_B1 %>% 
                      select(FLOOR, starts_with("WAP")),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_B1_1st_svm <- predict(object = mod_B1svm1, newdata = trainingF_B1)
postResample(train_resultsF_B1_1st_svm, trainingF_B1$FLOOR)
# Accuracy  99.97 %
# Kappa     0.9996


# Results on validation set
validation_resultsF_B1_1st_svm <- predict(object = mod_B1svm1,
                                          newdata = VBuilding1)
postResample(validation_resultsF_B1_1st_svm, VBuilding1$FLOOR)
# Accuracy  84.69 %
# Kappa     0.7787



#--- 1st kNN Training model for building 1----


# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a kNN model for floors in building 1
mod_B1knn1 <- train(FLOOR~., data = trainingF_B1 %>% 
                      select(FLOOR, starts_with("WAP")),
                    method = "knn",
                    trControl = Control3,
                    tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster. 
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_B1_1st_knn <- predict(object = mod_B1knn1, newdata = trainingF_B1)
postResample(train_resultsF_B1_1st_knn, trainingF_B1$FLOOR)
# Accuracy  99.85 %
# Kappa     0.9980


# Results on validation set
validation_resultsF_B1_1st_knn <- predict(object = mod_B1knn1,
                                          newdata = VBuilding1)
postResample(validation_resultsF_B1_1st_knn, VBuilding1$FLOOR)
# Accuracy  76.55 %
# Kappa     0.6633



#--- 2nd SVM Training model for building 1----

# Normalize training set by rows
trainingF_B1sc <- cbind(t(apply(select(trainingF_B1, 
                                       starts_with('WAP')), 1, normalize)), 
                        trainingF_B1[,466:474])

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a SVM model on the scaled data
mod_B1svm2 <- train(FLOOR~., data = trainingF_B1sc %>% 
                      select(FLOOR, starts_with("WAP")),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_B1_2nd_svm <- predict(object = mod_B1svm2, newdata = trainingF_B1sc)
postResample(train_resultsF_B1_2nd_svm, trainingF_B1sc$FLOOR)
# Accuracy  99.95 %
# Kappa     0.9993


# Results on validation set
validation_resultsF_B1_2nd_svm <- predict(object = mod_B1svm2,
                                          newdata = ValidationNormB1)
postResample(validation_resultsF_B1_2nd_svm, ValidationNormB1$FLOOR)
# Accuracy  85.34 %
# Kappa     0.7875



#--- 3rd SVM Training model for building 1----

# Filter out phone #13
trainingF_B1_exphone13 <- trainingF_B1 %>% filter(PHONEID != 13)

# Train a SVM model where user ID 13 is removed
mod_B1svm3 <- train(FLOOR~., data = trainingF_B1_exphone13 %>% 
                      select(FLOOR, starts_with("WAP")),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)


#Check results on the training set
train_resultsF_B1_3rd_svm <- predict(object = mod_B1svm3, newdata = trainingF_B1)
postResample(train_resultsF_B1_3rd_svm, trainingF_B1$FLOOR)
# Accuracy  99.90 %
# Kappa     0.9987


# Results on validation set
validation_resultsF_B1_3rd_svm <- predict(object = mod_B1svm3,
                                          newdata = VBuilding1)
postResample(validation_resultsF_B1_3rd_svm, VBuilding1$FLOOR)
# Accuracy  85.67 %
# Kappa     0.7916


#---- Error investigation ----
errorCheckF_B1 <- VBuilding1 %>% 
  filter(validation_resultsF_B1_2nd_svm != FLOOR) %>% 
  select_if(function(.) n_distinct(.) != 1)

# Summary of the prediction errors for floors in building 1
summary(errorCheckF_B1$FLOOR)
## Most of the prediction errors for building 1 happen in floor 1 (32 of 45 errors)

# Create a data frame for prediction errors on floor 1
investigateFloor1 <- errorCheckF_B1 %>% filter(FLOOR == 1)

## Add number of detected WAPs to data frame
investigateFloor1$DetectedWAPs <- apply(investigateFloor1[,1:109], 1,
                                        function(x) length(which(x != -105)))

# Convert timestamp fron UNIX time to date and time
investigateFloor1$TIMESTAMP <- as.POSIXct(investigateFloor1$TIMESTAMP, 
                                          origin = "1970-01-01", 
                                          tz = "UTC")

# Extract same columns for the training data
trainDataFloorsB1 <- v4data
B1WAPs <- names(errorCheckF_B1[, 1:109])
trainDataFloorsB1 <- select(trainDataFloorsB1, B1WAPs, LONGITUDE:TIMESTAMP)

# Convert time format                           
trainDataFloorsB1$TIMESTAMP <- as.POSIXct(trainDataFloorsB1$TIMESTAMP, 
                                          origin = "1970-01-01", 
                                          tz = "UTC")
# Create variable for numbers of WAPs detected
trainDataFloorsB1$DetectedWAPs <- apply(trainDataFloorsB1[,1:109], 1,
                                        function(x) length(which(x != -105)))

# Filter to zoom in on Building #1
trainDataFloorsB1 <- trainDataFloorsB1 %>% filter(BUILDINGID == 1)

# Plot signal strength distribution
trainDataFloorsB1 %>% 
  melt(id.vars = c(110:119),
       variable.name = "WAP",
       value.name = "Signal") %>%
  filter(Signal != -105) %>% 
  
  ggplot(aes(x = Signal)) + geom_density(aes(fill = FLOOR), alpha = 0.5) +
  guides(fill = guide_legend(title = "Floor")) + 
  labs(x = "Signal strength (dBm)")

# Summary of the training data for where the prediction errors happen
trainDataFloorsB1 %>% select(USERID:TIMESTAMP) %>% summary()



#--- 4th SVM Training model for building 1----

# Train a SVM model on the scaled data and remove two WAPs
mod_B1svm4 <- train(FLOOR~., data = trainingF_B1sc %>% 
                      select(FLOOR, starts_with("WAP"), -c(WAP035, WAP083)),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)


#Check results on the training set
train_resultsF_B1_4th_svm <- predict(object = mod_B1svm4, newdata = trainingF_B1sc)
postResample(train_resultsF_B1_4th_svm, trainingF_B1sc$FLOOR)
# Accuracy  99.95 %
# Kappa     0.9993


# Results on validation set
validation_resultsF_B1_4th_svm <- predict(object = mod_B1svm4,
                                          newdata = ValidationNormB1)
postResample(validation_resultsF_B1_4th_svm, ValidationNormB1$FLOOR)
# Accuracy  84.69 %
# Kappa     0.7787



#--- 2nd kNN Training model for building 1----

# Train a kNN model on the scaled data and remove two WAPs
mod_B1knn2 <- train(FLOOR~., data = trainingF_B1sc %>% 
                      select(FLOOR, starts_with("WAP"), -c(WAP035, WAP083)),
                    method = "knn",
                    trControl = Control3,
                    tuneLength = 5)


#Check results on the training set
train_resultsF_B1_2nd_knn <- predict(object = mod_B1knn2, newdata = trainingF_B1sc)
postResample(train_resultsF_B1_2nd_knn, trainingF_B1sc$FLOOR)
# Accuracy  99.81 %
# Kappa     0.9974


# Results on validation set
validation_resultsF_B1_2nd_knn <- predict(object = mod_B1knn2,
                                          newdata = ValidationNormB1)
postResample(validation_resultsF_B1_2nd_knn, ValidationNormB1$FLOOR)
# Accuracy  78.50 %
# Kappa     0.6910



#--- 5th SVM Training model for building 1----

# Train a SVM model on the scaled data and remove five WAPs
mod_B1svm5 <- train(FLOOR~., data = trainingF_B1sc %>% 
                      select(FLOOR, starts_with("WAP"), -c(WAP029, 
                                                           WAP035, WAP083,
                                                           WAP142, WAP256)),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)


#Check results on the training set
train_resultsF_B1_5th_svm <- predict(object = mod_B1svm5, newdata = trainingF_B1sc)
postResample(train_resultsF_B1_5th_svm, trainingF_B1sc$FLOOR)
# Accuracy  99.95 %
# Kappa     0.9993


# Results on validation set
validation_resultsF_B1_5th_svm <- predict(object = mod_B1svm5,
                                          newdata = ValidationNormB1)
postResample(validation_resultsF_B1_5th_svm, ValidationNormB1$FLOOR)
# Accuracy  86.97 %
# Kappa     0.8101



#--- 6th SVM Training model for building 1----

# Train a SVM model on the scaled data and remove user ID 17
mod_B1svm6 <- train(FLOOR~., data = trainingF_B1sc %>% filter(USERID != 17) %>% 
                      select(FLOOR, starts_with("WAP")),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)


#Check results on the training set
train_resultsF_B1_6th_svm <- predict(object = mod_B1svm6, newdata = trainingF_B1sc)
postResample(train_resultsF_B1_6th_svm, trainingF_B1sc$FLOOR)
# Accuracy  99.78 %
# Kappa     0.9971


# Results on validation set
validation_resultsF_B1_6th_svm <- predict(object = mod_B1svm6,
                                          newdata = ValidationNormB1)
postResample(validation_resultsF_B1_6th_svm, ValidationNormB1$FLOOR)
# Accuracy  86.64 %
# Kappa     0.8055



#--- 7th SVM Training model for building 1----

# Train a SVM model on the scaled data and three space IDs
mod_B1svm7 <- train(FLOOR~., data = trainingF_B1sc %>% 
                      filter(SPACEID != 014 & SPACEID != 022 & 
                               SPACEID != 214) %>% 
                      select(FLOOR, starts_with("WAP")),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)


#Check results on the training set
train_resultsF_B1_7th_svm <- predict(object = mod_B1svm7, newdata = trainingF_B1sc)
postResample(train_resultsF_B1_7th_svm, trainingF_B1sc$FLOOR)
# Accuracy  99.95 %
# Kappa     0.9993


# Results on validation set
validation_resultsF_B1_7th_svm <- predict(object = mod_B1svm7,
                                          newdata = ValidationNormB1)
postResample(validation_resultsF_B1_7th_svm, ValidationNormB1$FLOOR)
# Accuracy  85.99 %
# Kappa     0.7964



#--- 8th SVM Training model for building 1----

# Train a SVM model on the scaled data and look at captures taken outside of rooms
mod_B1svm8 <- train(FLOOR~., data = trainingF_B1sc %>% 
                      filter(RELATIVEPOSITION == 2) %>% 
                      select(FLOOR, starts_with("WAP")),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)


#Check results on the training set
train_resultsF_B1_8th_svm <- predict(object = mod_B1svm8, newdata = trainingF_B1sc)
postResample(train_resultsF_B1_8th_svm, trainingF_B1sc$FLOOR)
# Accuracy  99.46 %
# Kappa     0.9928


# Results on validation set
validation_resultsF_B1_8th_svm <- predict(object = mod_B1svm8,
                                          newdata = ValidationNormB1)
postResample(validation_resultsF_B1_8th_svm, ValidationNormB1$FLOOR)
# Accuracy  82.74 %
# Kappa     0.7524



#--- 9th SVM Training model for building 1----

# Train a SVM model on the scaled data and remove 12 WAPs
mod_B1svm9 <- train(FLOOR~., data = trainingF_B1sc %>% 
                      select(FLOOR, starts_with("WAP"), -c(WAP046, WAP151, 
                                                           WAP248, WAP082,
                                                           WAP083, WAP084, 
                                                           WAP085, WAP248, 
                                                           WAP286, WAP398, 
                                                           WAP478, WAP503)),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)


#Check results on the training set
train_resultsF_B1_9th_svm <- predict(object = mod_B1svm9, newdata = trainingF_B1sc)
postResample(train_resultsF_B1_9th_svm, trainingF_B1sc$FLOOR)
# Accuracy  99.93 %
# Kappa     0.9990


# Results on validation set
validation_resultsF_B1_9th_svm <- predict(object = mod_B1svm9,
                                          newdata = ValidationNormB1)
postResample(validation_resultsF_B1_9th_svm, ValidationNormB1$FLOOR)
# Accuracy  86.64 %
# Kappa     0.8043



#--- 10th SVM Training model for building 1----

# Train a SVM model on the scaled data and remove 16 WAPs
mod_B1svm10 <- train(FLOOR~., data = trainingF_B1sc %>% 
                      select(FLOOR, starts_with("WAP"), -c(WAP046, WAP151, 
                                                           WAP248, WAP082,
                                                           WAP083, WAP084, 
                                                           WAP085, WAP248, 
                                                           WAP286, WAP398, 
                                                           WAP478, WAP503,
                                                           
                                                           WAP029, WAP035, 
                                                           WAP142, WAP256)),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)


#Check results on the training set
train_resultsF_B1_10th_svm <- predict(object = mod_B1svm10, newdata = trainingF_B1sc)
postResample(train_resultsF_B1_10th_svm, trainingF_B1sc$FLOOR)
# Accuracy  99.88 %
# Kappa     0.9984


# Results on validation set
validation_resultsF_B1_10th_svm <- predict(object = mod_B1svm10,
                                           newdata = ValidationNormB1)
postResample(validation_resultsF_B1_10th_svm, ValidationNormB1$FLOOR)
# Accuracy  88.27 %
# Kappa     0.8270



#--- 11th SVM Training model for building 1----

# Train a SVM model on the scaled data, remove 16 WAPs and remove
# user ID 17
mod_B1svm11 <- train(FLOOR~., data = trainingF_B1sc %>% 
                       filter(USERID != 17) %>% 
                       select(FLOOR, starts_with("WAP"), -c(WAP046, WAP151, 
                                                            WAP248, WAP082,
                                                            WAP083, WAP084, 
                                                            WAP085, WAP248, 
                                                            WAP286, WAP398, 
                                                            WAP478, WAP503,
                                                            
                                                            WAP029, WAP035, 
                                                            WAP142, WAP256)),
                     method = "svmRadial",
                     trControl = Control3,
                     tuneLength = 5)


#Check results on the training set
train_resultsF_B1_11th_svm <- predict(object = mod_B1svm11, newdata = trainingF_B1sc)
postResample(train_resultsF_B1_11th_svm, trainingF_B1sc$FLOOR)
# Accuracy  99.68 %
# Kappa     0.9958


# Results on validation set
validation_resultsF_B1_11th_svm <- predict(object = mod_B1svm11,
                                           newdata = ValidationNormB1)
postResample(validation_resultsF_B1_11th_svm, ValidationNormB1$FLOOR)
# Accuracy  89.25 %
# Kappa     0.8409




#--- 1st GBM Training model for building 1----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a GBM on the input for the best SVM model (mod_B1svm11)
mod_B1gbm1 <- train(FLOOR~., data = trainingF_B1sc %>% 
                      filter(USERID != 17) %>% 
                      select(FLOOR, starts_with("WAP"), -c(WAP046, WAP151, 
                                                           WAP248, WAP082,
                                                           WAP083, WAP084, 
                                                           WAP085, WAP248, 
                                                           WAP286, WAP398, 
                                                           WAP478, WAP503,
                                                           
                                                           WAP029, WAP035, 
                                                           WAP142, WAP256)
                             ),
                    method = "gbm",
                    trControl = Control3,
                    tuneLength = 5,
                    verbose = FALSE)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Save model
saveRDS(mod_B1gbm1, file = "Floors_building1_best_model(mod_B1gbm1).rds")


#Check results on the training set
train_resultsF_B1_1st_gbm <- predict(object = mod_B1gbm1, newdata = trainingF_B1sc)
postResample(train_resultsF_B1_1st_gbm, trainingF_B1sc$FLOOR)
# Accuracy  99.61 %
# Kappa     0.9948


# Results on validation set
validation_resultsF_B1_1st_gbm <- predict(object = mod_B1gbm1,
                                          newdata = ValidationNormB1)
postResample(validation_resultsF_B1_1st_gbm, ValidationNormB1$FLOOR)
# Accuracy  93.16 %
# Kappa     0.8983




#--- Training models for building 2----

# Extract dataset
Building2 <- v4data %>% filter(BUILDINGID == 2) %>% droplevels()

#Set seed
set.seed(123)

## Create a training and testing set

#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(Building2$FLOOR, 
                                  p = .80, 
                                  list = FALSE)
trainingF_B2 <- Building2[inTraining,]
testingF_B2 <- Building2[-inTraining,]


#--- 1st SVM Training model for building 2----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a SVM model for floors in building 2
mod_B2svm1 <- train(FLOOR~., data = trainingF_B2 %>% 
                      select(FLOOR, starts_with("WAP")),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster. 
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_B2_1st_svm <- predict(object = mod_B2svm1, newdata = trainingF_B2)
postResample(train_resultsF_B2_1st_svm, trainingF_B2$FLOOR)
# Accuracy  99.98 %
# Kappa     0.9998


# Results on validation set
validation_resultsF_B2_1st_svm <- predict(object = mod_B2svm1,
                                          newdata = VBuilding2)
postResample(validation_resultsF_B2_1st_svm, VBuilding2$FLOOR)
# Accuracy  94.40 %
# Kappa     0.9238



#--- 1st kNN Training model for building 2----


# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a kNN model for floors in building 2
mod_B2knn1 <- train(FLOOR~., data = trainingF_B2 %>% 
                      select(FLOOR, starts_with("WAP")),
                    method = "knn",
                    trControl = Control3,
                    tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster. 
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_B2_1st_knn <- predict(object = mod_B2knn1, newdata = trainingF_B2)
postResample(train_resultsF_B2_1st_knn, trainingF_B2$FLOOR)
# Accuracy  99.89 %
# Kappa     0.9986


# Results on validation set
validation_resultsF_B2_1st_knn <- predict(object = mod_B2knn1,
                                          newdata = VBuilding2)
postResample(validation_resultsF_B2_1st_knn, VBuilding2$FLOOR)
# Accuracy  92.54 %
# Kappa     0.8986



#--- 2nd SVM Training model for building 2----

# Scale training data
trainingF_B2sc <- cbind(t(apply(select(trainingF_B2, 
                                       starts_with('WAP')), 1, normalize)), 
                        trainingF_B2[,466:474])

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a SVM model on the scaled data
mod_B2svm2 <- train(FLOOR~., data = trainingF_B2sc %>% 
                      select(FLOOR, starts_with("WAP")),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_B2_2nd_svm <- predict(object = mod_B2svm2, newdata = trainingF_B2sc)
postResample(train_resultsF_B2_2nd_svm, trainingF_B2sc$FLOOR)
# Accuracy  1
# Kappa     1


# Results on validation set
validation_resultsF_B2_2nd_svm <- predict(object = mod_B2svm2,
                                          newdata = ValidationNormB2)
postResample(validation_resultsF_B2_2nd_svm, ValidationNormB2$FLOOR)
# Accuracy  94.03 %
# Kappa     0.9188



#--- 3rd SVM Training model for building 2----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a SVM model on the scaled data and remove nine WAPs
mod_B2svm3 <- train(FLOOR~., data = trainingF_B2sc %>% 
                      select(FLOOR, starts_with("WAP"), -c(WAP113, WAP114,
                                                           WAP175, WAP180,
                                                           WAP181, WAP186,
                                                           WAP187, WAP248,
                                                           WAP286)
                    ),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_B2_3rd_svm <- predict(object = mod_B2svm3, newdata = trainingF_B2sc)
postResample(train_resultsF_B2_3rd_svm, trainingF_B2sc$FLOOR)
# Accuracy  1
# Kappa     1


# Results on validation set
validation_resultsF_B2_3rd_svm <- predict(object = mod_B2svm3,
                                          newdata = ValidationNormB2)
postResample(validation_resultsF_B2_3rd_svm, ValidationNormB2$FLOOR)
# Accuracy  94.40 %
# Kappa     0.9238



#--- 1st GBM Training model for building 2----

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2

# Train a GBM model on the scaled data and remove nine WAPs
mod_B2gbm1 <- train(FLOOR~., data = trainingF_B2sc %>% 
                      select(FLOOR, starts_with("WAP"), -c(WAP113, WAP114,
                                                           WAP175, WAP180,
                                                           WAP181, WAP186,
                                                           WAP187, WAP248,
                                                           WAP286)
                      ),
                    method = "gbm",
                    trControl = Control3,
                    tuneLength = 5,
                    verbose = FALSE)

# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Save model
saveRDS(mod_B2gbm1, file = "Floors_building2_best_model(mod_B2gbm1).rds")


#Check results on the training set
train_resultsF_B2_1st_gbm <- predict(object = mod_B2gbm1, newdata = trainingF_B2sc)
postResample(train_resultsF_B2_1st_gbm, trainingF_B2sc$FLOOR)
# Accuracy  1
# Kappa     1


# Results on validation set
validation_resultsF_B2_1st_gbm <- predict(object = mod_B2gbm1,
                                          newdata = ValidationNormB2)
postResample(validation_resultsF_B2_1st_gbm, ValidationNormB2$FLOOR)
# Accuracy  96.64 %
# Kappa     0.9543


