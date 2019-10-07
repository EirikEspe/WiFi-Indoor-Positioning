################################################################
#IoT Analytics
#Evaluate Techniques for WiFi Locationing 

#Models for determining which floor

#Created by Eirik Espe
################################################################

source("Model for building.R")



#--- Creating the first model ----

#Set seed
set.seed(123)

# Create a sample for the dataset
sampleF <- createDataPartition(data$FLOOR,
                               p = .30,
                               list = FALSE)
sampleDataF <- data[sampleF,]


#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(sampleDataF$FLOOR, 
                                  p = .80, 
                                  list = FALSE)
trainingF <- sampleDataF[inTraining,]
testingF <- sampleDataF[-inTraining,]



#---Cross validation----

#Setting a 4-fold cross validation to use on the model
Control <- trainControl(method = "repeatedcv", 
                        number = 4, 
                        repeats = 1)



#---Training model----

#Train a kNN model with tunelength = 5
mod_knnF1 <- train(FLOOR~., data = trainingF %>% 
                     select(FLOOR, starts_with("WAP")),
                  method = "knn",
                  trControl = Control, 
                  tuneLength = 5)


#---Results 1st kNN model----

#Check results on the training set
train_resultsF_1st_knn <- predict(object = mod_knnF1, newdata = trainingF)
postResample(train_resultsF_1st_knn, trainingF$FLOOR)
# Accuracy  95.07 %
# Kappa     0.9362        


#Results on testing set
test_resultsF_1st_knn <- predict(object = mod_knnF1, newdata = testingF)
postResample(test_resultsF_1st_knn, testingF$FLOOR)
# Accuracy  92.47 %
# Kappa     0.9025


# Add test predictions into the testing dataset for the floor
testingF$pred1Floor <- test_resultsF_1st_knn


# How many wrong predictions?
all.equal(testingF$FLOOR, testingF$pred1Floor)
# 90 mismatches


# Check where the model got wrong floor
testingF[testingF$FLOOR != testingF$pred1Floor, c(521:526, 529:530)]



# Results on validation set
validation_resultsF_1st_knn <- predict(object = mod_knnF1, 
                                       newdata = validation)
postResample(validation_resultsF_1st_knn, validation$FLOOR)
# Accuracy  69.31 %
# Kappa     0.5919




#---2nd kNN training model----

# Using same training set as after 1st pre-processing for building.

# WAPs (columns) with no detected signal and records (rows) without 
# detected signal have been removed.

# Undetected signals was converted from 100 to -105


# Train 2nd kNN model with tunelength = 5
mod_knnF2 <- train(FLOOR~., data = trainingB2 %>% 
                     select(FLOOR, starts_with("WAP")), 
                   method = "knn",
                   trControl = Control, 
                   tuneLength = 5)


#Check results on the training set
train_resultsF_2nd_knn <- predict(object = mod_knnF2, newdata = trainingB2)
postResample(train_resultsF_2nd_knn, trainingB2$FLOOR)
# Accuracy  99.47 %
# Kappa     0.9932


#Results on testing set
test_resultsF_2nd_knn <- predict(object = mod_knnF2, newdata = testingB2)
postResample(test_resultsF_2nd_knn, testingB2$FLOOR)
# Accuracy  98.82 %
# Kappa     0.9848


# Results on validation set
validation_resultsF_2nd_knn <- predict(object = mod_knnF2, 
                                       newdata = validation_cleaned)
postResample(validation_resultsF_2nd_knn, validation_cleaned$FLOOR)
# Accuracy  89.11 %
# Kappa     0.8487



#---3rd kNN training model----

# Using trainingB5 with 1st pre-processing, and outliers have been removed.


# Train 3rd kNN model
mod_knnF3 <- train(FLOOR~., data = trainingB5 %>% 
                     select(FLOOR, starts_with("WAP")),
                   method = "knn",
                   trControl = Control, 
                   tuneLength = 5)


# Check results on the training set
train_resultsF_3rd_knn <- predict(object = mod_knnF3, newdata = trainingB5)
postResample(train_resultsF_3rd_knn, trainingB5$FLOOR)
# Accuracy  99.46 %
# Kappa     0.9930


# Results on validation set
validation_resultsF_3rd_knn <- predict(object = mod_knnF3, 
                                       newdata = validation_cleaned)
postResample(validation_resultsF_3rd_knn, validation_cleaned$FLOOR)
# Accuracy  88.75 %
# Kappa     0.8437




#---4th kNN training model----

# Training model using kNN and the training set that has been scaled by row.
mod_knnF4 <- train(FLOOR~., data = trainingNorm %>% 
                     select(FLOOR, starts_with("WAP")),
                   method = "knn",
                   trControl = Control, 
                   tuneLength = 5)


# Check results on the training set
train_resultsF_4th_knn <- predict(object = mod_knnF4, newdata = trainingNorm)
postResample(train_resultsF_4th_knn, trainingNorm$FLOOR)
# Accuracy  99.55 %
# Kappa     0.9941


# Results on validation set
validation_resultsF_4th_knn <- predict(object = mod_knnF4, 
                                       newdata = ValidationNorm)
postResample(validation_resultsF_4th_knn, ValidationNorm$FLOOR)
# Accuracy  90.91 %
# Kappa     0.8733



#---1st SVM training model----

# Using same training set as after 1st pre-processing for building.

# WAPs (columns) with no detected signal and records (rows) without 
# detected signal have been removed.

# Undetected signals was converted from 100 to -105

# Train 1st SVM model for floors
mod_svmF1 <- train(FLOOR~., data = trainingB2 %>% 
                     select(FLOOR, starts_with("WAP")), 
                   method = "svmRadial",
                   trControl = Control, 
                   tuneLength = 5)


# Check results on the training set
train_resultsF_1st_svm <- predict(object = mod_svmF1, newdata = trainingB2)
postResample(train_resultsF_1st_svm, trainingB2$FLOOR)
# Accuracy  99.94 %
# Kappa     0.9992


# Results on validation set
validation_resultsF_1st_svm <- predict(object = mod_svmF1, 
                                       newdata = validation_cleaned)
postResample(validation_resultsF_1st_svm, validation_cleaned$FLOOR)
# Accuracy  91.27 %
# Kappa     0.8786


# Add test predictions into the testing dataset for the floor
validation_cleaned$pred_svm1Floor <- validation_resultsF_1st_svm

# Check where the model got wrong floor
validation_cleaned[validation_cleaned$FLOOR != validation_cleaned$pred_svm1Floor, 
                   c(466:469, 473:475)]
# 97 wrong classifications




#---1st RF training model----

# Setting up a parallel computing for faster computation of training model
library(doParallel)

# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2


# Random Forest model using training set after 1st pre-process (1h 30 min runtime)
mod_rfF1 <- train(FLOOR~., data = trainingB2 %>% 
                    select(FLOOR, starts_with("WAP")),  
                  method = "rf",
                  trControl = Control,
                  tuneLength = 5)


# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Save model
saveRDS(mod_rfF1, file = "RFmodel1_Floor.rds")


#Check results on the training set
train_resultsF_1st_rf <- predict(object = mod_rfF1, newdata = trainingB2)
postResample(train_resultsF_1st_rf, trainingB2$FLOOR)
# Accuracy  1
# Kappa     1


#Results on testing set
test_resultsF_1st_rf <- predict(object = mod_rfF1, newdata = testingB2)
postResample(test_resultsF_1st_rf, testingB2$FLOOR)
# Accuracy  99.41 %
# Kapppa    0.9924


# Results on validation set
validation_resultsF_1st_rf <- predict(object = mod_rfF1, 
                                      newdata = validation_cleaned)
postResample(validation_resultsF_1st_rf, validation_cleaned$FLOOR)
# Accuracy  89.83 %
# Kappa     0.8579


# Add test predictions into the testing dataset for the floor
validation_cleaned$predRF1Floor <- validation_resultsF_1st_rf

# Check where the model got wrong floor
validation_cleaned[validation_cleaned$FLOOR != validation_cleaned$predRF1Floor, 
                   c(466:469, 473:476)]
# 113 wrong classifications for floors



#---2nd SVM training model----

# Training set: Adjusted outliers to no detected signals (from >-30 dBm to -105 dBm)

# Model after adjusting outliers
mod_svmF2 <- train(FLOOR~., data = trainingB4 %>% 
                     select(FLOOR, starts_with("WAP")),
                   method = "svmRadial",
                   trControl = Control,
                   tuneLength = 5)


#Check results on the training set
train_resultsF_2nd_svm <- predict(object = mod_svmF2, newdata = trainingB4)
postResample(train_resultsF_2nd_svm, trainingB4$FLOOR)
# Accuracy  99.91%
# Kappa     0.9989


#Results on testing set
test_resultsF_2nd_svm <- predict(object = mod_svmF2, newdata = testingB4)
postResample(test_resultsF_2nd_svm, testingB4$FLOOR)
# Accuracy  99.75 %
# Kappa     0.9967


# Results on validation set
validation_resultsF_2nd_svm <- predict(object = mod_svmF2, 
                                       newdata = validation_cleaned)
postResample(validation_resultsF_2nd_svm, validation_cleaned$FLOOR)
# Accuracy  91.63 %
# Kappa     0.8835


# Add test predictions into the testing dataset for the floor
validation_cleaned$pred_svm2Floor <- validation_resultsF_2nd_svm

# Check where the model got wrong floor
validation_cleaned[validation_cleaned$FLOOR != validation_cleaned$pred_svm2Floor, 
                   c(467:469, 473:477)]
# 93 wrong classifications


# Confusion matrix
confusionMatrix(validation_cleaned$pred_svm2Floor, validation_cleaned$FLOOR)



#--- 3rd SVM training model----

# Model after removing rows with outliers
mod_svmF3 <- train(FLOOR~., data = trainingB5 %>% 
                     select(FLOOR, starts_with("WAP")),
                   method = "svmRadial",
                   trControl = Control,
                   tuneLength = 5)


#Check results on the training set
train_resultsF_3rd_svm <- predict(object = mod_svmF3, newdata = trainingB5)
postResample(train_resultsF_3rd_svm, trainingB5$FLOOR)
# Accuracy  99.96 %
# Kappa     0.9994


#Results on testing set
test_resultsF_3rd_svm <- predict(object = mod_svmF3, newdata = testingB5)
postResample(test_resultsF_3rd_svm, testingB5$FLOOR)
# Accuracy  99.57 %
# Kappa     0.9944


# Results on validation set
validation_resultsF_3rd_svm <- predict(object = mod_svmF3, 
                                       newdata = validation_cleaned)
postResample(validation_resultsF_3rd_svm, validation_cleaned$FLOOR)
# Accuracy  91.90 %
# Kappa     0.8875


# Add test predictions into the testing dataset for the floor
validation_cleaned$pred_svm3Floor <- validation_resultsF_3rd_svm

# Check where the model got wrong floor
validation_cleaned[validation_cleaned$FLOOR != validation_cleaned$pred_svm3Floor, 
                   c(467:469, 473:474, 477:478)]
# 90 wrong classifications





#--- 2nd RF training model----  


# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2



# Random Forest model after removing rows with outliers
                                                # Runtime 2 hours
mod_rfF2 <- train(FLOOR~., data = trainingB5 %>% 
                    select(FLOOR, starts_with("WAP")),
                  method = "rf",
                  trControl = Control,
                  tuneLength = 5)


# Stop Cluster. After performing tasks, stopping the cluster.  
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_2nd_rf <- predict(object = mod_rfF2, newdata = trainingB5)
postResample(train_resultsF_2nd_rf, trainingB5$FLOOR)
# Accuracy  1
# Kappa     1


# Results on validation set
validation_resultsF_2nd_rf <- predict(object = mod_rfF2, 
                                      newdata = validation_cleaned)
postResample(validation_resultsF_2nd_rf, validation_cleaned$FLOOR)
# Accuracy  88.57 %
# Kappa     0.8404





#--- 4th SVM training model----

# Adding number of WAPs detected to the dataset where outliers are removed
trainingB5$DetectedWAPs <- apply(trainingB5[,1:465], 1,
                                function(x) length(which(x != -105)))

# Train the 4th SVM model
mod_svmF4 <- train(FLOOR~., data = trainingB5 %>% 
                     select(FLOOR, starts_with("WAP"), DetectedWAPs),
                   method = "svmRadial",
                   trControl = Control,
                   tuneLength = 5)


#Check results on the training set
train_resultsF_4th_svm <- predict(object = mod_svmF4, newdata = trainingB5)
postResample(train_resultsF_4th_svm, trainingB5$FLOOR)
# Accuracy  99.91 %
# Kappa     0.9989


# Results on validation set
validation_cleaned$DetectedWAPs <- apply(validation_cleaned[,1:465], 1,
                                         function(x) length(which(x != -105)))

# Creating the predictions and check against actual numbers
validation_resultsF_4th_svm <- predict(object = mod_svmF4, 
                                       newdata = validation_cleaned)
postResample(validation_resultsF_4th_svm, validation_cleaned$FLOOR)
# Accuracy  91.63 %
# Kappa     0.8837




#--- 5th SVM training model----

# Using training set after adjusting the outliers to a signal strength of -30 dBm

# Train the 5th SVM model
mod_svmF5 <- train(FLOOR~., data = trainingB6 %>% 
                     select(FLOOR, starts_with("WAP")),
                   method = "svmRadial",
                   trControl = Control,
                   tuneLength = 5)


#Check results on the training set
train_resultsF_5th_svm <- predict(object = mod_svmF5, newdata = trainingB6)
postResample(train_resultsF_5th_svm, trainingB6$FLOOR)
# Accuracy  99.79 %
# Kappa     0.9973


# Results on validation set
validation_resultsF_5th_svm <- predict(object = mod_svmF5, 
                                       newdata = validation_cleaned)
postResample(validation_resultsF_5th_svm, validation_cleaned$FLOOR)
# Accuracy  91.54 %
# Kappa     0.8826




#--- 6th SVM training model----

# Using training set that is scaled by row

# Train the 6th SVM model
mod_svmF6 <- train(FLOOR~., data = trainingNorm %>% 
                     select(FLOOR, starts_with("WAP")),
                   method = "svmRadial",
                   trControl = Control,
                   tuneLength = 5)


#Check results on the training set
train_resultsF_6th_svm <- predict(object = mod_svmF6, newdata = trainingNorm)
postResample(train_resultsF_6th_svm, trainingNorm$FLOOR)
# Accuracy  99.96 %
# Kappa     0.9994


# Results on validation set
validation_resultsF_6th_svm <- predict(object = mod_svmF6, 
                                       newdata = ValidationNorm)
postResample(validation_resultsF_6th_svm, ValidationNorm$FLOOR)
# Accuracy  92.71 %
# Kappa     0.8984


# Confusion matrix
confusionMatrix(validation_resultsF_6th_svm, ValidationNorm$FLOOR)




#--- 7th SVM training model----

# Using training set that is scaled by row, removing userID 17 and removing
# some WAPs that are creating interference.
# To see how I selected WAPs, see "WAPs selection and visualizations.R"


# Setting upross validation
Control3 <- trainControl(method = "repeatedcv", 
                         number = 4, 
                         repeats = 1,
                         allowParallel = TRUE)


# Train 7th SVM model for floors
mod_svmF7 <- train(FLOOR~., data = trainingNorm %>% 
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
train_resultsF_7th_svm <- predict(object = mod_svmF7, newdata = trainingNorm)
postResample(train_resultsF_7th_svm, trainingNorm$FLOOR)
# Accuracy  99.48 %
# Kappa     0.9933


# Results on validation set
validation_resultsF_7th_svm <- predict(object = mod_svmF7, 
                                       newdata = ValidationNorm)
postResample(validation_resultsF_7th_svm, ValidationNorm$FLOOR)
# Accuracy  92.62 %
# Kappa     0.8971






#--- 8th SVM training model----

# Using training set that is scaled by row, removing userID 17 and removing
# some more WAPs that are creating interference

# Train 8th SVM model for floors
mod_svmF8 <- train(FLOOR~., data = trainingNorm %>%
                     filter(USERID != 17) %>% 
                     select(FLOOR, starts_with("WAP"), -c(WAP008, WAP046, 
                                                          WAP151, WAP184,
                                                          WAP185, WAP248, 
                                                          
                                                          WAP082, WAP083, 
                                                          WAP084, WAP085, 
                                                          WAP113, WAP114,
                                                          WAP175, WAP180, 
                                                          WAP181, WAP186,
                                                          WAP187, WAP248, 
                                                          WAP286, WAP398, 
                                                          WAP478, WAP503,
                                                          
                                                          WAP029, WAP035, 
                                                          WAP142, WAP256)
                            ),
                   method = "svmRadial",
                   trControl = Control3,
                   tuneLength = 5)


#Check results on the training set
train_resultsF_8th_svm <- predict(object = mod_svmF8, newdata = trainingNorm)
postResample(train_resultsF_8th_svm, trainingNorm$FLOOR)
# Accuracy  99.53 %
# Kappa     0.9938


# Results on validation set
validation_resultsF_8th_svm <- predict(object = mod_svmF8, 
                                       newdata = ValidationNorm)
postResample(validation_resultsF_8th_svm, ValidationNorm$FLOOR)
# Accuracy  91.63 %
# Kappa     0.8833



#--- 11th SVM training model----

# Same as mod_svmF8 with adding pre-process treatment in caret
# The "zv" pre-process in Caret identifies numeric predictor columns with a 
# single value (i.e. having zero variance) and excludes them from further 
# calculations

# Train 9th SVM model for floors
mod_svmF9 <- train(FLOOR~., data = trainingNorm %>% 
                      filter(USERID != 17) %>% 
                      select(FLOOR, starts_with("WAP"), -c(WAP008, WAP046, 
                                                           WAP151, WAP184,
                                                           WAP185, WAP248, 
                                                           
                                                           WAP082, WAP083, 
                                                           WAP084, WAP085, 
                                                           WAP113, WAP114,
                                                           WAP175, WAP180, 
                                                           WAP181, WAP186,
                                                           WAP187, WAP248, 
                                                           WAP286, WAP398, 
                                                           WAP478, WAP503,
                                                           
                                                           WAP029, WAP035, 
                                                           WAP142, WAP256)
                      ),
                    method = "svmRadial",
                    trControl = Control3,
                    tuneLength = 5,
                    preProcess = "zv")


#Check results on the training set
train_resultsF_9th_svm <- predict(object = mod_svmF9, newdata = trainingNorm)
postResample(train_resultsF_9th_svm, trainingNorm$FLOOR)
# Accuracy  99.53 %
# Kappa     0.9938


# Results on validation set
validation_resultsF_9th_svm <- predict(object = mod_svmF9, 
                                       newdata = ValidationNorm)
postResample(validation_resultsF_9th_svm, ValidationNorm$FLOOR)
# Accuracy  84.07 %
# Kappa     0.7773





#--- 1st GBM training model----

# Using training set that is scaled by row, removing userID 17 and removing
# some WAPs that are creating interference

# Train 1st GBM model for floors
mod_gbmF1 <- train(FLOOR~., data = trainingNorm %>% 
                     filter(USERID != 17) %>% 
                     select(FLOOR, starts_with("WAP"), -c(WAP008, WAP046, 
                                                          WAP151, WAP184,
                                                          WAP185, WAP248, 
                                                          
                                                          WAP082, WAP083, 
                                                          WAP084, WAP085, 
                                                          WAP113, WAP114,
                                                          WAP175, WAP180, 
                                                          WAP181, WAP186,
                                                          WAP187, WAP248, 
                                                          WAP286, WAP398, 
                                                          WAP478, WAP503,
                                                          
                                                          WAP029, WAP035, 
                                                          WAP142, WAP256)
                     ),
                   method = "gbm",
                   trControl = Control,
                   tuneLength = 5,
                   verbose = FALSE)


#Save model
saveRDS(mod_gbmF1, file = "GBMmodel1_Floor.rds")


#Check results on the training set
train_resultsF_1st_gbm <- predict(object = mod_gbmF1, newdata = trainingNorm)
postResample(train_resultsF_1st_gbm, trainingNorm$FLOOR)
# Accuracy  99.42 %
# Kappa     0.9924


# Results on validation set
validation_resultsF_1st_gbm <- predict(object = mod_gbmF1, 
                                       newdata = ValidationNorm)
postResample(validation_resultsF_1st_gbm, ValidationNorm$FLOOR)
# Accuracy  93.88 %
# Kappa     0.9137


# Confusion matrix
confusionMatrix(validation_resultsF_1st_gbm, ValidationNorm$FLOOR)




#--- 2nd GBM training model----

# Using training set where outliers are removed.
# Using all WAPs

# Training 2nd GBM model for floors
mod_gbmF2 <- train(FLOOR~., data = trainingB5 %>% 
                     select(FLOOR, starts_with("WAP")),
                   method = "gbm",
                   trControl = Control,
                   tuneLength = 5,
                   verbose = FALSE)


#Check results on the training set
train_resultsF_2nd_gbm <- predict(object = mod_gbmF2, newdata = trainingB5)
postResample(train_resultsF_2nd_gbm, trainingB5$FLOOR)
# Accuracy  1 %
# Kappa     1


# Results on validation set
validation_resultsF_2nd_gbm <- predict(object = mod_gbmF2, 
                                       newdata = validation_cleaned)
postResample(validation_resultsF_2nd_gbm, validation_cleaned$FLOOR)
# Accuracy  90.73 %
# Kappa     0.8702





#--- 3rd RF training model---- 


# Using training set that is converted from decibels relative to a milliwatt (dBm) 
# to Power in milliwatt.
# Removing some WAPs that are creating interference


## Register doParallel to run the training model faster
# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2


# Train 3rd RF model for floors
mod_rfF3 <- train(FLOOR~., data = trainingLog %>% 
                    select(FLOOR, starts_with("WAP"), -c(WAP008, WAP046, 
                                                         WAP151, WAP184,
                                                         WAP185, WAP248, 
                                                         
                                                         WAP082, WAP083, 
                                                         WAP084, WAP085, 
                                                         WAP113, WAP114,
                                                         WAP175, WAP180, 
                                                         WAP181, WAP186,
                                                         WAP187, WAP248, 
                                                         WAP286, WAP398, 
                                                         WAP478, WAP503,
                                                         
                                                         WAP029, WAP035, 
                                                         WAP142, WAP256)
                           ),
                  method = "rf",
                  trControl = Control,
                  tuneLength = 5)

# Stop Cluster. After performing tasks, stop cluster. 
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_3rd_rf <- predict(object = mod_rfF3, newdata = trainingLog)
postResample(train_resultsF_3rd_rf, trainingLog$FLOOR)
# Accuracy  1
# Kappa     1



# Results on validation set

## Convert validation set from dBm to power in milliwatt
validationLog <- cbind(apply(select(validation_cleaned, starts_with('WAP')), 2, mW),
                       validation_cleaned[,466:474])


# Check predictions against actual values
validation_resultsF_3rd_rf <- predict(object = mod_rfF3,
                                       newdata = validationLog)
postResample(validation_resultsF_3rd_rf, validationLog$FLOOR)
# Accuracy  44.73 %
# Kappa     0.0723





#--- 4th RF training model----

# New sample for floor ----
# sample based on distribution of floors


## Using dataset that is pre-processed and rows with outliers have been
## removed


#Set seed
set.seed(123)

# Create a sample for floor
sampleF2 <- createDataPartition(v4data$FLOOR,
                                p = .30,
                                list = FALSE)
sampleDataF2 <- v4data[sampleF2,]

#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(sampleDataF2$FLOOR, 
                                  p = .80, 
                                  list = FALSE)
trainingF2 <- sampleDataF2[inTraining,]
testingF2 <- sampleDataF2[-inTraining,]




# Create Cluster with desired number of cores
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2


# Train 4th RF model for floors
mod_rfF4 <- train(FLOOR~., data = trainingF2 %>% 
                    select(FLOOR, starts_with("WAP")),
                  method = "rf",
                  trControl = Control,
                  tuneLength = 5)

# Stop Cluster. After performing tasks, stop cluster. 
stopCluster(cl)
registerDoSEQ()
rm(cl)


#Check results on the training set
train_resultsF_4th_rf <- predict(object = mod_rfF4, newdata = trainingF2)
postResample(train_resultsF_4th_rf, trainingF2$FLOOR)
# Accuracy  1
# Kappa     1


# Results on validation set
validation_resultsF_4th_rf <- predict(object = mod_rfF2, 
                                      newdata = validation_cleaned)
postResample(validation_resultsF_4th_rf, validation_cleaned$FLOOR)
# Accuracy  88.57 %
# Kappa     0.8404

# Same metrics as for 2nd RF model

# The best metrics was provided by mod_gbmF1

# To improve my model, I will try to optimize model per building
# See "Models for floor - per building.R"
