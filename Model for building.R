################################################################
#IoT Analytics
#Evaluate Techniques for WiFi Locationing 

#Models for determining which building

#Created by Eirik Espe
################################################################

source("Pre-processing.R")


#--- Creating the first model ----

#Set seed
set.seed(123)

# Create a sample for the dataset
sampleB <- createDataPartition(data$BUILDINGID,
                               p = .30,
                               list = FALSE)
sampleData <- data[sampleB,]

#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(sampleData$BUILDINGID, 
                                  p = .80, 
                                  list = FALSE)
training <- sampleData[inTraining,]
testing <- sampleData[-inTraining,]


#---Cross validation----

#Setting a 4-fold cross validation to use on the model
Control <- trainControl(method = "repeatedcv", 
                        number = 4, 
                        repeats = 1)


#---Training model----

#Train a kNN model with tunelength = 5
mod_knnB1 <- train(BUILDINGID~., data = training %>% 
                     select(BUILDINGID, starts_with("WAP")),
                  method = "knn",
                  trControl = Control, 
                  tuneLength = 5)



#---Results 1st kNN model----

#Check results on the training set
train_resultsB_1st_knn <- predict(object = mod_knnB1, newdata = training)
postResample(train_resultsB_1st_knn, training$BUILDINGID)
# Accuracy  99.64 %
# Kappa     0.9944


#Results on testing set
test_resultsB_1st_knn <- predict(object = mod_knnB1, newdata = testing)
postResample(test_resultsB_1st_knn, testing$BUILDINGID)
# Accuracy  99.50 %
# Kappa     0.9921


# Create a variable in the testing dataset with building predictions
testing$pred1Building <- test_resultsB_1st_knn

# Check where the model got wrong building
testing[testing$BUILDINGID != testing$pred1Building, c(521:526, 529:530)]



#--- Visualizing the errors ----


plot_ly(testing, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, 
        color = (~BUILDINGID == pred1Building),
        name = list("Building 0", "Building 1", "Building 2"), 
        type = 'scatter3d', mode = 'markers') %>% 
  layout(title = "Universitat Jaume I", list(showticklabels = FALSE))




# Results on validation set
validation_resultsB_1st_knn <- predict(object = mod_knnB1, 
                                       newdata = validation)
postResample(validation_resultsB_1st_knn, validation$BUILDINGID)
# Accuracy  97.48 %
# Kappa     0.9604



#--- Model after 1st pre-processing

# WAPs (columns) with no detected signal and records (rows) without 
# detected signal have been removed.

# Undetected signals was converted from 100 to -105



## New training set

#Set seed
set.seed(123)

# Create a sample for the dataset
sampleB2 <- createDataPartition(v2data$BUILDINGID,
                               p = .30,
                               list = FALSE)
sampleDataB2 <- v2data[sampleB2,]


#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(sampleDataB2$BUILDINGID, 
                                  p = .80, 
                                  list = FALSE)
trainingB2 <- sampleDataB2[inTraining,]
testingB2 <- sampleDataB2[-inTraining,]



#--- 2nd Cross validation ----

#Setting a 4-fold cross validation to use on the model
Control <- trainControl(method = "repeatedcv", 
                        number = 4, 
                        repeats = 1)


#---Training model----

#Train a kNN model with tunelength = 5
mod_knnB2 <- train(BUILDINGID~., data = trainingB2 %>% 
                     select(BUILDINGID, starts_with("WAP")), 
                  method = "knn",
                  trControl = Control, 
                  tuneLength = 5)



#---Results 2nd kNN model----

#Check results on the training set
train_resultsB_2nd_knn <- predict(object = mod_knnB2, newdata = trainingB2)
postResample(train_resultsB_2nd_knn, trainingB2$BUILDINGID)
# Accuracy  1
# Kappa     1


#Results on testing set
test_resultsB_2nd_knn <- predict(object = mod_knnB2, newdata = testingB2)
postResample(test_resultsB_2nd_knn, testingB2$BUILDINGID)
# Accuracy  1
# Kappa     1


# Results on validation set
validation_resultsB_2nd_knn <- predict(object = mod_knnB2, 
                                       newdata = validation_cleaned)
postResample(validation_resultsB_2nd_knn, validation_cleaned$BUILDINGID)
# Accuracy  99.28 %
# Kappa     0.9886



#---Training model SVM----

# Using SVM algorithm on the same training dataset
mod_svmB1 <- train(BUILDINGID~., data = trainingB2 %>%
                     select(BUILDINGID, starts_with("WAP")),
                   method = "svmRadial",
                   trControl = Control, 
                   tuneLength = 5)


#Check results on the training set
train_resultsB_1st_svm <- predict(object = mod_svmB1, newdata = trainingB2)
postResample(train_resultsB_1st_svm, trainingB2$BUILDINGID)
# Accuracy  1
# Kappa     1


# 1st SVM model
# Results on validation set
validation_resultsB_1st_svm <- predict(object = mod_svmB1, 
                                       newdata = validation_cleaned)
postResample(validation_resultsB_1st_svm, validation_cleaned$BUILDINGID)
# Accuracy  99.82 %
# Kappa     0.9972



#--- New sample ----

# New sample adjusting outliers. Signal strengths higher than -30 dBm set to
# -105 dBm (not detected signals)


#Set seed
set.seed(123)

# Create a sample for the dataset
sampleB4 <- createDataPartition(v3data$BUILDINGID,
                                p = .30,
                                list = FALSE)
sampleDataB4 <- v3data[sampleB4,]

#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(sampleDataB4$BUILDINGID, 
                                  p = .80, 
                                  list = FALSE)
trainingB4 <- sampleDataB4[inTraining,]
testingB4 <- sampleDataB4[-inTraining,]


#--- 3rd Training model SVM----

# Adjusted outliers (signal strengths higher than -30 dBm converted to -105 dBm) 

mod_svmB3 <- train(BUILDINGID~., data = trainingB4 %>% 
                     select(BUILDINGID, starts_with("WAP")),
                   method = "svmRadial",
                   trControl = Control, 
                   tuneLength = 5)


#---Results 3rd SVM model----

#Check results on the training set
train_resultsB_3rd_svm <- predict(object = mod_svmB3, newdata = trainingB4)
postResample(train_resultsB_3rd_svm, trainingB4$BUILDINGID)
# Accuracy  1
# Kappa     1


#Results on testing set
test_resultsB_3rd_svm <- predict(object = mod_svmB3, newdata = testingB4)
postResample(test_resultsB_3rd_svm, testingB4$BUILDINGID)
# Accuracy  1
# Kappa     1 


# Results on validation set
validation_resultsB_3rd_svm <- predict(object = mod_svmB3,
                                       newdata = validation_cleaned)
postResample(validation_resultsB_3rd_svm, validation_cleaned$BUILDINGID)
# Accuracy  99.82 %
# Kappa     0.9972



#--- New sample ----

# New sample removing rows with outliers

#Set seed
set.seed(123)

# Create a sample for the dataset
sampleB5 <- createDataPartition(v4data$BUILDINGID,
                                p = .30,
                                list = FALSE)
sampleDataB5 <- v4data[sampleB5,]

#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(sampleDataB5$BUILDINGID, 
                                  p = .80, 
                                  list = FALSE)
trainingB5 <- sampleDataB5[inTraining,]
testingB5 <- sampleDataB5[-inTraining,]



#--- 4th Training model SVM----

# Training model using SVM algorithm and removing rows with outliers
mod_svmB4 <- train(BUILDINGID~., data = trainingB5 %>% 
                     select(BUILDINGID, starts_with("WAP")),
                   method = "svmRadial",
                   trControl = Control, 
                   tuneLength = 5)


#---Results 4th SVM model----

#Check results on the training set
train_resultsB_4th_svm <- predict(object = mod_svmB4, newdata = trainingB5)
postResample(train_resultsB_4th_svm, trainingB5$BUILDINGID)
# Accuracy  1 
# Kappa     1


#Results on testing set
test_resultsB_4th_svm <- predict(object = mod_svmB4, newdata = testingB5)
postResample(test_resultsB_4th_svm, testingB5$BUILDINGID)
# Accuracy  1
# Kappa     1


# Results on validation set
validation_resultsB_4th_svm <- predict(object = mod_svmB4,
                                       newdata = validation_cleaned)
postResample(validation_resultsB_4th_svm, validation_cleaned$BUILDINGID)
# Accuracy  99.73 %
# Kappa     0.9957



#--- New sample ----

# Adjusting outliers to -30 dBm


#Set seed
set.seed(123)

# Create a sample for the dataset
sampleB6 <- createDataPartition(v5data$BUILDINGID,
                                p = .30,
                                list = FALSE)
sampleDataB6 <- v5data[sampleB6,]


#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(sampleDataB6$BUILDINGID, 
                                  p = .80, 
                                  list = FALSE)
trainingB6 <- sampleDataB6[inTraining,]
testingB6 <- sampleDataB6[-inTraining,]


#--- 5th Training model SVM----

# Training model using SVM algorithm and adjusting outliers to -30 dBm
mod_svmB5 <- train(BUILDINGID~., data = trainingB6 %>% 
                     select(BUILDINGID, starts_with("WAP")),
                   method = "svmRadial",
                   trControl = Control, 
                   tuneGrid = expand.grid(sigma = 4.590076e-05, C = 2))



#---Results 5th SVM model----


#Check results on the training set
train_resultsB_5th_svm <- predict(object = mod_svmB5, newdata = trainingB6)
postResample(train_resultsB_5th_svm, trainingB6$BUILDINGID)
# Accuracy  1
# Kappa     1


#Results on testing set
test_resultsB_5th_svm <- predict(object = mod_svmB5, newdata = testingB6)
postResample(test_resultsB_5th_svm, testingB6$BUILDINGID)
# Accuracy  1
# Kappa     1


# Results on validation set
validation_resultsB_5th_svm <- predict(object = mod_svmB5,
                                       newdata = validation_cleaned)
postResample(validation_resultsB_5th_svm, validation_cleaned$BUILDINGID)
# Accuracy  99.82 %
# Kappa     0.9972



#--- 6th Training model SVM----

# To be able to run this model, you have to run the 2nd kNN model from
# the longitude script (line number 65 to 74)  and 
# the 2nd kNN model from the latitude script (line number 66 to 76).


# Add predictions og longitude and latitude to the training data
trainingL <- cbind(trainingB5, train_resultsLon_2nd_knn, train_resultsLat_2nd_knn)

#Rename the predictions columns
trainingL <- rename(trainingL, 
                    PredLongitude = train_resultsLon_2nd_knn,
                    PredLatitude = train_resultsLat_2nd_knn)



# Training model using SVM and including predictions for longitude and latitude
mod_svmB6 <- train(BUILDINGID~., data = trainingL %>% 
                     select(BUILDINGID, starts_with("WAP"), 
                            PredLongitude, PredLatitude),
                   method = "svmRadial",
                   trControl = Control, 
                   tuneLength = 5)


#---Results 6th SVM model----


#Check results on the training set
train_resultsB_6th_svm <- predict(object = mod_svmB6, newdata = trainingL)
postResample(train_resultsB_6th_svm, trainingL$BUILDINGID)
# Accuracy  99.98 %
# Kappa     0.9997



#--- New sample removing userID #6 ----

# After removing userID #6

#Set seed
set.seed(123)

# Create a sample for the dataset
sampleB7 <- createDataPartition(v6data$BUILDINGID,
                                p = .30,
                                list = FALSE)
sampleDataB7 <- v6data[sampleB7,]

#Define a 80%/20% train/test split of the sample dataset
inTraining <- createDataPartition(sampleDataB7$BUILDINGID, 
                                  p = .80, 
                                  list = FALSE)
trainingB7 <- sampleDataB7[inTraining,]
testingB7 <- sampleDataB7[-inTraining,]


#--- 7th Training model SVM----
mod_svmB7 <- train(BUILDINGID~., data = trainingB7 %>% 
                     select(BUILDINGID, starts_with("WAP")),
                   method = "svmRadial",
                   trControl = Control, 
                   tuneLength = 5)


#---Results 7th SVM model----

#Check results on the training set
train_resultsB_7th_svm <- predict(object = mod_svmB7, newdata = trainingB7)
postResample(train_resultsB_7th_svm, trainingB7$BUILDINGID)
# Accuracy  1
# Kappa     1


#Results on testing set
test_resultsB_7th_svm <- predict(object = mod_svmB7, newdata = testingB7)
postResample(test_resultsB_7th_svm, testingB7$BUILDINGID)
# Accuracy  1
# Kappa     1


# Results on validation set
validation_resultsB_7th_svm <- predict(object = mod_svmB7,
                                       newdata = validation_cleaned)
postResample(validation_resultsB_7th_svm, validation_cleaned$BUILDINGID)
# Accuracy  99.82 %
# Kappa     0.9972



#--- 8th Training model SVM (normalized by row)----


# Creating a model where the signal strengths are scaled by row

# Function for Min-Max Scaling of the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}


# Apply normalization of rows on the trainingB5
trainingNorm <- cbind(t(apply(select(trainingB5, starts_with('WAP')), 
                              1, normalize)),
                      select(trainingB5, -starts_with("WAP")))


# Apply normalization on the testing set (testingB5)
testingNorm <- cbind(t(apply(select(testingB5, 
                                    starts_with('WAP')), 1, normalize)), 
                     select(testingB5, -starts_with("WAP")))


# Training model using SVM algorithm on data with scaling by rows
mod_svmB8 <- train(BUILDINGID~., data = trainingNorm %>% 
                     select(BUILDINGID, starts_with("WAP")),
                   method = "svmRadial",
                   trControl = Control, 
                   tuneLength = 5)


#Check results on the training set
train_resultsB_8th_svm <- predict(object = mod_svmB8, newdata = trainingNorm)
postResample(train_resultsB_8th_svm, trainingNorm$BUILDINGID)
# Accuracy  1
# Kappa     1


#Results on testing set
test_resultsB_8th_svm <- predict(object = mod_svmB8, newdata = testingNorm)
postResample(test_resultsB_8th_svm, testingNorm$BUILDINGID)
# Accuracy  1
# Kappa     1


# Results on validation set

## Normalized validation dataset
ValidationNorm <- cbind(t(apply(select(validation_cleaned, 
                                       starts_with('WAP')), 1, normalize)),
                        validation_cleaned[,466:474])

## Check predictions on validation data
validation_resultsB_8th_svm <- predict(object = mod_svmB8,
                                       newdata = ValidationNorm)
postResample(validation_resultsB_8th_svm, ValidationNorm$BUILDINGID)
# Accuracy  1
# Kappa     1



#--- 3rd Cross validation----

#Setting a 4-fold cross validation to use on the model
Control2 <- trainControl(method = "repeatedcv", 
                        number = 4, 
                        repeats = 1,
                        summaryFunction = multiClassSummary)
