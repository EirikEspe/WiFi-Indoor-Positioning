################################################################
#IoT Analytics
#Evaluate Techniques for WiFi Locationing 

#Testing models on the validation dataset

#Created by Eirik Espe
################################################################

source("Model for latitude.R")



#--- Best model for building ----
postResample(predict(object = mod_svmB8, newdata = ValidationNorm), 
             ValidationNorm$BUILDINGID)
# Accuracy  1
# Kappa     1




#--- Best model for floor ----

## Combine the predictions from the best models for floors per building

# Add predictions from the best models as a column in corresponding data frame
ValidationNormB0$Prediction <- predict(object = mod_B0svm3, newdata = ValidationNormB0)
ValidationNormB1$Prediction <- predict(object = mod_B1gbm1, newdata = ValidationNormB1)
ValidationNormB2$Prediction <- predict(object = mod_B2gbm1, newdata = ValidationNormB2)

# Create a data frame with all the predictions the predictions
ValidationComplete <- rbind(ValidationNormB0, ValidationNormB1, ValidationNormB2)

## Results of floor predictions
postResample(ValidationComplete$Prediction, ValidationComplete$FLOOR)
# Accuracy  96.22 %
# Kappa     0.947

# Creating a confusion matrix of predictions and actual observations
confusionMatrix(ValidationComplete$Prediction, ValidationComplete$FLOOR)




#--- Best model for longitude ----

postResample(predict(object = mod_knnLon5, newdata = ValidationNorm), 
             ValidationNorm$LONGITUDE)
# RMSE  8.032
# R2    99.55 %
# MAE   5.467




#--- Best model for longitude ----

postResample(predict(object = mod_knnLat5, newdata = ValidationNorm), 
             ValidationNorm$LATITUDE)
# RMSE  7.645
# R2    98.83 %
# MAE   5.127



# Data frame for charts of predicted longitude and latitude vs. actual numbers
validation_cleaned_chart <- validation_cleaned

# Add predictions for Building
validation_cleaned_chart$PredictionsB <- 
  predict(object = mod_svmB8, newdata = ValidationNorm)


# Test if the predictions are equal to the actual numbers
identical(validation_cleaned_chart$BUILDINGID, validation_cleaned_chart$PredictionsB)


# Adding predictions for Longitude
validation_cleaned_chart$PredictionsLon <- 
  predict(object = mod_knnLon5, newdata = ValidationNorm)

# Adding predictions for Latitude
validation_cleaned_chart$PredictionsLat <- 
  predict(object = mod_knnLat5, newdata = ValidationNorm)



# Plot the predicted longitude and latitude
ggplot(validation_cleaned_chart, aes(x = PredictionsLon, y = PredictionsLat,
                                     color = BUILDINGID)) +
  geom_point() +
  scale_colour_discrete(name = "Building") +
  labs(title = "Universitat Jaume I", 
       subtitle = "Predicted Longitude and latitude",
       x = "Longitude", y = "Latitude") -> predlonglat

# Plot actual longitude and latitude
ggplot(validation_cleaned_chart, aes(x = LONGITUDE, y = LATITUDE, 
                                     color = BUILDINGID)) +
  geom_point() +
  scale_colour_discrete(name = "Building") +
  labs(subtitle = "Actual Longitude and latitude",
       x = "Longitude", y = "Latitude") -> actlonglat


gridExtra::grid.arrange(predlonglat, actlonglat)



#--- Which WAPs code ----
# validation_cleaned %>% 
#   filter(PredBuilding != BUILDINGID) %>% 
#   select_if(function(.) n_distinct(.) != 1)



# errorCheckF <- validation_cleaned %>% 
#   filter(Pred1Floor != FLOOR) %>% 
#   select_if(function(.) n_distinct(.) != 1)


