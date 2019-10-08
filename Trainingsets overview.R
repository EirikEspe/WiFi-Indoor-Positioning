################################################################
#IoT Analytics
#Evaluate Techniques for WiFi Locationing 

# Overview of training datasets

#Created by Eirik Espe
################################################################

source("Pre-processing.R")


#--- Training datasets for building ----

# Training datasets for predictive models for building
# characterized by the object name training immediately followed 
# by a B.
# E.g. trainingB2 means second training model for Building



# training  <-  is the first un-processed dataset.
#               The training set is based on 80 % of a weigted sample of the
#               original dataset. The weighted sample is made in order to reduce
#               calculation time for the training models.
#               The sample is weighted by distribution of buildings in the 
#               original dataset




# trainingB2 <- New training set after 1st pre-processing of the data.
#               WAPs (columns) with no detected signal and records (rows) 
#               without detected signal have been removed.
#
#               Undetected signals were converted from 100 to -105




# trainingB3 <- does not exist. This training set was removed




# trainingB4 <- Same pre-processing as trainingB2.
#               Outliers have been adjusted. Signal strengths higher than 
#               -30 dBm have been set to -105 dBm (same as not detected signals)




# trainingB5 <- Rows with outliers have been removed,
#               instead of just adjusting the outliers signal strength,




# trainingB6 <- Alternative outliers treatment.
#               The signal strength of outliers have been adjusted to -30 dBm.




# trainingL  <- Same as trainingB5, but predictions of longitude and latitude
#               have been added




# trainingB7 <- Same as training B6, but userID #6 has been removed.
#               This user has 87.4% of the records with signal strength 
#               higher than -30 dBm. User 6 also has 10.6 % of records with 
#               signal strength higher than -50 dBm. This indicates that 
#               his device was probably unrepresentative, buggy or 
#               had some sort of compatibility issue.




# trainingNorm <- This is same training set as trainingB5 with min-max scaling 
#                 of the rows. With signal strengths ranging from -30 dBm to
#                 -105 dBm, this is to prevent these magnitude differences 
#                 biasing our model.




#--- Training datasets for floors ----

# trainingF <- is the first un-processed dataset, sampled by distribution 
#              of floors.


# trainingF2 <- pre-processed and rows with outliers have been removed.
#               sampled by distribution of floors



# in the rest of the training sets, I use the same training sets as for 
# building.




#--- Training datasets for floors - per building ----

# trainingF_B0 <- Pre-processed data where information is extracted from building 0.


# trainingF_B0sc <- Same as trainingF_B0, where the data has been scaled by row.




# trainingF_B1 <- Pre-processed data where information is extracted from building 1.


# trainingF_B1sc <- Same as trainingF_B1, where the data has been scaled by row.




# trainingF_B2 <- Pre-processed data where information is extracted from building 2.


# trainingF_B2sc <- Same as trainingF_B2, where the data has been scaled by row.




