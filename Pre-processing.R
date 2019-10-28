################################################################
#IoT Analytics
#Evaluate Techniques for WiFi Locationing 

# Pre-processing

#Created by Eirik Espe
################################################################

source("Initial exploration.R")

#--- Pre-processing ----

# Find columns where signal strength is not detected
noSignal <- data[sapply(data, function(x) length(unique(x)) == 1)]
# 55 columns(WAPs) with no signals detected

#Alternatives
#     data %>% select(which(apply(., 2, var) == 0))
#     data %>% select_if(~n_distinct(.) == 1)

# Check wheter the same is true for the validation dataset

# Find columns where signal strength is not detected in the validation dataset
noSignalV <- validation[,1:520][sapply(validation[,1:520], 
                                       function(x) length(unique(x)) == 1)]
# some WAPs have no detected signals in the training set, but they have signal
# in the validation set. This is fine, because the model will then look for
# information elsewhere when looking at the validation set. On the other hand
# we do not have any information to compare with from the training set, so it 
# is to no help to include these columns.



# Dataset with pre-processed data 


#   Step 1: Removing columns with no detected signal
v2data <- select(data, -one_of(names(noSignal)))
#           New dataset with 474 columns (529-55)



#   Step 2: Change value for signals that was not detected.
#           The current value is +100. When the signal strengths are given
#           with values between 0 and -104, this will confuse the model.
#           New value -105
v2data[,1:465] <- replace(v2data[,1:465], v2data[,1:465] == 100, -105)


#   Step 3: Find rows with zero variance, i.e, record without signal
recordsNoSignal <- which(apply(v2data[,1:465], 1, 
                               function(x) length(unique(x)) == 1))

#           Alternatives: 
#                         which(apply(v2data[,1:465], 1, function(x) var(x) == 0))

#           Using dplyr (creates a data frame):
#                         v2data %>% filter_at(vars(contains("WAP")), 
#                                              all_vars(. == -105))                    

v2data <- v2data[-c(recordsNoSignal),]



#   Step 4: Look for outliers

#   For loop of 50 boxplots of WAP signal strengths from the v2data-set
chunk_size <- 50
for (i in seq(from = 1, to = 465, by = chunk_size)) {

# Creating a condition for the for loop to include the remaining chunk that is
# smaller than 50 
seq_size <- chunk_size
if((i + seq_size) > 465) seq_size <- 465 - i + 1 

outlierplot <- 
  
  #Creating the dataset to long format
  v2data[,c(i:(i+(seq_size - 1)), 466:474)] %>%
  melt(id.vars  = c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID", 
                    "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP"),
       variable.name = "WAP",
       value.name = "Signal") %>% 
  filter(between(Signal, -90, 0)) %>%       # Signals below -90 dBm are unusable.
                                            # Any functionality is highly unlikely
  
  # Plot
  ggplot(aes(x = WAP, y = Signal)) + geom_boxplot(aes(colour = PHONEID)) +
  coord_flip() + 
  labs(title = "Signal strength outliers", y = "Signal strength (dBm)")
  
  print(outlierplot)
}



# All boxplots
#v2data %>%
#  melt(id.vars  = c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID", 
#                    "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP"),
#       variable.name = "WAP",
#       value.name = "Signal") %>% 
#  filter(between(Signal, -40, 0)) %>%       # Signals below -90 dBm are unusable.
#  # Any functionality is highly unlikely
  
#  ggplot(aes(x = WAP, y = Signal)) + geom_boxplot(aes(colour = PHONEID)) + 
#  coord_flip() + 
#  facet_grid(~BUILDINGID) -> pboxplot

#ggplotly(pboxplot)



# Plot of outliers using density plot of signal distribution
data %>% 
  melt(id.vars = c(521:529),
       variable.name = "WAP",
       value.name = "Signal") %>%
  filter(Signal != 100) %>% 
  
  ggplot(aes(x = Signal)) + geom_density(aes(fill = BUILDINGID), alpha = 0.5) + 
  guides(fill = guide_legend(title = "Building")) + 
  labs(title = "Distribution of signal strength", x = "Signal strength (dBm)") -> plot1

# Interactive plot
ggplotly(plot1)

# Signal strengths stronger than -30 dBm is not common. This is in accordance
# with articles on WiFi signal strength, e.g. 
# https://www.metageek.com/training/resources/wifi-signal-strength-basics.html



# Count percentage of records with signal strength stronger than -30 dBm

## First, how many rows do we have in total
v2data %>% count()
# 19 861 records    (# of rows after removing  76 records with no detected signals)


## How many records with signal strength stronger than -30 dBm
v2data %>% filter_at(vars(contains("WAP")), any_vars(. > -30)) %>% count()
# 492 records

# Percentage of records that have signal strengths stronger than -30 dBm
492 / 19861     #  2.47% of the recorded signal strengths


# Set outliers as not detected signals ----
v3data <- cbind(replace(v2data[,1:465], v2data[,1:465] > -30 & v2data[,1:465] <= 0,
                        -105), 
                v2data[,466:474])
# (records with signal strength stronger than -30 dBM set to -105 dBm)


# or we can remove rows with outliers

# Data frame with the outliers
outliers <- v2data %>% filter_at(vars(contains("WAP")), any_vars(.> -30))

# How many outliers?
nrow(outliers)          # 492 rows with outliers


# Remove rows with outliers
v4data <- v2data %>% filter_at(vars(contains("WAP")), all_vars(. <= -30))


#Alternative using base R
  #v4data <- v2data[!apply(v2data[,1:465], 1, function(x) {any(x > -30)}),]

#  identical(v2data[!apply(v2data[,1:465], 1, function(x) {any(x > -30)}),],
#            v2data %>% filter_at(vars(contains("WAP")), all_vars(.<= -30)))



# or alternatively, we can set outliers to a signal strength of -30 dBm

# Set outliers to signal -30 dBm ----
v5data <- cbind(replace(v2data[,1:465], v2data[,1:465] > -30 & v2data[,1:465] <= 0,
                          -30), 
                  v2data[,466:474])

# Checking max value of the new dataset
v5data %>% select(starts_with("WAP")) %>% max()   # max signal strength is -30 dBm


# Remove user 6 from dataset. This user has 87.4% of the records with signal strength 
# higher than -30 dBm. User 6 also has 10.6 % of records with signal 
# strength higher than -50 dBm. This indicates that his device was probably 
# unrepresentative, buggy or had some sort of compatibility issue.
v6data <- v5data %>% filter(USERID != 6)
## When user 6 was removed from from the dataset, the performance of my models
## deteriorated.


# Calculation of percentage of user 6 records with signal strength higher 
# than -30 dBm
data %>% 
  filter(USERID == 6) %>% 
  filter_at(vars(contains("WAP")), any_vars(between(., -29, 0))) %>% 
  count() / 
  
  # Total number of records with signal strength higher than - 30 dBm
  data %>% 
  filter_at(vars(contains("WAP")), any_vars(between(., -29, 0))) %>% 
  count()


# Final note on outliers
# In my final models I used the option to remove rows with outliers (v4data)
# as this was creating the best models, and makes more sense than adjusting
# the value




#   Step 5: Normalized the data by row
#           In order to make a prediction model it is preferable to have input 
#           that are on the same scale. In this case all the signal strengths
#           are measured on the same scale, buth the magnitude ranges from
#           -30 dBm to -105dBm. To prevent these magnitude differences biasing 
#           our model, I have applied normalization by row.


# Creating a function to scale the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

# The application of this function can be found in the training data script.



#   Final step: Convert from dBM (decibel-milliwatts) to power in miliwatts

# Equations:
# P(dBm) = 10 * log(P(mW) / 1mW)              (log base 10 ~ log10)

# P(mW) = 1mW * 10^(P(dBm) / 10)

#Function to return power in miliwatts with dBm as input
mW <- function(dBm) {
  return (1 * 10^(dBm/10))
  }




#--- Validation pre-processing -----


# Apply the same pre-processing on validation dataset

# Step 1: Removing columns with no detected signal
validation_cleaned <- select(validation, -one_of(names(noSignal)))


# Step 2: Change value for signals that was not detected.
#           The current value is +100. When the signal strengths are given
#           with values between 0 and -104, this will confuse the model.
#           New value -105
validation_cleaned[,1:465] <- replace(validation_cleaned[,1:465], 
                                      validation_cleaned[,1:465] == 100, -105)


# Step 3: Find rows with zero variance, i.e, record without signal
recordsNoSignalV <- which(apply(validation_cleaned[,1:465], 1, 
                                function(x) length(unique(x))==1))
# No records without signal found


# Step 4: Remove rows with outliers
voutliers <- validation_cleaned %>% filter_at(vars(contains("WAP")), 
                                              any_vars(.> -30))
# No rows with outliers


# Step 5: Normalized the data by row

# Normalized validation dataset
ValidationNorm <- cbind(t(apply(select(validation_cleaned, 
                                       starts_with('WAP')), 1, normalize)),
                        validation_cleaned[,466:474])




#--- Creating validation set per building ----

# Extract dataset for building 0
VBuilding0 <- validation_cleaned %>% filter(BUILDINGID == 0) %>% droplevels()

#Normalized validation dataset - building 0
ValidationNormB0 <- cbind(t(apply(select(VBuilding0, 
                                         starts_with('WAP')), 1, normalize)),
                          VBuilding0[, 466:474])

--------------------------------
# Extract dataset for building 1
VBuilding1 <- validation_cleaned %>% filter(BUILDINGID == 1) %>% droplevels()

#Normalized validation dataset - building 1
ValidationNormB1 <- cbind(t(apply(select(VBuilding1, 
                                         starts_with('WAP')), 1, normalize)),
                          VBuilding1[, 466:474])

--------------------------------
# Extract dataset for building 2
VBuilding2 <- validation_cleaned %>% filter(BUILDINGID == 2) %>% droplevels()

#Normalized validation dataset - building 2
ValidationNormB2 <- cbind(t(apply(select(VBuilding2, 
                                         starts_with('WAP')), 1, normalize)),
                          VBuilding2[, 466:474])

--------------------------------


# Miscellaneous testing:

# Number of rows in the un-processed dataset with signal higher than -30 dBm
data %>% filter_at(vars(contains("WAP")), any_vars(between(., -29, 0))) %>% count()
# 492 records


# Formula (do not include columns that has a name that starts with "WAP") 
data %>% select(-starts_with("WAP")) %>% head



# Scaling the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}



# Adding building predictions to training set
trainingNorm$PredBuilding <- predict(object = mod_svmB8, newdata = trainingNorm)

#Connvert to Power in milliwatt
trainingLog <- cbind(apply(select(trainingB5, starts_with('WAP')), 2, mW),
                     trainingB5[,c(466:474, 476)])
#Normalize
trainingLog <- cbind(t(apply(select(trainingLog, 
                                    starts_with('WAP')), 1, normalize)), 
                     trainingLog[,466:475])



# Data frame with number of records per timestamp
Tstamp <- data %>% 
  select(TIMESTAMP) %>%  
  mutate(Timestamp = as.POSIXct(data$TIMESTAMP, 
                                origin = "1970-01-01", tz = "UTC")) %>% 
  group_by(TIMESTAMP, Timestamp) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  ungroup()
# Most frequent timestamp is 2013-06-20 08:01:31 or (1371715291 using UNIX time)
# This timestamp was recorded 18 times


# Looking at the 18 records
data %>% 
  filter(TIMESTAMP == 1371715291) %>% 
  select(TIMESTAMP, LONGITUDE, LATITUDE, BUILDINGID, FLOOR, USERID, PHONEID, 
         RELATIVEPOSITION)
# UserID 13 has 9 of the records


# Looking at the signal strengths detected at this timestamp for user 13
similarRecords <- data %>% 
  filter(TIMESTAMP == 1371715291 & USERID == 13) %>% 
  select_if(function(x) any(x != 100))



#--- Aggregate rows with duplicate information ----

# Creating a data frame where duplicated rows are aggregated

## First we convert the positive values of 100 to -105, in order to avoid creating
## means that does not make sense. 
## E.g. if a column has the values +100 and -89, the mean will be 5.5. 
## This does not make sense when WiFi signal strengths ranges from -30 to -104 dBm

aggData1 <- data
aggData1[, 1:520] <- replace(data[, 1:520], data[, 1:520] == 100, -105)

## Create an aggregation of the WAP columns using mean
aggData1 <-  aggData1 %>%
  group_by(TIMESTAMP, USERID, PHONEID) %>% 
  summarise_at(vars(contains("WAP")), mean) %>% 
  ungroup()

## Create a function for finding the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Create an aggregation of the space identifier variables
aggData2 <- data[, 521:529]
aggData2 <- aggData2 %>% 
  group_by(TIMESTAMP, USERID, PHONEID) %>% 
  summarise_at(vars(c("LONGITUDE":"RELATIVEPOSITION")), Mode) %>% 
  ungroup()

## Combine the two data frames
aggData <- inner_join(aggData1, aggData2, by = c("TIMESTAMP", "USERID", "PHONEID"))


# Reorder columns of the aggData data frame to the original column order
aggData <- select(aggData, 
                  starts_with("WAP"), 
                  LONGITUDE:RELATIVEPOSITION,
                  USERID, PHONEID, TIMESTAMP)


# Columns with no variance
aggData %>% select_if(~n_distinct(.) == 1) %>% ncol()
## 55 columns

# Rows with no variance
aggData %>% filter_at(vars(contains("WAP")), all_vars(. == -105)) %>% count()
## 60 rows


#--- Pre-processing of aggregated data ----

# Step 1: Removing columns with no detected signal
aggDataPre <- aggData %>% select_if(~n_distinct(.) > 1)

# Step 2: Remove rows with zero variance, i.e, record without detected signal
aggDataPre <- aggDataPre %>% filter_at(vars(contains("WAP")), any_vars(. != -105))


# Step 3: Look for outliers

## First, how many rows do we have in total
aggDataPre %>% count()
# 14 176 records    (# of rows after removing 60 records with no detected signals)


## How many records with signal strength stronger than -30 dBm
aggDataPre %>% filter_at(vars(contains("WAP")), any_vars(. > -30)) %>% count()
# 232 records

# Percentage of records that have signal strengths stronger than -30 dBm
232 / 14176     #  1.63% of the recorded signal strengths


# Remove rows with signal strenght higher than -30 dBm
aggDataPre <- aggDataPre %>% filter_at(vars(contains("WAP")), all_vars(. <= -30))


# Step 5: Normalized the data by row

# Normalized aggregated dataset
aggDataNorm <- cbind(t(apply(select(aggDataPre, 
                                    starts_with('WAP')), 1, normalize)),
                     select(aggDataPre, -starts_with("WAP")))


