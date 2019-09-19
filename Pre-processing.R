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
for (i in seq(from = 1, to = 465, by = 50)) {
  
outlierplot <- 
  
  v2data[,c(i:(i+49), 466:474)] %>%
  melt(id.vars  = c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID", 
                    "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP"),
       variable.name = "WAP",
       value.name = "Signal") %>% 
  filter(between(Signal, -90, 0)) %>%       # Signals below -90 dBm are unusable.
                                            # Any functionality is highly unlikely
  
  ggplot(aes(x = WAP, y = Signal)) + geom_boxplot(aes(colour = PHONEID)) +
  coord_flip() + 
  labs(title = "Signal strength outliers", y = "Signal strength (dBm)")
  
  print(outlierplot)
}


#   Last 12 WAPs
v2data[,c(451:465, 466:474)] %>%
  melt(id.vars  = c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID", 
                    "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP"),
       variable.name = "WAP",
       value.name = "Signal") %>% 
  filter(between(Signal, -90, 0)) %>%       # Signals below -90 dBm are unusable.
                                            # Any functionality is highly unlikely
  
  ggplot(aes(x = WAP, y = Signal)) + geom_boxplot(aes(colour = PHONEID)) + 
  coord_flip() +
  labs(title = "Signal strength outliers", y = "Signal strength (dBm)")


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



# Count percentage of observations with signal strength stronger than -30 dBm

## First, how many records do we have in total
data %>%
  melt(id.vars  = c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID", 
                    "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP"),
       variable.name = "WAP",
       value.name = "Signal") %>% 
  count()
# 10 367 240 records


## How many records with signal strength stronger than -30 dBm
data %>%
  melt(id.vars  = c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID", 
                    "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP"),
       variable.name = "WAP",
       value.name = "Signal") %>% 
  filter(between(Signal, -29, 0)) %>% 
  count()
# 775 records

# Percentage
775 / 10367240     #  0.007% of the recorded signal strengths


# Set outliers as not detected signals ----
v3data <- cbind(replace(v2data[,1:465], v2data[,1:465] > -30 & v2data[,1:465] <= 0,
                        -105), 
                v2data[,466:474])

#or

# Data frame with the outliers
outliers <- v2data %>% filter_at(vars(contains("WAP")), any_vars(.> -30))

# How many outliers?
nrow(outliers)          # 492 outliers


# Remove rows with outliers
v4data <- v2data[!apply(v2data[,1:465], 1, function(x) {any(x > -30)}),]

#Alternative
  #v4data <- v2data %>% filter_at(vars(contains("WAP")), all_vars(.<= -30))

#identical(v2data[!apply(v2data[,1:465], 1, function(x) {any(x > -30)}),], 
#          v2data %>% filter_at(vars(contains("WAP")), all_vars(.<= -30)))



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



#   Final step: Convert from dBM (decibel-milliwatts) to power in miliwatts

# Equations:
# P(dBm) = 10 * log(P(mW) / 1mW)              (log base 10 ~ log10)

# P(mW) = 1mW * 10^(P(dBm) / 10)

#Function to return power in miliwatts with dBm as input
mW <- function(dBm) {
  return (1 * 10^(dBm/10))
  }




# Number of rows with signal higher than -30 dBm
data %>% filter_at(vars(contains("WAP")), any_vars(between(., -29, 0))) %>% count()

# Formula (do not include columns that has a name that starts with "WAP") 
data %>% select(-starts_with("WAP"))



# Scaling the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

# New training sample with normalized rows
trainingNorm <- cbind(t(apply(select(trainingB5, 
                                     starts_with('WAP')), 1, normalize)), 
                      trainingB5[,c(466:474, 476)])
testingNorm <- cbind(t(apply(select(testingB5, 
                                    starts_with('WAP')), 1, normalize)), 
                     testingB5[,466:474])

# Adding building predictions to training set
trainingNorm$PredBuilding <- predict(object = mod_svmB8, newdata = trainingNorm)

#Connvert to Power in milliwatt
trainingLog <- cbind(apply(select(trainingB5, starts_with('WAP')), 2, mW),
                     trainingB5[,c(466:474, 476)])
#Normalize
trainingLog <- cbind(t(apply(select(trainingLog, 
                                    starts_with('WAP')), 1, normalize)), 
      trainingLog[,466:475])

