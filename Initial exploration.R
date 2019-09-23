################################################################
#IoT Analytics
#Evaluate Techniques for WiFi Locationing 

#Initial Exploration

#Created by Eirik Espe
################################################################

#Calling on packages. Install the packages if you do not have them already.
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(plotly)
library(caret)


#Import dataset - Training data
data <- read_csv("UJIndoorLoc/trainingData.csv")

# Looking for missing data
anyNA(data)   #No, did not find any missing values

# Look at the structure of the data. Column 1 to 520 contain intensity value 
#  for different WAPs
str(data[,c(1:3, 520:529)])

# Converting (floor, buildingID, spaceID, relative position, userID and 
#  phone ID attributes to factor
data[,c(523:528)] <- lapply(data[, c(523:528)], as.factor)



# Making samples

# Building 0 and building 1
building0v1 <- filter(data, BUILDINGID == 0 | BUILDINGID == 1)
# Building 1 and 2
building1v2 <- filter(data, BUILDINGID == 1 | BUILDINGID == 2)



# The best signals, with RSSI equal to 0
topsignal <- data %>% 
  melt(id.vars = c(521:529),
       variable.name = "WAP",
       value.name = "Signal") %>% 
  filter(Signal == 0)

# Reorder the columns with Access points in first column
topsignal <- select(topsignal, WAP, Signal, everything())


# Summary of signal strengths when signals was detected (not equal to +100)
data %>% 
  melt(id.vars = c(521:529),
       variable.name = "WAP",
       value.name = "Signal") %>% 
  select(WAP, Signal, everything()) %>%  
  filter(Signal != 100) %>% 
  summary


# Duration of time period for the training dataset
summary(as.POSIXct(data$TIMESTAMP,origin="1970-01-01", tz = "UTC"))
# 3 weeks (30/05/2013 to 20/06/2013)


# Unique users per building
data %>% filter(BUILDINGID == 0) %>% distinct(USERID)   # 2 users
data %>% filter(BUILDINGID == 1) %>% distinct(USERID)   # 12 users
data %>% filter(BUILDINGID == 2) %>% distinct(USERID)   # 16 users


# Data frame with a column for number of WAPs detected
data2 <- data
data2$DetectedWAPs <- apply(data2[,1:520], 1,
                             function(x) length(which(x != 100)))


# Create a vector for floor names to insert in facet_grid
Floors <- c("0" = "Floor 0",
            "1" = "Floor 1",
            "2" = "Floor 2",
            "3" = "Floor 3",
            "4" = "Floor 4")

# Create a vector of Labels for building to use in facet_grid below
Building <- c("0" = "Building 0",
              "1" = "Building 1", 
              "2" = "Building 2")



# Plot showing the distribution of WAPs detected per building
ggplot(data2, aes(x = DetectedWAPs)) + 
  geom_bar(aes(fill = BUILDINGID), show.legend = FALSE) + 
  labs(title = "Number of WAPs detected", x = "Number of WAPs") +
  facet_grid(BUILDINGID~., labeller = labeller(BUILDINGID = as_labeller(Building)))


#ggplot(data2, aes(x = DetectedWAPs)) + 
#  geom_bar(aes(fill = BUILDINGID)) + 
#  labs(title = "Number of WAPs detected", x = "Number of WAPs") +
#  facet_grid(FLOOR~., labeller = labeller(FLOOR = as_labeller(Floors)))


# WAPs with signal strength higher than -30 dBm
highsignal <- data %>% 
  melt(id.vars = c(521:529),
       variable.name = "WAP",
       value.name = "Signal") %>%
  filter(Signal != 100 & Signal > -30) %>% 
  select(WAP, Signal, LONGITUDE, LATITUDE, FLOOR, BUILDINGID, PHONEID)


# Rows (records) with no signal
noSignalRows <- data %>% filter_at(vars(contains("WAP")), all_vars(. == 100))

# Plot of where we can find records with no detected signal
noSignalRows %>% dplyr::mutate(Building = if_else(BUILDINGID == 0, "Building 0", 
                                 if_else(BUILDINGID == 1, "Building 1", 
                                         "Building 2"))) %>% 
  plot_ly(x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~Building,
          type = 'scatter3d', mode = 'markers') %>% 
  layout(title = "Universitat Jaume I")


# Pattern Building# 1, records with no signal
noSignalRows$TIMESTAMP <- as.POSIXct(noSignalRows$TIMESTAMP, 
                                     origin="1970-01-01", tz = "UTC")
filter(noSignalRows, BUILDINGID == 1) %>% select(LONGITUDE:TIMESTAMP) -> noSignalPattern




#--- Visualizations ----

# Plot the longitude and the latitude

ggplot(data, aes(x = LONGITUDE, y = LATITUDE)) + 
  geom_point(aes(colour = BUILDINGID)) +
  scale_colour_discrete(name = "Building") +
  labs(title = "Universitat Jaume I", x = "Longitude", y = "Latitude")


# Remove 0.5 ticks from Floor-axis
zaxis <- list(autotick = TRUE,
              tick0 = 0,
              dtick = 1)


# Plot the longitude and the latitude in 3D with floor on z-axis

# Plotly 3d
plot_ly(data, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~BUILDINGID,
        name = list("Building 0", "Building 1", "Building 2"),
        type = 'scatter3d', mode = 'markers') %>% 
  layout(title = "Universitat Jaume I", list(showticklabels = FALSE))


# Same graph with more explanatory legends
data %>% 
  dplyr::mutate(Building = if_else(BUILDINGID == 0, "Building 0", 
                              if_else(BUILDINGID == 1, "Building 1", 
                                      "Building 2"))) %>% 
  plot_ly(x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~Building,
          type = 'scatter3d', mode = 'markers') %>% 
  layout(title = "Universitat Jaume I", 
         scene = list(zaxis = zaxis),
         #annotations adds a title to the legend
         annotations = list(y = 1.05, x = 1.20, 
                                         text = "", showarrow = FALSE))

# Histogram signal strength
data %>% 
  melt(id.vars = c(521:529),
       variable.name = "WAP",
       value.name = "Signal") %>%
  filter(Signal != 100) %>% 
  
  ggplot(aes(x = Signal)) + geom_histogram(aes(y = ..density..), 
                                           binwidth = 5, colour = "white") +
  geom_density(colour = "red", linetype = "dashed")



# Histogram showing the distribution
data %>% 
  melt(id.vars = c(521:529),
       variable.name = "WAP",
       value.name = "Signal") %>%
  filter(Signal != 100) %>% 
  
  ggplot(aes(x = Signal)) + geom_histogram(aes(fill = BUILDINGID), binwidth = 5, 
                                           colour = "black", alpha = 0.7) +
  guides(fill = guide_legend(title = "Building"))


# Same as density plot
data %>% 
  melt(id.vars = c(521:529),
       variable.name = "WAP",
       value.name = "Signal") %>%
  filter(Signal != 100) %>% 
  
  ggplot(aes(x = Signal)) + geom_density(aes(fill = BUILDINGID), alpha = 0.5) + 
  guides(fill = guide_legend(title = "Building")) + 
  labs(title = "Distribution of signal strength", x = "Signal strength (dBm)") 



# Graph for signal ranges per floor and building
df <- cbind(replace(data[,1:520], data[,1:520] == 100, -105), data[,521:529]) 
df %>%
  mutate(Rowmax = apply(df[,1:520], 1, max),
         Quality = case_when(Rowmax > -55 ~ "> -55 dBm",      #Define signal range
                             Rowmax <= -55 & Rowmax >= -75 ~ "55 - 75 dBm",
                             Rowmax < -75 ~ "< -75 dBm"),
         Quality = factor(Quality, ordered = TRUE,            #Order of the range
                          levels = c("< -75 dBm","55 - 75 dBm","> -55 dBm"))) %>%
  plot_ly(x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~Quality, 
          colors = c("darksalmon", "palegoldenrod", "aquamarine3"),
          type = 'scatter3d', mode = 'markers', marker = list(size = 5)) %>% 
  layout(title = "Universitat Jaume I",
         scene = list(zaxis = zaxis),
         annotations = list(y = 1.05, x = 1.335, 
                            text = "Signal strength", showarrow = FALSE))

# ordered factors
#factor(credit_rating, ordered = TRUE, levels = c("AAA", "AA", "A", "BBB"))


#--- Visualization room access ----


## Graph of relative position, with panels for building and floor
data2 %>% 
  mutate(Floors = ifelse(FLOOR == 4, "Floor 4", 
                         ifelse(FLOOR == 3, "Floor 3",
                                ifelse(FLOOR == 2, "Floor 2",
                                       ifelse(FLOOR == 1, "Floor 1", "Floor 0")))
                         )
         ) %>% 
  
  ggplot(aes(x = RELATIVEPOSITION)) + 
  geom_bar(aes(fill = RELATIVEPOSITION)) + 
  scale_fill_discrete(name = "", label = c("Inside", "Front of the door")) + 
  labs(title = "Capture taken inside or outside a room", x = "Relative position") + 
  facet_grid(reorder(Floors, desc(Floors)) ~ BUILDINGID, 
             labeller = labeller(FLOOR = as_labeller(Floors),
                                 BUILDINGID = as_labeller(Building)))
# Information that there was little access to rooms in Building 0 and floor 4 in
# building 2.


# Proportion of captures taken inside or outside a room 
round(prop.table(table(Building = data$BUILDINGID, 
                       "Relative position" = data$RELATIVEPOSITION)), 3)

# Sum proportion of captures taken inside or outside a room
colSums(round(prop.table(table(data$BUILDINGID, data$RELATIVEPOSITION)), 3))
# 16.6% of the captures are taken inside a room, while 83.4% of the captures 
# are taken outside a room.


# Plot showing recordings across buildings and floors
data2 %>% 
  mutate(Floors = ifelse(FLOOR == 4, "Floor 4", 
                         ifelse(FLOOR == 3, "Floor 3",
                                ifelse(FLOOR == 2, "Floor 2",
                                       ifelse(FLOOR == 1, "Floor 1", "Floor 0")))
                         )
         ) %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE, colour = BUILDINGID)) + 
  geom_point() +
  labs(title = "Recordings across buildings and floors",
       x = "Longitude", y = "Latitude") +
  facet_grid(reorder(Floors, desc(Floors)) ~ BUILDINGID, 
             labeller = labeller(FLOOR = as_labeller(Floors),
                                 BUILDINGID = as_labeller(Building))) +
  theme(legend.position = "none") + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank())
# We can see that the users have not covered all areas of the building.
# For example, notice the missing corner in building 2, floor 4.



#--- Import validation data ----

# Import dataset - Validation data
validation <- read_csv("UJIndoorLoc/validationData.csv")

# Look at the structure of the data. Column 1 to 520 contain intensity value 
#  for different WAPs
str(validation[,c(1:3, 520:529)])

# Converting (floor, buildingID, spaceID, relative position, userID and 
#  phone ID) attributes to factor
validation[,c(523:528)] <- lapply(validation[, c(523:528)], as.factor)


# Duration of time period for the validation dataset
summary(as.POSIXct(validation$TIMESTAMP, origin = "1970-01-01", tz = "UTC"))
# From 19/09/2013 to 08/10/2013


# Summary of the validation dataset
summary(validation[, c(1:3, 520:529)])


# How many days is this time period
difftime(max(as.POSIXct(validation$TIMESTAMP, origin = "1970-01-01", tz = "UTC")), 
         min(as.POSIXct(validation$TIMESTAMP, origin = "1970-01-01", tz = "UTC")),
         units = "days")
# Time difference of 19.32256 days


# Density plot of the validation dataset
validation %>% 
  melt(id.vars = c(521:529),
       variable.name = "WAP",
       value.name = "Signal") %>%
  filter(Signal != 100) %>% 
  
  ggplot(aes(x = Signal)) + geom_density(aes(fill = BUILDINGID), alpha = 0.5) + 
  guides(fill = guide_legend(title = "Building")) + 
  labs(title = "Distribution of signal strength for validation dataset", 
       x = "Signal strength (dBm)")