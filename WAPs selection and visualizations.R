################################################################
#IoT Analytics
#Evaluate Techniques for WiFi Locationing 

#Selection of WAPs to include in training models and 
#visualizations

#Created by Eirik Espe
################################################################

source("Models for floor - per building.R")


# Pick WAPs with signal between -70 and -30 dBm for building 0
WAPsHighSignalB0 <- cbind(Building0[,1:465] %>% 
                            select_if(~ any(. %in% -70:-30)), 
                          Building0[,466:474])

# Pick WAPs with signal between -70 and -30 dBm for building 1
WAPsHighSignalB1 <- cbind(Building1[,1:465] %>% 
                            select_if(~ any(. %in% -70:-30)), 
                          Building1[,466:474])

# Pick WAPs with signal between -70 and -30 dBm for building 2
WAPsHighSignalB2 <- cbind(Building2[,1:465] %>% 
                            select_if(~ any(.x %in% -70:-30)), 
                          Building2[,466:474])


# Shared WAPs building 0 and building 2
intersect(WAPsHighSignalB0 %>% select(starts_with("WAP")) %>% names(), 
          WAPsHighSignalB2 %>% select(starts_with("WAP")) %>% names())    # 1 WAP
# WAP248


# Shared WAPs building 0 and building 1
intersect(WAPsHighSignalB0 %>% select(starts_with("WAP")) %>% names(), 
          WAPsHighSignalB1 %>% select(starts_with("WAP")) %>% names())    # 6 WAPs
# WAP008, WAP046, WAP151, WAP184, WAP185, WAP248


# Shared WAPs building 1 and building 2
intersect(WAPsHighSignalB1 %>% select(starts_with("WAP")) %>% names(), 
          WAPsHighSignalB2 %>% select(starts_with("WAP")) %>% names())    # 16 WAPs
# WAP082, WAP083, WAP084, WAP085, WAP113, WAP114, WAP175, WAP180
# WAP181, WAP186, WAP187, WAP248, WAP286, WAP398, WAP478, WAP503




# Looking at summary statistics to find which building the WAPs most likely belong to
# (looking at median, mean, 3rd quartile and max)

# Summary data for the shared WAPs between building 0 and 2
WAPsHighSignalB0 %>% select(WAP248) %>% summary
WAPsHighSignalB1 %>% select(WAP248) %>% summary

# Summary data for the shared WAPs between building 0 and 1
WAPsHighSignalB0 %>% select(WAP008, WAP046, WAP151, WAP184, WAP185, WAP248) %>% summary
WAPsHighSignalB1 %>% select(WAP008, WAP046, WAP151, WAP184, WAP185, WAP248) %>% summary

# Summary data for the shared WAPs between building 1 and 2
WAPsHighSignalB1 %>% 
  select(WAP082, WAP083, WAP084, WAP085, WAP113, WAP114, WAP175, WAP180,
         WAP181, WAP186, WAP187, WAP248, WAP286, WAP398, WAP478, WAP503) %>% summary
WAPsHighSignalB2 %>% 
  select(WAP082, WAP083, WAP084, WAP085, WAP113, WAP114, WAP175, WAP180, 
         WAP181, WAP186, WAP187, WAP248, WAP286, WAP398, WAP478, WAP503) %>% summary



#--- Visualizations ----

# Labels for building to use in facet_grid below
Building <- c("0" = "Building 0",
              "1" = "Building 1", 
              "2" = "Building 2")

# Labels for floor to insert in facet_grid
Floors <- c("0" = "Floor 0",
            "1" = "Floor 1",
            "2" = "Floor 2",
            "3" = "Floor 3",
            "4" = "Floor 4")

# Graph of relative position, with panels for building and floor
data2 %>% ggplot(aes(x = RELATIVEPOSITION)) + 
  geom_bar(aes(fill = RELATIVEPOSITION)) + 
  scale_fill_discrete(name = "", label = c("Inside", "Front of the door")) + 
  labs(x = "Relative position") + 
  facet_grid(FLOOR ~ BUILDINGID, 
             labeller = labeller(FLOOR = as_labeller(Floors),
                                 BUILDINGID = as_labeller(Building)))


# Same graph with Floor 0 at the bottom
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
  


# Graph of signal strength distribution for different phones in Building 1
Building1 %>% melt(id.vars = c(465:474), 
                   variable.name = "WAP", 
                   value.name = "Signal") %>%
  filter(Signal != -105) %>% 
  
  ggplot(aes(x = Signal)) + geom_density(aes(fill = PHONEID), alpha = 0.5) + 
  guides(fill = guide_legend(title = "Phone ID")) + 
  labs(x = "Signal strength (dBm)")




