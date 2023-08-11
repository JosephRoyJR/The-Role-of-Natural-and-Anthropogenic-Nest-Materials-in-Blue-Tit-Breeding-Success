### Data Creation
#Joseph Rot
# 27/07/2023


library(dplyr)


source("Script/Data Creation.R")

RAWbreeding <-read.csv("Data/Breeding.csv")
RAWenvironment <-read.csv("Data/Environment.csv")
RAWnest <-read.csv("Data/Nest.csv")

#selecting the columns that we need

SELbreeddata<- select(RAWbreeding, nestbox_number, 
                    clutch_size, hatchlings, 
                   fledglings, unhatched_eggs, first_egg_date)

SELenvironment<- select(RAWenvironment, lat, long,
                        nestbox_number, IS50, TD50, IS100, TD100, 
                        IS500, TD500, HPD500)

SELnest<- select(RAWnest, ID, Site, Habitat, Date, Total, Anthro, 
                 Moss, Feather, Wool.Hair, Arthropod)


#renaming the columns

colnames(SELbreeddata)[colnames(SELbreeddata) == 
                         "nestbox_number"] <- "ID"

colnames(SELenvironment)[colnames(SELenvironment) == 
                           "nestbox_number"] <- "ID"

colnames(SELnest)[colnames(SELnest) == 
                    "Date"] <- "Nest_Collection_Date"

#removing repeated rows from SELenvironment

SELenvironment <- SELenvironment[!duplicated(SELenvironment$ID), ]



#merging the data

BreedNest <- merge(SELbreeddata, 
                   SELnest, by = "ID", all = F)

BreedNestEnv <- merge(BreedNest, 
                      SELenvironment, by = "ID", all = F)

#removing the rows with NA values

BreedNestEnv <- na.omit(BreedNestEnv)

#saving the data as a csv file

write.csv(BreedNestEnv, "Data/BreedNestEnv.csv", row.names = FALSE)


view(BreedNestEnv)
```
