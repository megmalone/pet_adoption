library(readr)
library(dplyr)
library(readxl)

intelligence <-
  read.csv("~/GitHub/pet_adoption/dog_intelligence.csv")
# Source: https://data.world/len/dog-size-intelligence-linked / https://en.m.wikipedia.org/wiki/The_Intelligence_of_Dogs#cite_ref-ReferenceA_18-0
# Dictionary:
# obey: probability that the breed obeys the first command (figure is lower bound)
# reps_lower: lower limit of repetitions to understand new commands
# reps_upper: upper limit of repetitions to understand new commands

# breed <- read.csv("~/GitHub/pet_adoption/AKC Breed Info.csv")
# Source: https://data.world/len/dog-size-intelligence-linked / http://www.petplace.com/article/dogs/selecting-a-dog/adopting-or-buying-a-dog/american-kennel-club-akc-breeds-by-size

intakes <- read_excel("Austin_Animal_Center_Intakes.xlsx")
# Source: https://data.austintexas.gov/Health-and-Community-Services/Austin-Animal-Center-Intakes/wter-evkm

outcomes <- read_excel("Austin_Animal_Center_Outcomes.xlsx")
# Source: https://data.austintexas.gov/Health-and-Community-Services/Austin-Animal-Center-Outcomes/9t4d-g238

# colnames(intakes)
# colnames(outcomes) # Primary Key is Animal ID

# Renaming the DateTime column in both datasets to distinguish
intakes <- rename(intakes, Intake_DateTime = DateTime)
outcomes <- rename(outcomes, Outcome_DateTime = DateTime)

intakes <- distinct(intakes)
outcomes <- distinct(outcomes)

animals <- intakes %>%
  left_join(outcomes, by = c('Animal ID' = 'Animal ID'))
# Why are there more rows in the combined dataset? Some animals were found and then returned/adopted more than once.

colnames(animals)

# Removing duplicate information
animals <- animals %>%
  select(-c(
    'Animal Type.y',
    'Breed.y',
    'Color.y',
    'Name.y',
    'MonthYear.x',
    'MonthYear.y'
  ))

animals <-
  rename(animals,
         c(
           Name = Name.x,
           Breed = Breed.x,
           Color = Color.x,
           Animal_Type = 'Animal Type.x'
         ))
