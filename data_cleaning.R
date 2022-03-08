# Creating a copy for additional manipulation:
joined_dogs <- dogs_int

# Split DateTime variables and convert w/ lubridate:
joined_dogs <- joined_dogs %>%
  separate(Intake_DateTime, c('Intake_Date', NA), sep = ' ', extra = 'drop')
joined_dogs$Intake_Date <- ymd(joined_dogs$Intake_Date)

joined_dogs<- joined_dogs %>%
  separate(Outcome_DateTime, c('Outcome_Date', NA), sep = ' ', extra = 'drop')
joined_dogs$Outcome_Date <- ymd(joined_dogs$Outcome_Date)

joined_dogs <- joined_dogs %>%
  rename(Animal_ID = 'Animal ID')

# Adding a spayed/neutered binary indicator:
joined_dogs$Income_SpayNeut <- ifelse(grepl('Spayed', joined_dogs$`Sex upon Intake`) | grepl('Neutered', joined_dogs$`Sex upon Intake`), 1, 0)
joined_dogs$Outcome_SpayNeut <- ifelse(grepl('Spayed', joined_dogs$`Sex upon Outcome`) | grepl('Neutered', joined_dogs$`Sex upon Outcome`), 1, 0)

# Now creating a single variable for sex and dropping the other intake/outcome columns:
joined_dogs$Sex <- ifelse(grepl('Female', joined_dogs$`Sex upon Intake`), 'Female', 'Male')

joined_dogs <- joined_dogs %>%
  select(-c(
    'Sex upon Intake',
    'Sex upon Outcome'
  ))

# Remove instances where the outcome date preceeds the intake date, assuming data entry issue:
joined_dogs <- joined_dogs %>%
  filter(Outcome_Date > Intake_Date)

# Creating a copy for additional manipulation:
dogs_filt <- joined_dogs

dogs_filt <- dogs_filt %>%
  separate(`Age upon Intake`, c('IntakeAge', 'IntakeAge_Unit'), sep = ' ', extra = 'drop', fill = 'right') 
dogs_filt$IntakeAge <- as.numeric(dogs_filt$IntakeAge)

age_list <- c()
for (i in 1:nrow(dogs_filt)) {
  if (dogs_filt$IntakeAge_Unit[(i)] == 'month') {
    val <- dogs_filt$IntakeAge[(i)]/12
    age_list <- append(age_list, val)
  } else if (dogs_filt$IntakeAge_Unit[(i)] == 'months') {
    val <- dogs_filt$IntakeAge[(i)]/12
    age_list <- append(age_list, val)
  } else if (dogs_filt$IntakeAge_Unit[(i)] == 'weeks') {
    val <- dogs_filt$IntakeAge[(i)]/52
    age_list <- append(age_list, val)
  } else if (dogs_filt$IntakeAge_Unit[(i)] == 'week') {
    val <- 7/365
    age_list <- append(age_list, val)
  } else if (dogs_filt$IntakeAge_Unit[(i)] == 'days') {
    val <- dogs_filt$IntakeAge[(i)]/365
    age_list <- append(age_list, val)
  } else if (dogs_filt$IntakeAge_Unit[(i)] == 'day') {
    val <- 1
    age_list <- append(age_list, val)
  } else if (dogs_filt$IntakeAge_Unit[(i)] == 'year') {
    val <- 1
    age_list <- append(age_list, val)
  } else if (dogs_filt$IntakeAge_Unit[(i)] == 'years') {
    val <- dogs_filt$IntakeAge[(i)]
    age_list <- append(age_list, val)
  } else {
    val <- NA
    age_list <- append(age_list, val)
  }
}
dogs_filt$IntakeAge_Years <- age_list
# summary(dogs_filt$IntakeAge_Years)

dogs_filt <- dogs_filt %>%
  separate(`Age upon Outcome`, c('OutcomeAge', 'OutcomeAge_Unit'), sep = ' ', extra = 'drop', fill = 'right') 
dogs_filt$OutcomeAge <- as.numeric(dogs_filt$OutcomeAge)
# table(dogs_filt$OutcomeAge)
# Removing the negative value, which is not possible:
dogs_filt <- dogs_filt %>%
  filter(OutcomeAge > 0)

age_list <- c()
for (i in 1:nrow(dogs_filt)) {
  if (dogs_filt$OutcomeAge_Unit[(i)] == 'month') {
    val <- dogs_filt$OutcomeAge[(i)]/12
    age_list <- append(age_list, val)
  } else if (dogs_filt$OutcomeAge_Unit[(i)] == 'months') {
    val <- dogs_filt$OutcomeAge[(i)]/12
    age_list <- append(age_list, val)
  } else if (dogs_filt$OutcomeAge_Unit[(i)] == 'weeks') {
    val <- dogs_filt$OutcomeAge[(i)]/52
    age_list <- append(age_list, val)
  } else if (dogs_filt$OutcomeAge_Unit[(i)] == 'week') {
    val <- 7/365
    age_list <- append(age_list, val)
  } else if (dogs_filt$OutcomeAge_Unit[(i)] == 'days') {
    val <- dogs_filt$OutcomeAge[(i)]/365
    age_list <- append(age_list, val)
  } else if (dogs_filt$OutcomeAge_Unit[(i)] == 'day') {
    val <- 1
    age_list <- append(age_list, val)
  } else if (dogs_filt$OutcomeAge_Unit[(i)] == 'year') {
    val <- 1
    age_list <- append(age_list, val)
  } else if (dogs_filt$OutcomeAge_Unit[(i)] == 'years') {
    val <- dogs_filt$OutcomeAge[(i)]
    age_list <- append(age_list, val)
  } else {
    val <- NA
    age_list <- append(age_list, val)
  }
}
dogs_filt$OutcomeAge_Years <- age_list
# summary(dogs_filt$OutcomeAge_Years)

# Removing now duplicate age columns:
dogs_filt <- dogs_filt %>%
  select(-c(
    'IntakeAge',
    'IntakeAge_Unit',
    'OutcomeAge',
    'OutcomeAge_Unit',
    'Date of Birth'
  ))

# One last copy for final changes:
dogs <- dogs_filt

# Creating an index counted by animal to track order of visits:
# Ordering by min(Intake_Date) and min(Outcome_Date) as there are data issues!
dogs <- dogs %>% 
  group_by(Animal_ID) %>%
  arrange(Intake_Date, Outcome_Date) %>%
  mutate(Visit = row_number(Animal_ID))

# Counting visits per dog as Instances:
Instances_table <- dogs %>%
  count(Animal_ID, sort = TRUE, name = "Instances")

dogs <- dogs %>%
  left_join(Instances_table, by = c('Animal_ID' = 'Animal_ID'))

# I'm noticing duplicate intake/outcome rows per Animal ID.
dups <- dogs %>%
  select(c(Animal_ID,
           Intake_Date,
           Outcome_Date,
           Outcome_Type,
           'Outcome Subtype',
           Instances,
           Visit)) %>%
  group_by(Animal_ID) %>%
  arrange(Intake_Date, Outcome_Date) %>%
  filter(Instances > 2)

# Looks like intake date is not consistently readjusted if a dog is returned to the shelter.
# To circumnavigate this, I'm going to create new variables with min(Intake_Date) and max(Outcome_Date) to more
# accurately reflect the dog's entire time in "the system." The instances variable will still indictate over the course
# of the dog's stay it was in/out. I'm going to use these new variables to calculate duration.

dogs <- dogs %>%
  group_by(Animal_ID) %>%
  mutate(Earl_Intake_Date = min(Intake_Date)) %>%
  mutate(Lat_Outcome_Date = max(Outcome_Date))
  
# Creating a variable for duration of stay:
dogs$Duration <- days(dogs$Lat_Outcome_Date-dogs$Earl_Intake_Date)
summary(dogs$Duration)
  