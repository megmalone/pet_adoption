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

# There are other issues with the intake and outcome dates.
# > Sometimes the same intake date is repeated for numerous outcome dates, indicating it isn't updated when the dog is returned.
# > Sometimes the same outcome date is repeated for different intake dates, indicating that the intake date was updated and outcome
#   dates were overridden.
# Here is how I'm going to address these problems:
# > Group by Animal ID, arrange by intake date then outcome date, and take the earliest outcome date.
# > This will leave me with one row per animal's visit to the shelter(s).
# > If an animal has been there more than once, we'll have more than one row, but we will be able to see how long they were there
#   each time before being adopted/reclaimed/transfered.

# Creating a copy before additional manipulation:
date_dogs <- joined_dogs

date_dogs <- date_dogs %>%
  group_by(Animal_ID, Intake_Date) %>%
  arrange(Intake_Date, Outcome_Date) %>%
  slice(1)

# A new copy for remaining steps:
dogs_filt <- date_dogs

# Creating a new variable to capture age in years:
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
# Removing the negative value, which is not possible at outcome:
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

# Creating an index count by animal to track order and number of visits:
dogs <- dogs %>% 
  group_by(Animal_ID) %>%
  arrange(Intake_Date, Outcome_Date) %>%
  mutate(Visit = row_number(Animal_ID))

# Counting visits per dog as visits:
visits_table <- dogs %>%
  count(Animal_ID, sort = TRUE, name = "Visit")

dogs <- dogs %>%
  left_join(visits_table, by = c('Animal_ID' = 'Animal_ID'))
  
# Creating a variable for duration of stay:
dogs$visit_duration <- days(dogs$Outcome_Date-dogs$Intake_Date)
summary(dogs$visit_duration)

# dur <- dogs %>%
#   group_by(Animal_ID) %>%
#   arrange(visit_duration)
  
# Converting obey variable to an integer:
dogs$obey <- gsub('%', '', dogs$obey)
dogs$obey <- as.numeric(dogs$obey)

# Clearing 'Unknown or uninitialised column' warning message: https://stackoverflow.com/questions/39041115/fixing-a-multiple-warning-unknown-column
dogs <- dogs %>%
  ungroup()

# write.csv(dogs, 'dogs.csv') # Exporting data for visualization
