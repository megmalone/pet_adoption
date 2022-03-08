library(caret)
library(ggplot2)

summary(dogs)

# I want to try to build a model to predict the duration of a dogs stay before adoption.
# First steps:
# 1. Select only items where the outcome type is adoption and outcome subtype is not fostering (because that is by definition temporary).

dogs$`Outcome Subtype` <- dogs$`Outcome Subtype` %>%
  replace_na('NA')

data <- dogs %>%
  filter(Outcome_Type == 'Adoption') %>%
  filter(`Outcome Subtype` != 'Foster') %>%
  filter(Visit == 1)

# Convert duration to integer:
data$visit_duration <- as.numeric(data$visit_duration, 'days')

ggplot(data, aes(visit_duration)) +
  geom_bar() # Super skewed! 

# Outlier treatment:
ggplot(data, aes(visit_duration)) + 
  geom_boxplot() # Visualization confirms there are signficiant outliers

outliers <-
  boxplot.stats(data$visit_duration)$out # Identifies values outside of 1.5 * inter quartile range (distance between 75th and 25th)
'%!in%' <- Negate('%in%')
data <- data %>%
  filter(visit_duration %!in% outliers) # Removes these outliers

ggplot(data, aes(visit_duration)) + 
  geom_boxplot() # Visualization confirms most outliers have been removed
ggplot(data, aes(visit_duration)) +
  geom_bar() # Still looks very skewed
qqnorm(data$visit_duration, pch = 1, frame = FALSE)
qqline(data$visit_duration, col = 'red', lwd = 2) # Appears very abnormal

quantile(data$visit_duration)
# 0%  25%  50%  75% 100% 
# 1    4    6    9   29 

# I'm going to handle this by binning the duration variable.
data <- data %>%
  mutate(duration_bin = cut(visit_duration, breaks = c(0, 3, 7, 14, 21, 28, 35)))
table(data$duration_bin)

# Exploring feature variables:
table(data$`Intake Type`)

table(data$`Intake Condition`)
# I'm going to combine some of these levels as well.
data$Intake_Cond_bin <-
        ifelse(data$`Intake Condition` == 'Aged' | data$`Intake Condition` == 'Injured' | data$`Intake Condition` == 'Med Attn' | data$`Intake Condition` == 'Medical' | data$`Intake Condition` == 'Sick' | data$`Intake Condition` == 'Neonatal' | data$`Intake Condition` == 'Nursing' | data$`Intake Condition` == 'Pregnant', 'Sick_Injured_Pregnant_Nursing_Neonatal_Age',
                              ifelse(data$`Intake Condition` == 'Normal', 'Normal', 'Other'))
table(data$Intake_Cond_bin)

table(data$Breed)
# I'm going to pull out top 10 and bin others.
table(data$Breed) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))
# 1                            Chihuahua 3456
# 2                   Labrador Retriever 3447
# 3       American Staffordshire Terrier 2397
# 4                      German Shepherd 1799
# 5                Australian Cattle Dog  974
# 6                            Dachshund  865
# 7                        Border Collie  563
# 8                       Siberian Husky  544
# 9                                Boxer  418
# 10                              Poodle  409



data$Breed_bin <- ifelse(data$Breed == 'Labrador Retriever', 'Labrador Retriever',
                         ifelse(data$Breed == 'Chihuahua', 'Chihuahua',
                                ifelse(data$Breed == 'American Staffordshire Terrier', 'American Staffordshire Terrier',
                                       ifelse(data$Breed == 'German Sheperd', 'German Sheperd',
                                              ifelse(data$Breed == 'Australian Cattle Dog', 'Australian Cattle Dog',
                                                     ifelse(data$Breed == 'Dachshund', 'Dachshund',
                                                            ifelse(data$Breed == 'Border Collie', 'Border Collie',
                                                                   ifelse(data$Breed == 'Siberian Husky', 'Siberian Husky',
                                                                          ifelse(data$Breed == 'Boxer', 'Boxer',
                                                                                 ifelse(data$Breed == 'Poodle', 'Poodle', 'Other'))))))))))
table(data$Breed_bin)

table(data$Color) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))
# Taking a similar approach, keeping top 10 but also adjusting for ordering:
data$Color_bin <-
  ifelse(data$Color == 'Black/White' | data$Color == 'White/Black', 'Black_White',
         ifelse(data$Color == 'Brown/White' | data$Color == 'White/Brown', 'Brown_White',
                ifelse(data$Color == 'Tan/White' | data$Color == 'White/Tan', 'Tan_White',
                       ifelse(data$Color == 'Black', 'Black',
                              ifelse(data$Color == 'White', 'White',
                                     ifelse(data$Color == 'Tan', 'Tan',
                                            ifelse(data$Color == 'Black/Tan' | data$Color == 'Tan/Black', 'Black_Tan',
                                                   ifelse(data$Color == 'Tricolor', 'Tricolor',
                                                          ifelse(data$Color == 'Black/Brown' | data$Color == 'BrownBlack', 'Black_Brown',
                                                                 ifelse(data$Color == 'Brown', 'Brown', 'Other'))))))))))
table(data$Color_bin)

# Selection of final data points I want to work with:
ds <- data %>%
  rename(c(
    Intake_Type = `Intake Type`,
    Intake_Cond = `Intake Condition`
  ))

ds <- ds %>%
  select(-c(
    Animal_ID,
    Intake_Date,
    Breed,
    Second_Breed,
    Outcome_Date,
    `Outcome Subtype`,
    Income_SpayNeut,
    IntakeAge_Years,
    Intake_Cond,
    Color,
    Outcome_Type,
    visit_duration,
    Classification
  ))

colnames(ds)

# Imputing missing obey values:
ds$obey_missing <- ifelse(is.na(ds$obey), 1, 0) # Creating a flag
median <- ds %>%
  select(obey, obey_missing) %>%
  filter(obey_missing == 0)
median_val <- median(median$obey)  
ds$obey[is.na(ds$obey)] <- median_val # Imputing the median

ds$reps_missing <- ifelse(is.na(ds$reps_lower), 1, 0) # Creating a flag
median <- ds %>%
  select(reps_lower, reps_missing) %>%
  filter(reps_missing == 0)
median_val <- median(median$reps_lower)  
ds$reps_lower[is.na(ds$reps_lower)] <- median_val # Imputing the median

median <- ds %>%
  select(reps_upper, reps_missing) %>%
  filter(reps_missing == 0)
median_val <- median(median$reps_upper)  
ds$reps_upper[is.na(ds$reps_upper)] <- median_val # Imputing the median
