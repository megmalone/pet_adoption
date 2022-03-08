library(caret)
library(ggplot2)

summary(dogs)

# I want to try to build a model to predict the duration of a dogs stay before adoption.
# First steps:
# 1. Select only items where the outcome type is adoption and outcome subtype is not fostering (because that is by definition temporary).
# 2. Select only the first time each dog is up for adoption as later adoptions will have higher durations and less accurately reflect the 
#    public's interest in adopting the dog (the dog's "adoptability"), which is what we're investigating. We'll come back to instances later.

dogs$`Outcome Subtype` <- dogs$`Outcome Subtype` %>%
  replace_na('NA')

data <- dogs %>%
  filter(Outcome_Type == 'Adoption') %>%
  filter(`Outcome Subtype` != 'Foster') %>%
  filter(Visit == 1)

# Selecting variables to consider for modeling:
data <- data %>%
  select(c(
    Animal_ID,
    `Intake Type`,
    `Intake Condition`,
    Breed,
    Second_Breed,
    Color,
    Mix,
    Classification,
    obey,
    Outcome_SpayNeut,
    Sex,
    OutcomeAge_Years,
    Duration
  ))

# Convert to integer:
data$Duration <- as.numeric(data$Duration, 'days')

ggplot(data, aes(Duration)) +
  geom_bar() # Super skewed! 

# Outlier treatment:
ggplot(data, aes(Duration)) + 
  geom_boxplot() # Visualization confirms there are signficiant outliers

outliers <-
  boxplot.stats(data$Duration)$out # Identifies values outside of 1.5 * inter quartile range (distance between 75th and 25th)
'%!in%' <- Negate('%in%')
data <- data %>%
  filter(Duration %!in% outliers) # Removes these outliers

ggplot(data, aes(Duration)) + 
  geom_boxplot() # Visualization confirms most outliers have been removed
ggplot(data, aes(Duration)) +
  geom_bar() # Still looks very skewed
qqnorm(data$Duration, pch = 1, frame = FALSE)
qqline(data$Duration, col = 'red', lwd = 2) # Appears very abnormal

quantile(data$Duration)
# 0%  25%  50%  75% 100% 
# 1    4    6   13   67 

# I'm going to handle this by binning the duration variable.
data <- data %>%
  mutate(duration_bin = cut(Duration, breaks = c(0, 7, 14, 21, 28, 35, 42, 70))) %>%
  select(-Duration)
table(data$duration_bin)

# Exploring feature variables:
table(data$`Intake Type`)
# Removing 'Euthanasia Request' as only 1 instance
data <- data %>%
  filter(`Intake Type` != 'Euthanasia Request')

table(data$`Intake Condition`)
# I'm going to combine some of these levels as well.
data$Intake_Cond_bin <-
        ifelse(data$`Intake Condition` == 'Aged' | data$`Intake Condition` == 'Injured' | data$`Intake Condition` == 'Med Attn' | data$`Intake Condition` == 'Medical' | data$`Intake Condition` == 'Sick' | data$`Intake Condition` == 'Neonatal' | data$`Intake Condition` == 'Nursing' | data$`Intake Condition` == 'Pregnant', 'Sick/Injured/Pregnant/Nursing/Neonatal/Age',
                              ifelse(data$`Intake Condition` == 'Normal', 'Normal', 'Other'))
table(data$Intake_Cond_bin)
data <- data %>%
  select(-`Intake Condition`)

table(data$Breed)
# I'm going to pull out top 10 and bin others.
table(data$Breed) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))
# Var1 Freq
# 1                   Labrador Retriever 3503
# 2                            Chihuahua 3374
# 3       American Staffordshire Terrier 2761
# 4                      German Shepherd 1851
# 5                Australian Cattle Dog  999
# 6                            Dachshund  850
# 7                        Border Collie  547
# 8                       Siberian Husky  527
# 9                                Boxer  427
# 10                              Poodle  389


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
data <- data %>%
  select(-Breed)

table(data$Color) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))
# Taking a similar approach, keeping top 10 but also adjusting for ordering:
data$Color_bin <-
  ifelse(data$Color == 'Black/White' | data$Color == 'White/Black', 'Black/White',
         ifelse(data$Color == 'Brown/White' | data$Color == 'White/Brown', 'Brown/White',
                ifelse(data$Color == 'Tan/White' | data$Color == 'White/Tan', 'Tan/White',
                       ifelse(data$Color == 'Black', 'Black',
                              ifelse(data$Color == 'White', 'White',
                                     ifelse(data$Color == 'Tan', 'Tan',
                                            ifelse(data$Color == 'Black/Tan' | data$Color == 'Tan/Black', 'Black/Tan',
                                                   ifelse(data$Color == 'Tricolor', 'Tricolor',
                                                          ifelse(data$Color == 'Black/Brown' | data$Color == 'BrownBlack', 'Black/Brown',
                                                                 ifelse(data$Color == 'Brown', 'Brown', 'Other'))))))))))
table(data$Color_bin)
data <- data %>%
  select(-Color)

# Selection of final data points I want to work with:
data <- data %>%
  select(-c(
    `Intake Type`,
    Second_Breed
  ))

data$Classification <- as.character(data$Classification)
data$Classification <- data$Classification %>% replace_na('Unknown')
table(data$Classification)          

data$obey_missing <- ifelse(is.na(data$obey), 1, 0) # Creating a flag
median <- data %>%
  select(obey, obey_missing) %>%
  filter(obey_missing == 0)
median_val <- median(median$obey)  
data$obey[is.na(data$obey)] <- median_val # Imputing the median
