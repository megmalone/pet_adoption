library(caret)
library(ggplot2)
library(MASS)
library(dplyr)

ds <- data # Making a copy of the cleaned dataset for modeling 

features <- ds %>%
  select(-c(Animal_ID,
            duration_bin))

# Creating dummy variables for all factors:
dummy <- dummyVars(" ~.", features)
features <- as.data.frame(predict(dummy, features))
features <- features %>%
  select(-SexMale)

# Checking the variance of variables to ensure I don't have any potential complete or partial seperation issues:
nzv <-
  nearZeroVar(features, saveMetrics = TRUE)

# Checking correlation between variables:
corvars <-  cor(features)
highcor <- sum(abs(corvars[upper.tri(features)]) > .999) # None!

target <- ds %>% select(duration_bin)
table(target$duration_target)
target$duration_target <- ifelse(target$duration_bin == '(0,7]', 1,
                 ifelse(target$duration_bin == '(7,14]', 2,
                        ifelse(target$duration_bin == '(14,21]', 3,
                               ifelse(target$duration_bin == '(21,28]', 4,
                                      ifelse(target$duration_bin == '(28,35]', 5,
                                             ifelse(target$duration_bin == '(35,42]', 6, 7))))))

model_data <- cbind(features, as.ordered(target$duration_target))
model_data <- model_data %>%
  rename(target = 'as.ordered(target$duration_target)')

# Splitting the data into training and testing groups:
set.seed(733)
train_index <- createDataPartition(model_data$target,
                                   p = 0.7,
                                   list = FALSE)
train <- as.data.frame(model_data[train_index, ])
test <- as.data.frame(model_data[-train_index, ])



clogit.model <- polr(factor(target) ~ ., 
                     method = "logistic", data = train)
summary(clogit.model)

brant(clogit.model)

# Partial Proportional Odds #
plogit.model <- vglm(factor(wallet) ~ male + business + punish + explain,
                     data = train, family = cumulative(parallel = F ~ business))
summary(plogit.model)

# Odds Ratios #
ORtable <- data.frame(OR = exp(coef(clogit.model)),
                      lower = exp(confint(clogit.model))[,1],
                      upper = exp(confint(clogit.model))[,2])
print(ORtable)

# Predicted Probabilities #
pred_probs <- predict(clogit.model, newdata = train, type = "probs")
print(pred_probs)
