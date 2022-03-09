library(caret)
library(ggplot2)
library(MASS)
library(dplyr)
library(brant)

features <- ds %>%
  select(-duration_bin)

# Creating dummy variables for all factors:
dummy <- dummyVars(" ~.", features)
features <- as.data.frame(predict(dummy, features))

colnames(features)

features <- features %>%
  select(-SexMale)

# Checking the variance of variables to ensure I don't have any potential complete or partial seperation issues:
nzv <-
  nearZeroVar(features, saveMetrics = TRUE)
# Visit has zero variance, so removing it from consideration.
# Several variables have nzv: 

features <- features %>%
  select(-c(
    Intake_TypeAbandoned,
    `Intake_TypePublic Assist`,
    Outcome_SpayNeut,
    Intake_Cond_binNormal,
    Intake_Cond_binSick_Injured_Pregnant_Nursing_Neonatal_Age,
    `Breed_binAustralian Cattle Dog`,
    `Breed_binBorder Collie`,
    Breed_binBoxer,
    Breed_binDachshund,
    Breed_binPoodle,
    `Breed_binSiberian Husky`,
    Color_binBlack,
    Color_binBlack_Brown,
    Color_binBrown,
    Color_binTan,
    Color_binTricolor,
    Color_binWhite
  ))

# Checking correlation between variables:
corvars <-  cor(features)
highcor <- sum(abs(corvars[upper.tri(features)]) > .999)
# Obviously a high correlation between upper and lower reps, I'm going to drop upper reps.

features <- features %>%
  select(-c(reps_upper,
            reps_missing,
            obey_missing))

target <- ds %>% select(duration_bin)
table(target$duration_bin)
target$duration_target <- ifelse(target$duration_bin == '(0,3]', 1,
                 ifelse(target$duration_bin == '(3,7]', 2,
                        ifelse(target$duration_bin == '(7,14]', 3,
                               ifelse(target$duration_bin == '(14,21]', 4,
                                      ifelse(target$duration_bin == '(21,28]', 5,
                                             ifelse(target$duration_bin == '(28,35]', 6, 7))))))

model_data <- cbind(features, as.ordered(target$duration_target))
model_data <- model_data %>%
  rename(c(target = 'as.ordered(target$duration_target)',
           Surr = 'Intake_TypeOwner Surrender',
           Staff = "Breed_binAmerican Staffordshire Terrier",
           Lab = "Breed_binLabrador Retriever"
           ))

# Splitting the data into training and testing groups:
set.seed(733)
train_index <- createDataPartition(model_data$target,
                                   p = 0.7,
                                   list = FALSE)
train <- as.data.frame(model_data[train_index, ])
test <- as.data.frame(model_data[-train_index, ])

# Ordinal Logistic Regression:
clogit.model <- polr(target ~ .,
                     Hess = T,
                     data = train)

summary(clogit.model)
# Need to test to see if the slopes are statistically different from each other in the proportional odds model:
brant(clogit.model)
# H0: Parallel Regression Assumption holds

# p-values:
ctable <- coef(summary(clogit.model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
# All p-values are < 0.05

# Odds Ratios:
ORtable <- data.frame(OR = exp(coef(clogit.model)),
                      lower = exp(confint(clogit.model))[,1],
                      upper = exp(confint(clogit.model))[,2])
print(ORtable)

# Predicted Probabilities:
pred_probs <- predict(clogit.model, newdata = train, type = "probs")
print(pred_probs)

# Testing on hold-out data:
predict <- predict(clogit.model, test)
table(test$target, predict)
mean(as.character(test$target) != as.character(predict)) # Only 43% accuracy, not great.

# What would I do next to potentially improve this model?
# > Consider an ensemble model.
# > Review distribution across factors.
# > Try a more complex classification model (e.g. random forest)