if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(pander)) install.packages("pander", repos = "http://cran.us.r-project.org")
if(!require(lares)) install.packages("lares", repos = "http://cran.us.r-project.org")

#######################################################################
# This Rs script takes about 20 minutes to run. The computationally 
# expensive parts are the grid tunes of the caret::XGBoost algorithms that
# can be commented out to reduce run time (lines 581-758). The optimal values 
# are set in the parameters for the XGBoost::XGBoost model.
#######################################################################

#######################################################################
# Data Preparation
#######################################################################

# Download and unzip the raw data from github
dl <- tempfile()
download.file("https://github.com/hjwestbye/capstone2/raw/main/data_raw.zip", dl)


nfdata_raw <- fread(unzip(dl, "data_raw.csv"), 
                    col.names = c("year", "patId", "age", "sex", 
                                  "session", "itemName", "rating", 
                                  "question", "n_sessions"))

# Inspect the raw data
dim(nfdata_raw)
str(nfdata_raw)

# Table of the data without question column
knitr::kable(nfdata_raw[1:10,-8])

# Unique patients in dataset
length(unique((nfdata_raw$patId)))

# Number of items
length(unique((nfdata_raw$itemName)))

# Create item dataframe
item_df <- nfdata_raw %>% distinct(itemName, .keep_all = TRUE) %>% select(itemName, question)

# Extract question text
item_df[1,]
question_list <- item_df$question %>% str_extract_all("^(.+)\\.")

# Convert questions from list to dataframe
question_df <- data.frame(t(data.frame(t(sapply(question_list,c)))))

# Update item dataframe with question texts
item_df <- item_df %>% mutate(question = question_df[,1]) %>% 
  mutate(itemId = row_number()) %>%
  select(itemId, itemName, question)

# Convert itemId to factor
item_df$itemId <- as.factor(item_df$itemId)  

# Add missing questions manually
item_df[2,3] <- "I would like my therapist to use less/more techniques and exersises"
item_df[3,3] <- "I feel that my therapist accepts me as a person"
item_df[18,3] <- "I feel that the therapist understands me and understands why I am in treatment now"
item_df[19,3] <- "I would like my therapist to focus less/more on the relationship between us"
item_df[21,3] <- "I would like my therapist to show their personality more/be more formal"
item_df[22,3] <- "I have an understanding of how treatment is going to help me"
item_df[23,3] <- "I would like my therapist to focus more on my feelings / more on my cognitions"
item_df[25,3] <- "Now I understand what I need to do or work on to get better"

head(item_df, 30)

# Create nfdata dataframe
nfdata <- nfdata_raw %>% left_join(item_df, by = "itemName") %>% 
  select(patId, age, sex, session, itemId, rating)

# Create patient dataframe
patient_df <- nfdata_raw %>% distinct(patId, .keep_all = TRUE) %>% 
  mutate(patN = row_number()) %>% 
  select(patN, patId, age, sex, n_sessions)

# Rank sessions
session_ranked <- nfdata_raw %>% 
  group_by(patId) %>%
  distinct(session, .keep_all = TRUE) %>%
  mutate(session_rank = order(order(session))) %>% 
  select(patId, session, session_rank) %>%
  arrange(patId) %>%
  ungroup()

pander(tail(session_ranked, 50))

# Update nfdata dataframe with session rank
nfdata <- nfdata %>% 
  left_join(session_ranked, by = c("patId" = "patId", "session" = "session")) %>%
  arrange(patId)

# Update nfdata with patient number and select features
nfdata <- nfdata %>% left_join(patient_df %>% select(patId, patN, n_sessions), by = "patId") %>% 
  select(patN, age, sex, session, session_rank, n_sessions, itemId, rating)

# Convert variables
str(nfdata)
nfdata$rating <- as.numeric(nfdata$rating)
nfdata$sex[nfdata$sex == ""] <- NA #Replace blanks with NAs
nfdata$sex <- as.factor(nfdata$sex)

length(unique(nfdata$patN))

str(nfdata)

rm(question_list, session_ranked)

##################################################################
# Data exploration
##################################################################

# Reported items by year
nfdata_raw %>% ggplot(aes(year)) + 
  geom_histogram() +
  ggtitle("Number of reported items by year", subtitle = "") +
  ylab("Number of reported items") +
  theme_hc()

# Plot distributions of sessions per patient
patient_df %>% ggplot(aes(n_sessions)) + 
  geom_histogram(binwidth = 1, color = "white") +
  ggtitle("Patients by total number of sessions", subtitle = "") +
  ylab("Number of patients") +
  xlab("Total number of sessions") +
  theme_hc()

# Mean number of sessions
mean(patient_df$n_sessions)

# Inspect nf_data structure
glimpse(nfdata)

# Inspect number of items in dataset
length(unique((nfdata$itemId)))

# Inspect number of patients in dataset
length(unique((nfdata$patN)))

# Age distribution
nfdata %>% ggplot(aes(age)) + 
  geom_histogram(binwidth = 1, color = "white") +
  ggtitle("Age distribution of patients in dataset", subtitle = "") +
  ylab("Number of patients") +
  theme_hc()

# Gender distribution
nfdata %>% distinct(patN, .keep_all = TRUE) %>%
  ggplot(aes(sex, fill = sex)) + 
  geom_bar() +
  ggtitle("Gender distribution of patients in dataset", subtitle = "") +
  scale_y_continuous(labels = comma) +
  ylab("Number of patients") +
  theme_hc()

# Item report distribution
nfdata %>% group_by(itemId) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(itemId, -count), fill = count)) + 
  geom_bar() +
  ggtitle("Distribution of reports on individual items", subtitle = "") +
  xlab("ItemId") +
  ylab("Number of reports") +
  theme_hc() +
  theme(legend.position = "NULL")

#############################################################################
# Missing values
#############################################################################

tibble("age" = sum(is.na(nfdata$age)),
       "sex" = sum(is.na(nfdata$sex)),
       "session" = sum(is.na(nfdata$session)),
       "session_rank" = sum(is.na(nfdata$session_rank)),
       "n_sessions" = sum(is.na(nfdata$n_sessions)),
       "itemId" = sum(is.na(nfdata$itemId)),
       "rating" = sum(is.na(nfdata$rating)))

if(!require(lares)) install.packages("lares", repos = "http://cran.us.r-project.org")
missingness(nfdata)

#############################################################################
# Methodology
#############################################################################

#Create a logistic function curve
x_plogis <- seq(- 10, 10, by = 0.1) 
y_plogis <- plogis(x_plogis)
plot(x_plogis, y_plogis)

#############################################################################
# Outcome labeling
#############################################################################

# Briefly inspect the data
glimpse(nfdata)
length(unique(nfdata$patN))

# Outcome parameter is item 11
pander(item_df[11])

# Missing values on item 11
nfdata %>% filter(itemId == 11) %>% summarize(n = sum(is.na(rating)))

# Create a dataframe to store improvement in outcomes
outcomes_df <- nfdata %>% 
  filter(itemId == 11) %>% 
  group_by(patN) %>% 
  mutate(imp = rating[which.min(session_rank)]-rating[which.max(session_rank)]) %>%
  ungroup() %>%
  arrange(patN)

# Find patients that start treatment with 7 and end with 6
filter_list <- nfdata %>% 
  filter(itemId == 11, session_rank == 1, rating == 7) %>% 
  select(patN, itemId, session_rank, rating)

filter_list <- nfdata %>% filter(itemId == 11, patN %in% filter_list$patN) %>%
  group_by(patN) %>%
  filter(session_rank == max(session_rank), rating == 6)

# Number of patients to filter
length(unique(filter_list$patN))

# Set the improvement for the filtered patients to 0
outcomes_df$imp[outcomes_df$patN %in% filter_list$patN] <- 0

# Plot distribution of outcomes
hist(outcomes_df$imp)

# Summary statistics of outcomes
summary(outcomes_df$imp)

# Binary outcome
outcomes <- outcomes_df %>% 
  mutate(outcome = ifelse(imp > 0, 1, 0)) %>% 
  select(patN, outcome) %>%
  distinct(patN, .keep_all = TRUE)

# Create final data set
nfdata <- nfdata %>% 
  left_join(outcomes, by = "patN") %>%
  filter(!is.na(outcome)) 

glimpse(nfdata %>% distinct(patN, .keep_all = TRUE) %>% select(patN, outcome))

# Proportion with no treatment effect
mean(nfdata$outcome == 0)

# Update patient_df
patient_df <- patient_df %>% filter(patN %in% nfdata$patN)

rm(filter_list, outcomes, outcomes_df, question_df, nfdata_raw)

##################################################################
# Transform dataset to wide format
##################################################################

# I want to use the first 3 sessions to train and predict outcome
nfdata_w <- nfdata %>% group_by(patN) %>% 
  filter(session_rank <=3) %>%
  select(-session, -n_sessions) %>%
  ungroup()

# Some patients got a year older during treatment
age_changed <- nfdata_w %>% 
  group_by(patN, age) %>%
  summarize(n = n()) %>%
  ungroup() %>% 
  filter(patN %in% unique(.[["patN"]][duplicated(.[["patN"]])]))

head(age_changed, 20)

# Set patient age to age at first session
nfdata_w <- nfdata_w %>% 
  group_by(patN) %>% 
  mutate(age = min(age)) %>%
  ungroup()

# Create wide dataframe
nfdata_w <- pivot_wider(nfdata_w, 
                        names_from = c(itemId, session_rank), 
                        values_from = rating, 
                        values_fn = list(rating = mean))

# One-hot encoding
dmy <- dummyVars("~.", data = nfdata_w)
nfdata_w <- data.frame(predict(dmy, newdata = nfdata_w))

glimpse(nfdata_w)

rm(dmy, age_changed)

##################################################################
# Missing values
##################################################################

# lares library - exploring and reducing missing data
missingness(nfdata_w)

# Removing patients with missing report on item 11 in the first session
nfdata_w <- nfdata_w %>%
  filter(!is.na(X.11_1.))

missingness(nfdata_w)

length(unique(nfdata_w$patN))

# Update patient dataframe
patient_df <- patient_df %>% filter(patN %in% nfdata_w$patN)

# Replace NAs in the item rating columns with 0s
nfdata_w[, 7:93][is.na(nfdata_w[,7:93])] <- 0
head(nfdata_w)

##################################################################
# Feature engineering
##################################################################

if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")

# Sort columns by name
nfdata_w <- nfdata_w %>% select(order(colnames(.)))

# Create temporary dataframes for each session and sort columns by name
nfdata_1 <- nfdata_w %>% select(ends_with("_1.")) %>% select(order(colnames(.)))
nfdata_2 <- nfdata_w %>% select(ends_with("_2.")) %>% select(order(colnames(.)))
nfdata_3 <- nfdata_w %>% select(ends_with("_3.")) %>% select(order(colnames(.)))

# Calculate the trend for the first to the third session
nfdata_t <- data.frame(
  sapply(seq(1:ncol(nfdata_1)), 
         function(i){(nfdata_1[,i] - nfdata_3[,i])/-2}))

# Rename columns
colnames(nfdata_t) <- gsub("_1.", "_trend", colnames(nfdata_1))

# Find the standard deviation for each item over the three first sessions
nfdata_sd <- data.frame(sapply(seq(7, ncol(nfdata_w), 3),
                          function(i){rowSds(as.matrix(nfdata_w[,i:(i+2)]))}))

# Rename columns
colnames(nfdata_sd) <- gsub("_1.", "_sd", colnames(nfdata_1))

# Update the dataset and select features to use for analysis
nfdata_w <- bind_cols(nfdata_w %>% 
                        select(patN, age, sex.F, sex.M, sex.O, ends_with("_3."), outcome) %>% 
                        select(order(colnames(.))), nfdata_t, nfdata_sd)

glimpse(nfdata_w)

rm(nfdata_1, nfdata_2, nfdata_3)

##################################################################
# Create train, test and validation sets
##################################################################

# Split into train and validation set
set.seed(31) 
test_index <- createDataPartition(y = nfdata_w$outcome, times = 1, p = 0.2, list = FALSE)
nfdata_train_full <- nfdata_w[-test_index,]
nfdata_validation <- nfdata_w[test_index,]

# Split into train and test set
set.seed(41) 
test_index <- createDataPartition(y = nfdata_train_full$outcome, times = 1, p = 0.2, list = FALSE)
nfdata_test <- nfdata_train_full[test_index,]
nfdata_train <- nfdata_train_full[-test_index,]

nfdata_train <- nfdata_train %>% select(order(colnames(.)))
nfdata_test <- nfdata_test %>% select(order(colnames(.)))

summary(nfdata_train$outcome)
summary(nfdata_test$outcome)
summary(nfdata_validation$outcome)

dim(nfdata_train)
dim(nfdata_test)
dim(nfdata_validation)

rm(test_index, nfdata_w)


##################################################################
# Explore the training set
##################################################################

if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

# Correlation matrix for features
glimpse(nfdata_train_full)
mcor <- cor(na.omit(nfdata_train_full))
corrplot.mixed(mcor, upper = "color", lower = "number", number.cex= 0.3, tl.cex = 0.3)

# Filtered correlation matrix
threshold <- 0.7
mcor_1 <- mcor
diag(mcor_1) <- 0
mcor_2 <- apply(abs(mcor_1) >= threshold, 1, any)
mcor[mcor_2, mcor_2]

corrplot.mixed(mcor[mcor_2, mcor_2], upper = "color", lower = "number", number.cex= 0.7, tl.cex = 0.7)

# Exploring feature correlations
corr_cross(nfdata_train_full, max_pvalue = 0.05, top = 20)

# Item-item correlations for session 3
nfdata_train_full %>% select(ends_with("3.")) %>% corr_cross(max_pvalue = 0.05, top = 25)

# Feature correlation with outcome
corr_var(nfdata_train_full, var = outcome, top = 20)

#Local correlation for third session items
nfdata_train_full %>% select(ends_with("3.")) %>% corr_cross(type = 2)

# Frequency of response per item
freqs_df(nfdata_train_full[,7:92], plot = TRUE)

##################################################################
# Random prediction based on probability
##################################################################

if(!require(pscl)) install.packages("pscl", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")

# Finding the probability of a "good" outcome
prob_1 <- mean(nfdata_train$outcome == 1)

# Create random predictions based on probability 
set.seed(574)
base_pred <- rbinom(n = nrow(nfdata_test), size = 1, prob = prob_1)

# Check proportion of "good" outcomes in the predictions
mean(base_pred == 1)

# Create a confusion matrix
cfm <- confusionMatrix(as.factor(base_pred), as.factor(nfdata_test$outcome))
cfm

# Plot ROC curve and calculate the AUC
pr <- prediction(base_pred, nfdata_test$outcome)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Create a summary table for results
summary <- tribble( ~Model, ~Accuracy, ~"95% CI Lower", ~"95% CI Upper", ~AUC, 
                    "Random", cfm$overall[[1]], cfm$overall[[3]], cfm$overall[[4]], auc)

# Print summary table
knitr::kable(summary)

##################################################################
# Logistic regression
##################################################################

if(!require(pscl)) install.packages("pscl", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")

# Set up datasets, remove the patient number variable.
glm_train <- nfdata_train %>% select(-patN)
glm_test <- nfdata_test %>% select(-patN)

# For glm there can be no NAs. 
# Choosing to set NAs to 0s rather than omitting sex as a variable
glm_train[is.na(glm_train)] <- 0
glm_test[is.na(glm_test)] <- 0

# Train a logistic regression model
glm_model <- glm(outcome ~., family=binomial, data = glm_train)
summary(glm_model)
alias(glm_model)

# Pseudo R^2 for the model 
pR2(glm_model)

# Create predictions in the test set
glm_pred <- predict.glm(glm_model, newdata = glm_test, type='response')

# Predictions are given as probabilities, transform to binary outcome
glm_pred <- ifelse(glm_pred > 0.5,1,0)

# Construct a confusion matrix
cfm2 <- confusionMatrix(as.factor(glm_pred), as.factor(glm_test$outcome))
cfm2

# Plot ROC curve and calculate the AUC
pr <- prediction(glm_pred, glm_test$outcome)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc2 <- performance(pr, measure = "auc")
auc2 <- auc2@y.values[[1]]
auc2

# Update the summary table for results

summary <- bind_rows(
  summary, tribble( ~Model, ~Accuracy, ~"95% CI Lower", ~"95% CI Upper", ~AUC, 
                    "glm", cfm2$overall[[1]], cfm2$overall[[3]], cfm2$overall[[4]], auc2))

knitr::kable(summary)

###################################################################
# caret::XGBoost
###################################################################

xgb_train <- nfdata_train %>% select(-outcome, -patN)
xgb_test <- nfdata_test %>% select(-outcome, -patN)

glimpse(xgb_train)
glimpse(xgb_test)

xgb_tr_outcome <- nfdata_train$outcome
xgb_te_outcome <- nfdata_test$outcome

# Caret algorithm requires outcomes to be factors
xgb_tr_foutcome <- as.factor(xgb_tr_outcome)
xgb_te_foutcome <- as.factor(xgb_te_outcome)

#+++++++++++++++++++++++++++++++++++++++++++++++
# Base xgboost model - using default parameters
#+++++++++++++++++++++++++++++++++++++++++++++++

set.seed(93)

grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "none",
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_base <- caret::train(
  x = xgb_train,
  y = xgb_tr_foutcome,
  trControl = train_control,
  tuneGrid = grid_default,
  method = "xgbTree",
  verbose = TRUE
)

# Evaluating fit on test data
xgb_base_pred <- (as.numeric(predict(xgb_base, xgb_test)))-1
mean(xgb_te_outcome == xgb_base_pred)

cfm3 <- confusionMatrix(as.factor(xgb_base_pred), xgb_te_foutcome)
cfm3

varImp(xgb_base)

# Plot ROC curve
pr <- prediction(predictions = xgb_base_pred, labels = xgb_te_foutcome)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc3 <- performance(pr, measure = "auc")
auc3 <- auc3@y.values[[1]]
auc3

summary <- bind_rows(
  summary, tribble( ~Model, ~Accuracy, ~"95% CI Lower", ~"95% CI Upper", ~AUC, 
                    "XGBoost - default", cfm3$overall[[1]], cfm3$overall[[3]], cfm3$overall[[4]], auc3))

knitr::kable(summary)

#+++++++++++++++++++++++
# Tune model
#+++++++++++++++++++++++

# Plot function
tuneplot <- function(x, probs = .99) {
  ggplot(x) +
    coord_cartesian(ylim = c(min(x$results$Accuracy), max(x$results$Accuracy))) +
    theme_hc()
}


# Tune learning rate and tree depth
nrounds <- 1000

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 6, 8, 10),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 5, # with n folds 
  verboseIter = FALSE, 
  allowParallel = TRUE,
  seeds = set.seed(45)  
)

xgb_tune_depth <- caret::train(
  x = xgb_train,
  y = xgb_tr_foutcome,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbosity = 0
)

tuneplot(xgb_tune_depth)
xgb_tune_depth$bestTune

# Tune child weight
nrounds <- 1000

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = xgb_tune_depth$bestTune$eta,
  max_depth = xgb_tune_depth$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3, 4, 5),
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 5, # with n folds 
  verboseIter = FALSE, 
  allowParallel = TRUE,
  seeds = set.seed(62)
)

xgb_tune_child <- caret::train(
  x = xgb_train,
  y = xgb_tr_foutcome,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbosity = 0
)

tuneplot(xgb_tune_child)
xgb_tune_child$bestTune

# Tune column sample and subsample rate
nrounds <- 1000

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = xgb_tune_depth$bestTune$eta,
  max_depth = xgb_tune_depth$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune_child$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 5, # with n folds 
  verboseIter = FALSE, 
  allowParallel = TRUE,
  seeds = set.seed(27)  
)

xgb_tune_sample <- caret::train(
  x = xgb_train,
  y = xgb_tr_foutcome,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbosity = 0
)


tuneplot(xgb_tune_sample)
xgb_tune_sample$bestTune

# Tune gamma
nrounds <- 1000

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = xgb_tune_depth$bestTune$eta,
  max_depth = xgb_tune_depth$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune_sample$bestTune$colsample_bytree,
  min_child_weight = xgb_tune_child$bestTune$min_child_weight,
  subsample = xgb_tune_sample$bestTune$subsample
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 5, # with n folds 
  verboseIter = FALSE, 
  allowParallel = TRUE,
  seeds = set.seed(15)  
)

xgb_tune_gamma <- caret::train(
  x = xgb_train,
  y = xgb_tr_foutcome,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbosity = 0
)


tuneplot(xgb_tune_gamma)
xgb_tune_gamma$bestTune

# Tune eta
nrounds <- 5000

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.01, 0.025, 0.05, 0.1, 0.3),
  max_depth = xgb_tune_depth$bestTune$max_depth,
  gamma = xgb_tune_gamma$bestTune$gamma,
  colsample_bytree = xgb_tune_sample$bestTune$colsample_bytree,
  min_child_weight = xgb_tune_child$bestTune$min_child_weight,
  subsample = xgb_tune_sample$bestTune$subsample
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 5, # with n folds 
  verboseIter = FALSE, 
  allowParallel = TRUE,
  seeds = set.seed(52)  
)

xgb_tune_eta <- caret::train(
  x = xgb_train,
  y = xgb_tr_foutcome,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbosity = 0
)

tuneplot(xgb_tune_eta)
xgb_tune_eta$bestTune

#+++++++++++++++++++++++++++++++
# XGBoost package algorithms
#+++++++++++++++++++++++++++++++

if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(DiagrammeR)) install.packages("DiagrammeR", repos = "http://cran.us.r-project.org") #required by xgboost
if(!require(Ckmeans.1d.dp)) install.packages("Ckmeans.1d.dp", repos = "http://cran.us.r-project.org") #required by xgboost

# The XGBoost library requires a matrix for outcomes
xgb_tr_outcome <- as.matrix(xgb_tr_outcome)
xgb_te_outcome <- as.matrix(xgb_te_outcome)

# Converting the datasets into the xgb.DMatrix format for the XGBoost algorithms
xgb_train_xgb <- xgb.DMatrix(data = as.matrix(xgb_train), label = xgb_tr_outcome)
xgb_test_xgb <- xgb.DMatrix(data = as.matrix(xgb_test), label = xgb_te_outcome)

# Best parameteres from tuning
params <- list(booster = "gbtree", 
                objective = "binary:logistic", 
                eta=0.05, 
                gamma= 0.9,
                alpha = 0,
                lambda = 0,
                max_depth = 2, 
                min_child_weight= 2, 
                subsample= 1, 
                colsample_bytree= 1,
                eval_metric = "error")

# 5-fold cross-validation with XGBoost library to find optimal nrounds
set.seed(68)
xgbcv <- xgb.cv(data = as.matrix(xgb_train),
                label = xgb_tr_outcome,
                params = params,
                nrounds = 5000,
                nfold = 5,
                showsd = T,
                stratified = T,
                print_every_n = 50,
                early_stopping_rounds = 1500,
                maximize = F)

# Plot error curves
cv_model_error <- tibble(train_error = xgbcv$evaluation_log$train_error_mean, 
                         test_error = xgbcv$evaluation_log$test_error_mean,
                         iters = seq(1, length(xgbcv$evaluation_log$train_error_mean), 1))

ggplot(cv_model_error) + geom_point(aes(iters, train_error), color = "blue") + 
  geom_point(aes(iters, test_error), color = "red") +
  theme_hc()

# Train model with hyperparameters from the tuning process
set.seed(75)
xgb <- xgb.train(params = params, 
                 data = xgb_train_xgb,
                 nrounds = 325, 
                 watchlist = list(test = xgb_test_xgb, train = xgb_train_xgb),
                 print_every_n = 10,
                 early_stopping_rounds = 500, 
                 maximize = F)

# Plot error
model_error <- tibble(train_error = xgb$evaluation_log$train_error, 
                      test_error = xgb$evaluation_log$test_error,
                      iters = seq(1, length(train_error), 1))

ggplot(model_error) + geom_point(aes(iters, train_error), color = "blue") + 
  geom_point(aes(iters, test_error), color = "red") +
  theme_hc()

# Make predictions on test set
xgbpred <- predict(xgb,xgb_test_xgb)
xgbpreds <- ifelse (xgbpred > 0.5,1,0)

cfm4 <- confusionMatrix (as.factor(xgbpreds), as.factor(xgb_te_outcome))
cfm4

# Plot ROC curve
pr <- prediction(predictions = xgbpred, labels = xgb_te_outcome)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,
     colorize = TRUE,
     print.cutoffs.at= seq(0,1,0.05),
     text.adj=c(-0.2,1.7))
auc4 <- performance(pr, measure = "auc")
auc4 <- auc4@y.values[[1]]
auc4

summary <- bind_rows(
  summary, tribble( ~Model, ~Accuracy, ~"95% CI Lower", ~"95% CI Upper", ~AUC, 
                    "XGBoost - tuned", cfm4$overall[[1]], cfm4$overall[[3]], cfm4$overall[[4]], auc4))

summary

########################################################################
# Final model validation
########################################################################

# Train final model on all training data and test in validation set
xgb_train_full <- nfdata_train_full %>% select(-outcome, -patN)
xgb_valid <- nfdata_validation %>% select(-outcome, -patN)

xgb_train_outcome <- as.matrix(nfdata_train_full$outcome)
xgb_vali_outcome <- as.matrix(nfdata_validation$outcome)

xgb_train_f <- xgb.DMatrix(data = as.matrix(xgb_train_full), label = xgb_train_outcome)
xgb_vali <- xgb.DMatrix(data = as.matrix(xgb_valid), label = xgb_vali_outcome)

set.seed(1536489)
xgb_validation <- xgb.train(params = params, 
                            data = xgb_train_f,
                            nrounds = 325, 
                            print_every_n = 10,
                            early_stopping_rounds = NULL, 
                            maximize = F)

# Make predictions on validation set
xgbpred_v <- predict(xgb_validation, xgb_vali)
xgbpreds_v <- ifelse (xgbpred_v > 0.5,1,0)

cfm_v <- confusionMatrix (as.factor(xgbpreds_v), as.factor(xgb_vali_outcome))
cfm_v

# Plot ROC curve
pr <- prediction(predictions = xgbpred_v, labels = xgb_vali_outcome)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,
     colorize = TRUE,
     print.cutoffs.at= seq(0,1,0.05),
     text.adj=c(-0.2,1.7))
auc_v <- performance(pr, measure = "auc")
auc_v <- auc_v@y.values[[1]]
auc_v

# Create a summary table for validation results
summary_v <- tribble( ~Model, ~Accuracy, ~"95% CI Lower", ~"95% CI Upper", ~AUC, 
                      "Validation", cfm_v$overall[[1]], cfm_v$overall[[3]], cfm_v$overall[[4]], auc_v)

summary_v

#++++++++++++++++++++++++++++++++++++++
# Model explanation
#++++++++++++++++++++++++++++++++++++++

# Create a table of predictions, probability from model, confidence in prediction (in percent) and outcome.
preds_v <- tibble("prediction" = xgbpreds_v,
                  "probability" = xgbpred_v,
                  "confidence" = abs(xgbpred_v/0.5 -1)*100,
                  "y" = xgb_vali_outcome,
                  "correct" = ifelse(xgbpreds_v == xgb_vali_outcome, 1, 0))

pander(head(preds_v, 20))

preds_v %>% ggplot(aes(correct, confidence, color = correct)) + 
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  scale_x_discrete(labels = NULL) + 
  labs(x = "") +
  annotate("text", x = 0, y =  -3, label = "Predicted wrong") +
  annotate("text", x = 1, y =  -3, label = "Predicted right") +
  coord_cartesian(ylim = c(0, 100), clip = "off") +
  theme_minimal()

# Density plot of predictions
mplot_density(tag = xgb_vali_outcome, score = xgbpred_v, subtitle = "Distribution of predictions in XGBoost model")

# Validation set: SHAP values and ranked features by mean|SHAP|
if(!require(SHAPforxgboost)) install.packages("SHAPforxgboost", repos = "http://cran.us.r-project.org")
shap_values_v <- shap.values(xgb_model = xgb_validation, X_train = as.matrix(xgb_valid))

# Validation set: Ranked features by mean |SHAP|
shap_values_v$mean_shap_score

# Validation set: Calculate SHAP values and plot summary
shap_long_v <- shap.prep(xgb_model = xgb_validation, X_train = as.matrix(xgb_valid), top_n = 10)
shap.plot.summary(shap_long_v, x_bound = 1.5, dilute = 1)

# Inspecting items 25, 17, 28 and 27
item_df[c(25,17,28,27), c(1,3)]

# Exploring the impact of item 11 features in model
g1 <- shap.plot.dependence(data_long = shap_long_v,
                           x = 'X.11_3.',
                           y = 'X.11_3.',
                           color_feature = 'X.11_3.') +
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  ylim(-1.6, 1.6) +
  ggtitle("(A) SHAP values of item 11 vs. feature values of item 11")

g2 <- shap.plot.dependence(data_long = shap_long_v,
                           x = 'X.11_trend',
                           y = 'X.11_trend',
                           color_feature = 'X.11_trend') +
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  ylim(-1.6, 1.6) +
  ggtitle("(B) SHAP values of item 11 trend vs. feature values of item 11 trend")

gridExtra::grid.arrange(g1, g2, ncol = 2)

# Visualization of individual patients impact from items/features

# Force plot for the 40 first patients. Showing top 7 features by importance
plot_data <- shap.prep.stack.data(shap_contrib = shap_values_v$shap_score[1:40], top_n = 7)
shap.plot.force_plot(plot_data, zoom_in = FALSE, y_parent_limit = c(-2,2))

# Create a waterfall plot function
if(!require(ggalluvial)) install.packages("ggalluvial", repos = "http://cran.us.r-project.org")
if(!require(waterfalls)) install.packages("waterfalls", repos = "http://cran.us.r-project.org")

plot_waterfall <- function(x, top_n = NULL){
  if (is.null(top_n)) top_n <- dim(x)[2]
  shap_long <- pivot_longer(shap_values_v$shap_score[x,], 
                            cols = 1:ncol(shap_values_v$shap_score), 
                            names_to = "feature", 
                            values_to = "value")
  bias <- tibble(feature = "bias", value = shap_values_v$BIAS0$BIAS)
  shap_long <- shap_long %>% bind_rows(bias) %>% 
    select(feature, value) %>%
    arrange(desc(abs(value))) %>%
    head(top_n)
  
  shap_long %>% 
  mutate(value = round(value, 2)) %>%
  waterfall() +
    ggtitle(paste("Waterfall plot for patient", x, "in validation set")) +
    xlab("Feature") +
    ylab("Impact on prediction") +
    theme(axis.text=element_text(size=8)) +
    theme_hc()
}

# Plot for random patients
plot_waterfall(122, 10)
plot_waterfall(222, 10)
plot_waterfall(322, 10)
plot_waterfall(422, 10)

