if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(caret)) install.packages('caret')
library(caret)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(lubridate)) install.packages('lubridate')
library(lubridate)

if (!require(knitr)) install.packages('knitr')
library(knitr)

if (!require(scales)) install.packages('scales')
library(scales)

if (!require(kableExtra)) install.packages('kableExtra')
library(kableExtra)


# Read in the CSV file
beer <- read.csv("beer_reviews.csv")

# Drop the additional "Rating" columns and only keep the Overall rating
beer <- beer %>% select(-one_of("review_aroma", "review_appearance",
                                "review_palate", "review_taste"))

# Rename some columns to make them easier to use
colnames(beer)[3]  <- "timestamp"          # Convert "review_time" to "timestamp"
colnames(beer)[4]  <- "rating"             # Convert "review_overall" to "rating"
colnames(beer)[5]  <- "user_id"            # Convert "review_profilename" to "user_id"
colnames(beer)[9] <- "beer_id"            # Convert "beer_beer_id" to "beer_id"

colnames(beer)

# Convert from a Reviewer Name Factor to a Numeric ID
beer$user_id <- as.numeric(beer$user_id)

# Create a numeric ID for Beer Styles
beer <- beer %>% mutate(beer_style_id = as.numeric(beer_style))

# CREATE TRAINING AND TESTING DATASETS
# The Testing Set will be 40% of the Beer Review Dataset

# NOTE: if using R version 3.6.0, use: set.seed(1, sample.kind = "Rounding") instead of set.seed(1)
# set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = beer$rating, times = 1, p = 0.4, list = FALSE)
beer_train <- beer[-test_index,]
temp <- beer[test_index,]

# Make sure user_id and beer_id in the Testing set are also in Training set
beer_test <- temp %>% 
  semi_join(beer_train, by = "beer_id") %>%
  semi_join(beer_train, by = "user_id")

# Add rows removed from beer_test set back into beer_train set
removed <- anti_join(temp, beer_test)
beer_train <- rbind(beer_train, removed)

# Clean up the Environment
rm(beer, removed, test_index, temp)


#******************************************************************************
#******************************************************************************

# Create working copies of the Training (beer_train) and Test (beer_test) datasets
train_set <- beer_train
test_set <- beer_test

# Define the RMSE Evaluation Function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#*******************************************************************************
# Visualize the Dataset
#*******************************************************************************

#*******************************************************************************
# Create Additional Datasets
#*******************************************************************************

# USER RATINGS - Create dataframe with additional User rating information:
#       Average (u_avg), StDev (u_std), Number of Reviews (u_numrtg)
user_data <- train_set %>%
  group_by(user_id) %>%
  summarize(u_avg = mean(rating),
            #            u_std = sd(rating),
            u_numrtg = as.numeric(n()))

# BEER RATINGS - Create dataframe with additional Beer rating information:
#       Average (b_avg), StDev (b_std), Number of Reviews (b_numrtg)
beer_data <- train_set %>%
  group_by(beer_id) %>%
  summarize(b_avg = mean(rating),
            #            b_std = sd(rating),
            b_numrtg = as.numeric(n()))

# BREWERY RATINGS - Create dataframe with additional Brewery rating information:
#       Average (brw_avg), StDev (brw_std), Number of Reviews (brw_numrtg)
brewery_data <- train_set %>%
  group_by(brewery_id) %>%
  summarize(brw_avg = mean(rating),
            #            brw_std = sd(rating),
            brw_numrtg = as.numeric(n()))

# BEER STYLE RATINGS - Create dataframe with additional Beer Style rating information:
#       Average (s_avg), StDev (s_std), Number of Reviews (s_numrtg)
beer_style_data <- train_set %>%
  group_by(beer_style_id) %>%
  summarize(s_avg = mean(rating),
            #            s_std = sd(rating),
            s_numrtg = as.numeric(n()))


#*******************************************************************************
# ALL BEERS
#*******************************************************************************
# Distribution of all Ratings
train_set %>%
  ggplot(aes(x = rating)) +
  geom_histogram(binwidth = 0.25, color = "black") +
  scale_x_continuous(breaks=seq(0, 10, 0.5)) +
  scale_y_continuous(labels=comma) +
  labs(x="Beer Rating", y="# of Ratings") +
  ggtitle("Distribution of all Ratings")


#*******************************************************************************
# USER INFORMATION
#*******************************************************************************

# Plot the distribution of Ratings by User ID in the Training set
train_set %>%
  group_by(user_id) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black") +
  labs(x="User Rating", y="# of Ratings") +
  ggtitle("Histogram of Ratings by User")

# Plot the log distribution of # of Ratings per User
user_data %>%
  ggplot(aes(x = u_numrtg)) +
  geom_histogram(bins = 100, color = "black") +
  scale_x_log10() +
  labs(x="# of Ratings (log10 scale)", y="# of Users") +
  ggtitle("Distribution - # of Ratings by User")

#*******************************************************************************
# BEER INFORMATION
#*******************************************************************************

# Plot the distribution of Ratings by Beer ID in the Training set
train_set %>%
  group_by(beer_id) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black") +
  labs(x="Beer Rating", y="# of Ratings") +
  ggtitle("Histogram of Ratings by Beer")

# Plot the log distribution of # of Ratings per Beer
beer_data %>%
  ggplot(aes(x = b_numrtg)) +
  geom_histogram(bins = 50, color = "black") +
  scale_x_log10() +
  labs(x="# of Ratings (log10 scale)", y="# of Beers") +
  ggtitle("Distribution - # of Ratings by Beer")

#*******************************************************************************
# BREWERY INFORMATION
#*******************************************************************************

# # of Breweries
# Top Breweries by Rating

# Plot the distribution of Ratings by Brewery ID in the Training set
train_set %>%
  group_by(brewery_id) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black") +
  labs(x="Brewery Rating", y="# of Ratings") +
  ggtitle("Histogram of Ratings by Brewery")

# Plot the log distribution of # of Ratings per Brewery
brewery_data %>%
  ggplot(aes(x = brw_numrtg)) +
  geom_histogram(bins = 50, color = "black") +
  scale_x_log10() +
  labs(x="# of Ratings (log10 scale)", y="# of Beers") +
  ggtitle("Distribution - # of Ratings by Brewery")

#*******************************************************************************
# BEER STYLE INFORMATION
#*******************************************************************************

# Plot the distribution of Ratings by Beer Style in the Training set
train_set %>%
  group_by(beer_style_id) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black") +
  labs(x="Brewery Rating", y="# of Ratings") +
  ggtitle("Histogram of Ratings by Brewery")

# Plot the log distribution of # of Ratings per Beer Style
beer_style_data %>%
  ggplot(aes(x = s_numrtg)) +
  geom_histogram(bins = 50, color = "black") +
  scale_x_log10() +
  labs(x="# of Ratings (log10 scale)", y="# of Beers") +
  ggtitle("Distribution - # of Ratings by Brewery")

# Display graph of Average Rating by all Beer Styles
train_set %>%
  group_by(beer_style_id) %>%
  summarize(avg = mean(rating)) %>%
  ggplot(aes(x = as.numeric(reorder(beer_style_id, avg)), y = avg)) +
  geom_point() +
  geom_smooth( aes(x = as.numeric(reorder(beer_style_id, avg)), y = avg),
               method = 'lm', formula = y ~ poly(x, 4), se = TRUE) +
  theme(axis.text.x=element_blank()) +
  labs(x="Beer Style", y="Average Rating") +
  ggtitle("Mean Rating by Beer Style")



#*******************************************************************************
#*******************************************************************************
#****************    Linear Regression - Model Building    *********************
#*******************************************************************************
#*******************************************************************************



#*******************************************************************************
# We're only using this to get the absolute WORST RMSE
#*******************************************************************************
# Linear Regression - Using just the overall average
#   Yu,i = mu
#*******************************************************************************

# Calculate the overall average rating
mu <- mean(train_set$rating)
mu

# Evaluate the performance of simply guessing the overall average
rmse <- RMSE(test_set$rating, mu)

# Save the RMSE result to display later
LR_rmse_results <- tibble(Method = "LR: Base Mean Model",
                          RMSE = rmse)
LR_rmse_results %>% knitr::kable()


#*******************************************************************************
#*******************************************************************************
#***********    Regularized Linear Regression - Model Building    **************
#*******************************************************************************
#*******************************************************************************

# Since we've previously seen that Regularized Linear Regression almost always
# outperformes Linear Regression, we are only going to use Penalized Least Squares

#*******************************************************************************
# Regularized Linear Regression - Base Average Model + Beer Effect:
#   Yu,i = mu + Beer_reg_avgs$b_i
#*******************************************************************************

# Find the best RMSE across range of Lambdas
lambdas <- seq(1, 3, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(beer_id) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "beer_id") %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)

# Lambda resulting in best fit / lowest RMSE
lambda <- lambdas[which.min(rmses)]
lambda

LR_rmse_results <- bind_rows(LR_rmse_results,
                             tibble(Method="Reg LR: Mean + Beer Effect Model",
                                    RMSE = min(rmses)))
LR_rmse_results %>% knitr::kable()


#*******************************************************************************
# Regularized Linear Regression - Mean + Beer Effect + User Effect:
#   Yu,i = mu + b_i + b_u
#*******************************************************************************

# Find the best RMSE across range of Lambdas
lambdas <- seq(2.5, 3.5, 0.1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(beer_id) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by="beer_id") %>%
    group_by(user_id) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "beer_id") %>%
    left_join(b_u, by = "user_id") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)

# Lambda resulting in best fit / lowest RMSE
lambda <- lambdas[which.min(rmses)]
lambda

LR_rmse_results <- bind_rows(LR_rmse_results,
                             tibble(Method="Reg LR: Mean + Beer + User Effect Model",
                                    RMSE = min(rmses)))
LR_rmse_results %>% knitr::kable()



#*******************************************************************************
# Regularized Linear Regression - Mean + Beer + User + Style Effect:
#   Yu,i = mu + b_i + b_u + s_u
#*******************************************************************************

# Find best Lambda to minimize RMSE
lambdas <- seq(2.8, 3.6, 0.1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(beer_id) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by="beer_id") %>%
    group_by(user_id) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  s_u <- train_set %>%
    left_join(b_i, by="beer_id") %>%
    left_join(b_u, by="user_id") %>%
    group_by(beer_style_id) %>%
    summarize(s_u = sum(rating - b_i - b_u - mu)/(n()+l))
  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "beer_id") %>%
    left_join(b_u, by = "user_id") %>%
    left_join(s_u, by = "beer_style_id") %>%
    mutate(pred = mu + b_i + b_u + s_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)]
lambda

LR_rmse_results <- bind_rows(LR_rmse_results,
                             tibble(Method="Reg LR: Mean + Beer + User + Style Effect Model",
                                    RMSE = min(rmses)))
LR_rmse_results %>% knitr::kable()

#*******************************************************************************
# Regularized Linear Regression - Mean + Beer + User + Style + Brewery Effect:
#   Yu,i = mu + b_i + b_u + s_u + brw_u
#*******************************************************************************

# Find best Lambda to minimize RMSE
lambdas <- seq(3, 6, 0.1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(beer_id) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by="beer_id") %>%
    group_by(user_id) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  s_u <- train_set %>%
    left_join(b_i, by="beer_id") %>%
    left_join(b_u, by="user_id") %>%
    group_by(beer_style_id) %>%
    summarize(s_u = sum(rating - b_i - b_u - mu)/(n()+l))
  brw_u <- train_set %>%
    left_join(b_i, by="beer_id") %>%
    left_join(b_u, by="user_id") %>%
    left_join(s_u, by="beer_style_id") %>%
    group_by(brewery_id) %>%
    summarize(brw_u = sum(rating - b_i - b_u -s_u - mu)/(n()+l))
  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "beer_id") %>%
    left_join(b_u, by = "user_id") %>%
    left_join(s_u, by = "beer_style_id") %>%
    left_join(brw_u, by = "brewery_id") %>%
    mutate(pred = mu + b_i + b_u + s_u + brw_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)]
lambda
min(rmses)

LR_rmse_results <- bind_rows(LR_rmse_results,
                             tibble(Method="Reg LR: Mean + Beer + User + Style + Brewery Effect Model",
                                    RMSE = min(rmses)))
LR_rmse_results %>% knitr::kable()


# Clean up Environment
rm(lambda, lambdas, rmses, mu)

#*******************************************************************************



#*******************************************************************************
#*******************************************************************************
#***********    XGB - Extreme Parallel Tree Boosting (xgBoost)    **************
#*******************************************************************************
#*******************************************************************************

# Load required libraries
if (!require(xgboost)) install.packages('xgboost')
library(xgboost)

if (!require(data.table)) install.packages('data.table')
library(data.table)

# Create new Training and Test datasets
# xgBoost requires that all values to be numeric

train_set <- beer_train %>% select(-one_of("brewery_name", "timestamp", "beer_style",
                                           "beer_name"))
test_set  <- beer_test  %>% select(-one_of("brewery_name", "timestamp", "beer_style",
                                           "beer_name"))

# Merge User, Beer, Brewery, and Beer Style derived fields into the Training and Test datasets
train_set <- train_set %>% left_join(user_data,       by = "user_id")
train_set <- train_set %>% left_join(beer_data,       by = "beer_id")
train_set <- train_set %>% left_join(brewery_data,    by = "brewery_id")
train_set <- train_set %>% left_join(beer_style_data, by = "beer_style_id")

test_set <- test_set %>% left_join(user_data,       by = "user_id")
test_set <- test_set %>% left_join(beer_data,       by = "beer_id")
test_set <- test_set %>% left_join(brewery_data,    by = "brewery_id")
test_set <- test_set %>% left_join(beer_style_data, by = "beer_style_id")


# xgBoost requires that the target variable, referred to as the "label" (rating) be in a separate data table
# Here we create a Label and Data for the Training and Testing sets
# We are also dropping the Genres variable since it isn't numeric and no longer needed due to the One-Hot encoding added earlier

train_data  <- train_set %>% select(-one_of("rating")) %>% as.data.table()
train_label <- train_set %>% select("rating")          %>% as.data.table()
test_data   <- test_set  %>% select(-one_of("rating")) %>% as.data.table()
test_label  <- test_set  %>% select("rating")          %>% as.data.table()

# Create the xgBoost Training & Testing matricies
train_matrix = xgb.DMatrix(as.matrix(train_data), label=as.matrix(train_label))
test_matrix = xgb.DMatrix(as.matrix(test_data), label=as.matrix(test_label))


# xgBoost allows the creation of both Linear, Tree, and Mixed Linear & Tree based models
# xgBoost can also create Classification trees (binary and multi-class) by changing the objective function
# We will be creating only regression trees. Linear trees are the fastest to train, but do not produce results as good as mixed models

# Below are the multiple paramaters used to train the models presented in the Results
# Most take a considerable amount of time to train so we will just train a simple model first

#**********************************************************************************************
# Model: XGB Linear 50 Boost Rnds
#**********************************************************************************************
# Set paramaters for most basic Linear tree
# These settings should train in less than a minute with 50 Boosting Rounds
xgb_params <- list(booster = "gblinear",      # Linear boosting alg
                   objective = "reg:linear",  # Linear regression (default)
                   eval_metric = "rmse",      # rmse as objective function
                   verbosity = 3,             # Highest verbosr level - shows all debug info
                   silent = 0)                # Not silent

# Train the XGB tree using the currently set xgb_params
start_time <- Sys.time()                    # Record start time

set.seed(1, sample.kind = "Rounding")
xgb_model <- xgboost(params = xgb_params,
                     data = train_matrix,
                     nrounds = 50,          # Maximum number of Boosting Rounds
                     nthread = 1,           # Must be set to 1 to get reproducible results
                     verbose = 2)           # Display pogress during Training

finish_time <- Sys.time()                   # Record finish time
finish_time - start_time                    # Display total training time

# Use the trained model to predict the Test dataset
test_pred <- as.data.frame(predict(xgb_model , newdata = test_matrix))

# Calculate the RMSE of the Predictions
rmse <- RMSE(test_label$rating, test_pred$`predict(xgb_model, newdata = test_matrix)`)

XGB_rmse_results <- tibble(Method="XGB Linear 50 Boost Rnds",
                           RMSE = rmse,
                           Train_Time = paste(as.character(round(as.numeric(finish_time - start_time, units="secs"), 2)), "secs"))
XGB_rmse_results %>% knitr::kable()


#**********************************************************************************************
# Model: XGB Linear 1000 Boost Rnds
#**********************************************************************************************

xgb_params <- list(booster = "gblinear",      # Linear boosting alg
                   objective = "reg:linear",  # Linear regression (default)
                   eval_metric = "rmse",      # rmse as objective function
                   verbosity = 3,             # Highest verbosr level - shows all debug info
                   silent = 0)                # Not silent

# Train the XGB tree using the currently set xgb_params
start_time <- Sys.time()                    # Record start time

set.seed(1, sample.kind = "Rounding")
xgb_model <- xgboost(params = xgb_params,
                     data = train_matrix,
                     nrounds = 1000,        # Maximum number of Boosting Rounds
                     nthread = 1,           # Must be set to 1 to get reproducible results
                     verbose = 2)           # Display pogress during Training

finish_time <- Sys.time()                   # Record finish time
finish_time - start_time                    # Display total training time

# Use the trained model to predict the Test dataset
test_pred <- as.data.frame(predict(xgb_model , newdata = test_matrix))

# Calculate the RMSE of the Predictions
rmse <- RMSE(test_label$rating, test_pred$`predict(xgb_model, newdata = test_matrix)`)

XGB_rmse_results <- bind_rows(XGB_rmse_results,
                              tibble(Method="XGB Linear 1000 Boost Rnds",
                                     RMSE = rmse,
                                     Train_Time = paste(as.character(round(as.numeric(finish_time - start_time, units="secs"), 2)), "secs")))
XGB_rmse_results %>% knitr::kable()


#**********************************************************************************************
# Model: XGB Mixed Tree 5 Boost Rnds
#**********************************************************************************************

xgb_params <- list(booster = "gbtree",
                   objective = "reg:linear",
                   colsample_bynode = 0.8,
                   learning_rate = 1,
                   max_depth = 10,
                   num_parallel_tree = 25,
                   subsample = 0.8,
                   verbosity = 3,
                   silent = 0)

# Train the XGB tree using the currently set xgb_params
start_time <- Sys.time()                    # Record start time

set.seed(1, sample.kind = "Rounding")
xgb_model <- xgboost(params = xgb_params,
                     data = train_matrix,
                     nrounds = 5,           # Maximum number of Boosting Rounds
                     nthread = 1,           # Must be set to 1 to get reproducible results
                     verbose = 2)           # Display pogress during Training

finish_time <- Sys.time()                   # Record finish time
finish_time - start_time                    # Display total training time

# Use the trained model to predict the Test dataset
test_pred <- as.data.frame(predict(xgb_model , newdata = test_matrix))

# Calculate the RMSE of the Predictions
rmse <- RMSE(test_label$rating, test_pred$`predict(xgb_model, newdata = test_matrix)`)

XGB_rmse_results <- bind_rows(XGB_rmse_results,
                              tibble(Method="XGB Mixed Tree 5 Boost Rnds",
                                     RMSE = rmse,
                                     Train_Time = paste(as.character(round(as.numeric(finish_time - start_time, units="secs"), 2)), "secs")))
XGB_rmse_results %>% knitr::kable()



#*******************************************************************************
#*******************************************************************************
#** Recosystem - Matrix Factorization w/ Parallel Stochastic Gradient Descent **
#*******************************************************************************
#*******************************************************************************

# Load required libraries
if (!require(recosystem)) install.packages('recosystem')
library(recosystem)

#************************************************************************************
# Recosystem - Matrix Factorization with Parallel Stochastic Gradient Descent
#************************************************************************************

# Create new Traing and Test datasets. Recosystem only uses three columns: Rating, user_id, and beer_id
train_set <- beer_train %>% select("user_id","beer_id", "rating") %>% as.matrix()
test_set <-  beer_test %>% select("user_id","beer_id", "rating") %>% as.matrix()


# Create the Reco Recommender object
r = Reco()


# One very nice feature of Reco, besides it's speed, is that tuning training set is very easy and the
# overall best performing tuned paramaters are stored directly in the Reco object.

# Here we will create some Tuning Grids

#***********************************************************************************
# First Tuning Round - Takes around 20 minutes with 8 Threads
#***********************************************************************************
opts_list = list(dim      = c(10, 20, 30, 40), # Number of Latent Features
                 costp_l1 = c(0, 0.1),         # L1 regularization cost for User factors
                 costp_l2 = c(0.01, 0.1),      # L2 regularization cost for User factors
                 costq_l1 = c(0, 0.1),         # L1 regularization cost for Beer factors
                 costq_l2 = c(0.01, 0.1),      # L2 regularization cost for Beer factors
                 lrate    = c(0.01, 0.1),      # Learning Rate - Aprox step size in Gradient Descent
                 niter    = 50,                # Number of Iterations for Training (Not used in Tuning)
                 nfolds   = 5,                 # Number of Folds for CV in Tuning
                 verbose  = FALSE,             # Don't Show Progress
                 nthread  = 8)        #!!! Can be set to higher values for Tuning, but MUST be set to 1
#    for Training or the results are not reproducible
# RESULTS
# dim      20       
# costp_l1 0        
# costp_l2 0.01     
# costq_l1 0.1      
# costq_l2 0.1      
# lrate    0.1 

#***********************************************************************************


#***********************************************************************************
# TUNE the Model - Takes around 20 minutes with 8 Threads
#***********************************************************************************

start <- Sys.time()

set.seed(1, sample.kind = "Rounding")
opts_tune = r$tune(data_memory(train_set[, 1], train_set[, 2], train_set[, 3]),
                   opts = opts_list)

finish <- Sys.time()
tune_time <- finish - start
tune_time
opts_tune$min


#***********************************************************************************
# TRAIN the Model over 50 Iterations
#***********************************************************************************
start <- Sys.time()

set.seed(1, sample.kind = "Rounding")
r$train(data_memory(train_set[, 1], train_set[, 2], train_set[, 3]),
        opts = c(opts_tune$min,
                 niter    = 50,     # Train over 50 Iterations
                 nthread = 1))      # Must be set to 1 for training

finish <- Sys.time()
train_time <- finish - start
train_time


#***********************************************************************************
# PREDICT the Test Ratings
#***********************************************************************************
start <- Sys.time()

pred <- r$predict(data_memory(test_set[, 1], test_set[, 2], test_set[, 3]), out_memory())

finish <- Sys.time()
pred_time <- finish - start
pred_time


#***********************************************************************************
# Calculate the RMSE
#***********************************************************************************

rmse <- RMSE(test_set[,3],pred)

RECO_rmse_results <- tibble(Method="V1 - 40 x 50",
                            RMSE = rmse,
                            Train_Time = round(as.numeric(train_time, units="mins"), 2))
RECO_rmse_results %>% knitr::kable()



# Since we've seen that the # of Latent Factors is important, sweep the CV

#***********************************************************************************
# Tune the DIM parameter - FOR 10-100 and then 10-20 and Graph
# !!!!  NOTE !!!!  THESE WILL TAKE A LONG TIME TO RUN
#***********************************************************************************
# Tune & Graph Latent Factors (dim) from 10 to 60 by 10
#***********************************************************************************

opts_list = list(dim      = c(seq(10, 60, 10)),    # Number of Latent Features
                 costp_l1 = c(0),       # L1 regularization cost for User factors
                 costp_l2 = c(0.01),    # L2 regularization cost for User factors
                 costq_l1 = c(0.1),     # L1 regularization cost for Beer factors
                 costq_l2 = c(0.1),     # L2 regularization cost for Beer factors
                 lrate    = c(0.1),     # Learning Rate - Aprox step size in Gradient Descent
                 nfold    = 5,          # Number of folds for Cross Validation
                 nthread  = 8,          # Set Number of Threads to 1 *** Required to get Reproducible Results ***
                 niter    = 50,         # Number of Iterations
                 verbose  = FALSE)      # Don't Show Fold Details

start <- Sys.time()
set.seed(1, sample.kind = "Rounding")
opts_tune <- r$tune(data_memory(train_set[, 1], train_set[, 2], train_set[, 3]),
                    opts = opts_list)
finish <- Sys.time()
tune_time <- finish - start

opts_tune$min

opts_tune$res %>%
  ggplot(aes(dim, loss_fun)) +
  geom_point() +
  geom_smooth(method="loess") +
  labs(x="Latent Factors (dim)", y="RMSE") +
  ggtitle("Latent Factors vs RMSE")

RECO_CV_10_100 <- opts_tune


#***********************************************************************************
# Tune & Graph Latent Factors (dim) from 10 to 30
#***********************************************************************************

opts_list = list(dim      = c(seq(10, 30, 2)),    # Number of Latent Features
                 costp_l1 = c(0),       # L1 regularization cost for User factors
                 costp_l2 = c(0.01),    # L2 regularization cost for User factors
                 costq_l1 = c(0.1),     # L1 regularization cost for Beer factors
                 costq_l2 = c(0.1),     # L2 regularization cost for Beer factors
                 lrate    = c(0.1),     # Learning Rate - Aprox step size in Gradient Descent
                 nfold    = 5,          # Number of folds for Cross Validation
                 nthread  = 8,          # Set Number of Threads to 1 *** Required to get Reproducible Results ***
                 niter    = 50,         # Number of Iterations
                 verbose  = FALSE)      # Don't Show Fold Details

start <- Sys.time()
set.seed(1, sample.kind = "Rounding")
opts_tune <- r$tune(data_memory(train_set[, 1], train_set[, 2], train_set[, 3]),
                    opts = opts_list)
finish <- Sys.time()
tune_time <- finish - start

opts_tune$min

opts_tune$res %>%
  ggplot(aes(dim, loss_fun)) +
  geom_point() +
  geom_smooth(method="loess") +
  labs(x="Latent Factors (dim)", y="RMSE") +
  ggtitle("Latent Factors vs RMSE")

RECO_CV_15_30 <- opts_tune


#***********************************************************************************
# Train "Optimal" RMSE - (dim = 16)  V3 - 16 x 50
#***********************************************************************************
opts_list = list(dim      = c(16),      # Number of Latent Features
                 costp_l1 = c(0),       # L1 regularization cost for User factors
                 costp_l2 = c(0.01),    # L2 regularization cost for User factors
                 costq_l1 = c(0.1),     # L1 regularization cost for Beer factors
                 costq_l2 = c(0.1),     # L2 regularization cost for Beer factors
                 lrate    = c(0.1),     # Learning Rate - Aprox step size in Gradient Descent
                 nfold    = 5,          # Number of folds for Cross Validation
                 nthread  = 1,          # Set Number of Threads to 1 *** Required to get Reproducible Results ***
                 niter    = 50,         # Number of Iterations
                 verbose  = FALSE)      # Don't Show Fold Details

start <- Sys.time()

set.seed(1, sample.kind = "Rounding")
r$train(data_memory(train_set[, 1], train_set[, 2], train_set[, 3]),
        opts = c(opts_list,
                 nthread = 1))

finish <- Sys.time()
train_time <- finish - start

start <- Sys.time()

pred <- r$predict(data_memory(test_set[, 1], test_set[, 2], test_set[, 3]), out_memory())
finish <- Sys.time()

pred_time <- finish - start
pred_time

rmse <- RMSE(test_set[,3],pred)

RECO_rmse_results <- bind_rows(RECO_rmse_results,
                               tibble(Method="V3 - 16 x 50 (Optimal DIM)",
                                      RMSE = rmse,
                                      Train_Time = round(as.numeric(train_time, units="mins"), 2)))
RECO_rmse_results %>% knitr::kable()

#***********************************************************************************

#***********************************************************************************
# Tune & Graph Latent Factors (dim) from 100 to 1000 by 100
#***********************************************************************************

opts_list = list(dim      = c(seq(100, 1000, 100)),    # Number of Latent Features
                 costp_l1 = c(0),       # L1 regularization cost for User factors
                 costp_l2 = c(0.01),    # L2 regularization cost for User factors
                 costq_l1 = c(0.1),     # L1 regularization cost for Beer factors
                 costq_l2 = c(0.1),     # L2 regularization cost for Beer factors
                 lrate    = c(0.1),     # Learning Rate - Aprox step size in Gradient Descent
                 nfold    = 5,          # Number of folds for Cross Validation
                 nthread  = 8,          # Set Number of Threads to 1 *** Required to get Reproducible Results ***
                 niter    = 50,         # Number of Iterations
                 verbose  = FALSE)      # Don't Show Fold Details

start <- Sys.time()
set.seed(1, sample.kind = "Rounding")
opts_tune <- r$tune(data_memory(train_set[, 1], train_set[, 2], train_set[, 3]),
                    opts = opts_list)
finish <- Sys.time()
tune_time <- finish - start

opts_tune$min

opts_tune$res %>%
  ggplot(aes(dim, loss_fun)) +
  geom_point() +
  geom_smooth(method="loess") +
  labs(x="Latent Factors (dim)", y="RMSE") +
  ggtitle("Latent Factors vs RMSE")

RECO_CV_100_1000 <- opts_tune


#***********************************************************************************
# Train "Large" - V4 - 1000 x 100
#***********************************************************************************
opts_list = list(dim      = c(1000),    # Number of Latent Features
                 costp_l1 = c(0),       # L1 regularization cost for User factors
                 costp_l2 = c(0.01),    # L2 regularization cost for User factors
                 costq_l1 = c(0.1),     # L1 regularization cost for Beer factors
                 costq_l2 = c(0.1),     # L2 regularization cost for Beer factors
                 lrate    = c(0.1),     # Learning Rate - Aprox step size in Gradient Descent
                 nfold    = 5,          # Number of folds for Cross Validation
                 nthread  = 1,          # Set Number of Threads to 1 *** Required to get Reproducible Results ***
                 niter    = 100,        # Number of Iterations
                 verbose  = FALSE)      # Don't Show Fold Details

start <- Sys.time()

set.seed(1, sample.kind = "Rounding")
r$train(data_memory(train_set[, 1], train_set[, 2], train_set[, 3]),
        opts = c(opts_list,
                 nthread = 1))

finish <- Sys.time()
train_time <- finish - start

start <- Sys.time()

pred <- r$predict(data_memory(test_set[, 1], test_set[, 2], test_set[, 3]), out_memory())
finish <- Sys.time()

pred_time <- finish - start
pred_time

rmse <- RMSE(test_set[,3],pred)

RECO_rmse_results <- bind_rows(RECO_rmse_results,
                               tibble(Method="V4 - 1000 x 100",
                                      RMSE = rmse,
                                      Train_Time = round(as.numeric(train_time, units="mins"), 2)))
RECO_rmse_results %>% knitr::kable()

#***********************************************************************************
