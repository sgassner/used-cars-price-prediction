#------------------------------------------------------------------------------#
# Big Data Analytics: Group Project
# Team: Abnormal Distribution
# Created On: 26 March 2022
# Data Sets: 
# - US Used Cars: 
#   (https://www.kaggle.com/datasets/ananaymital/us-used-cars-dataset)
# - US Zip Codes: 
#   (https://www.unitedstateszipcodes.org/zip-code-database/)
#------------------------------------------------------------------------------#

# load packages
library(tidyverse)
library(data.table)
library(corrplot)
library(fastDummies)
library(mice)
library(bigmemory)
library(biganalytics)
library(biglasso)
library(MatrixModels)

# set working directory
setwd("~/Documents/R/BigData/project")

# define columns that should not be read in
drop <- c("vin",                      # car identification number / not relevant
          "bed",                      # only for pick-up truck / 99% NA
          "bed_height",               # only for pick-up truck / 86% NA
          "bed_length",               # only for pick-up truck / 86% NA
          "cabin",                    # only for pick-up truck / 98% NA
          "combine_fuel_economy",     # 100% NA
          "description",              # redundancies with major_options
          "engine_cylinders",         # redundant with engine_type
          "exterior_color",           # redundant with listing_color
          "franchise_make",           # redundant with make_name
          "interior_color",           # too many different / special colors
          "is_certified",             # 100% NA
          "is_cpo",                   # 94% NA
          "is_new",                   # redundant with year
          "is_oemcpo",                # 95% NA
          "latitude",                 # redundant with city / zip
          "listed_date",              # redundant with daysonmarket
          "listing_id",               # not relevant
          "longitude",                # redundant with city / zip
          "main_picture_url",         # not relevant
          "power",                    # redundant with horsepower
          "sp_id",                    # selling point id / not relevant
          "sp_name",                  # selling point name / not relevant
          "theft_title",              # 0% TRUE
          "torque",                   # we have horsepower and engine_displacement
          "transmission_display",     # redundant with transmission
          "trimId",                   # not relevant
          "trim_name",                # redundant with other variables
          "vehicle_damage_category",  # 100% NA
          "wheel_system_display")     # redundant with wheel_system

# load data
data <- fread("used_cars_data.csv", drop = drop, data.table = FALSE)

#------------------------------------------------------------------------------#
# Data Cleaning
#------------------------------------------------------------------------------#

# show summary statistics, data set structure and number of NAs
summary(data)
str(data)
colSums(is.na(data))

# count observations by car brand and model
data %>% count(make_name) %>% arrange(desc(n))
data %>% count(model_name) %>% arrange(desc(n))

# fill empty fields with NA
data[data == ""] <- NA
data[data == " "] <- NA
data[data == "--"] <- NA

# deal with variables in inch
variables_inch = c("back_legroom", "front_legroom", "length", "height", 
                   "wheelbase", "width")

for (i in variables_inch){
  # remove " in" after the values
  data[,i] <- sub(" in", "", data[,i])
  # convert inch to cm, change type to numeric and round values
  data[,i] <- round(as.numeric(data[,i]) * 2.54, 2)
  # change column name
  names(data)[names(data) == i] <- paste(i, "cm", sep="_")
}

# remove " seats" after the values and change to integers
data[,"maximum_seating"] <- gsub(" seats", "", data[,"maximum_seating"])
data[,"maximum_seating"] <- as.integer(data[,"maximum_seating"])

# convert fuel_tank_volume to liters
data[,"fuel_tank_volume"] <- gsub(" gal", "", data[,"fuel_tank_volume"])
data[,"fuel_tank_volume"] <- round(as.numeric(data[,"fuel_tank_volume"]) 
                                   * 3.7854, 2)
names(data)[names(data) == "fuel_tank_volume"] <- "fuel_tank_volume_l"

# convert mileage to km
data[,"mileage"] <- round(as.numeric(data[,"mileage"]) * 1.609344, 0)
names(data)[names(data) == "mileage"] <- "mileage_km"

# round seller rating
data[,"seller_rating"] <- round(data[,"seller_rating"], 2)

# convert fuel economy to liters per 100km
data[,"city_fuel_economy"] <- round((1 / data[,"city_fuel_economy"] * 100), 2)
data[,"highway_fuel_economy"] <- round((1 / data[,"highway_fuel_economy"]) 
                                       * 100, 2)

# delete rows with salvage = TRUE and change NAs to FALSE
data$salvage[is.na(data$salvage)] <- FALSE
data <- data %>% filter(salvage == FALSE)
data <- select(data, -salvage)

# match dealer_zip with state_id and remove columns dealer_zip and city
us_zip_codes <- fread("uszips.csv", select = c("zip", "state_id"), 
                      data.table = FALSE)

data$dealer_zip <- as.numeric(data$dealer_zip)
names(data)[names(data) == "dealer_zip"] <- "zip"
data <- merge(x = data, y = us_zip_codes, by = "zip", all.x = TRUE)
data <- select(data, -c(zip, city))

# clear us_zip_codes from memory
rm(us_zip_codes)

# create variable age from year and drop column year
data$age <- 2021 - data$year
data <- select(data, -year)

# convert booleans to integers
variables_boolean <- c("fleet", "frame_damaged", "franchise_dealer", 
                       "has_accidents", "isCab")

for (i in variables_boolean){
  # convert to integers
  data[,i] <- as.integer(data[,i]) 
}

# correlation plot
# find numeric or integer values
nums <- unlist(lapply(data, is.numeric))
ints <- unlist(lapply(data, is.integer))
nums_ints <- nums + ints
nums_ints[nums_ints == 2] <- 1
nums_ints <- as.logical(nums_ints)

# calculate correlations of numeric and integer variables
cor <- round(cor(na.omit(data[ ,nums_ints])), 2)

# create correlation plot
corrplot(cor)

# drop highly correlated variables (> 0.9)
data <- select(data, -c(wheelbase_cm, highway_fuel_economy))

# save clean data set (V1)
write.csv(data, "used_cars_data_clean_V1.csv", row.names = FALSE)

#------------------------------------------------------------------------------#
# NA Imputation with MICE package
#------------------------------------------------------------------------------#

# load clean data set (V1)
# data <- fread("used_cars_data_clean_V1.csv", data.table = FALSE)

# check number of missing values
colSums(is.na(data))

# imputing missing data
tempData <- mice(data, m = 1, maxit = 20, meth='pmm', seed = 500)
summary(tempData)
data <- complete(tempData, 1)

# check number of missing values again
colSums(is.na(data))

# clear tempData from memory
rm(tempData)

# save clean data set (V2)
write.csv(data, "used_cars_data_clean_V2.csv", row.names = FALSE)

#------------------------------------------------------------------------------#
# Dummy Transformation
#------------------------------------------------------------------------------#

# load clean data set (V2)
# data <- fread("used_cars_data_clean_V2.csv", data.table = FALSE)

# manipulate stings to count major options
data[,"major_options"] <- gsub("\\['", "", data[,"major_options"])
data[,"major_options"] <- gsub("\\']", "", data[,"major_options"])
data[,"major_options"] <- gsub(" / ", "_", data[,"major_options"])
data[,"major_options"] <- gsub("/", "_", data[,"major_options"])
data[,"major_options"] <- gsub("-", "_", data[,"major_options"])

# count major options
major_options <- data.frame(table(unlist(strsplit(data$major_options, "', '"))))
major_options <- major_options %>% arrange(desc(Freq))

# show the 20 most frequent car options
head(major_options, 20)

# fill NAs in major options with 0
data$major_options[is.na(data$major_options)] <- 0

# create dummies for all 145 options
major_options <- as.vector(major_options[,1])

for (i in major_options){
  data[, tolower(gsub(" ", "_", i, fixed = TRUE))] <- 
    str_detect(data$major_options, i) * 1
}

# remove option dummy quick_order_package to avoid dummy trap
data <- select(data, -quick_order_package)

# remove original major options column
data <- select(data, -major_options)

# replace spaces and special characters to create dummies
data$body_type <- gsub(" ", "_", data[,"body_type"])
data$body_type <- gsub("/", "_", data[,"body_type"])
data$engine_type <- gsub(" ", "_", data[,"engine_type"])
data$transmission <- gsub(" ", "_", data[,"transmission"])
data$fuel_type <- gsub(" ", "_", data[,"fuel_type"])

# create dummies for all columns except model_name
dummy_variables <- c("body_type", "engine_type", "fuel_type", "listing_color", 
                     "make_name", "transmission", "wheel_system", "state_id")

for (i in dummy_variables){
  # fill NAs in i with "NA"
  data[,i][is.na(data[,i])] <- "NA"
  # create dummies for i
  data <- dummy_cols(data, select_columns = i, remove_first_dummy = TRUE)
}

# remove original columns of all dummies
data <- select(data, -all_of(dummy_variables))

# create dummy variabels for model_names manually, since fastDummies-Package
# uses too much RAM (vector memory exhausted)
model_name <- data %>% count(model_name) %>% arrange(desc(n))
model_name <- as.vector(model_name[,1])

for (i in model_name){
  data[,(gsub(" ", "", paste("model_name", as.character(i), sep = "_")))] <- 
    ifelse(data[,"model_name"] == i, 1, 0)
}

# remove model_name_Renegade dummy to avoid dummy trap
data <- select(data, -model_name_Renegade)

# remove original model_name column
data <- select(data, -model_name)

# check for NAs again
sum(is.na(data))

#------------------------------------------------------------------------------#
# Variable Transformations
#------------------------------------------------------------------------------#

# log transformation of price
data$price <- log(data$price)

# add new variable: age squared
data$age_squared <- (data$age)^2

#------------------------------------------------------------------------------#
# Outlier Removal with the IQR-Method (performed worse!)
#------------------------------------------------------------------------------#

# iqr <- IQR(data$price)
# Q <- quantile(data$price, probs=c(.25, .75), na.rm = FALSE)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low <- Q[1]-1.5*iqr # Lower Range
# data <- data %>% filter(price > low & price < up)

# save data set for later analysis (V3)
write.csv(data, "used_cars_data_clean_V3.csv", row.names = FALSE)

#------------------------------------------------------------------------------#
# LASSO-Model with BigLasso
#------------------------------------------------------------------------------#

# clear R environment
rm(list = ls())

# load clean data set (V3)
data <- fread("used_cars_data_clean_V3.csv", data.table = FALSE)

# set seed for replicability
set.seed(1234)

# split data into training and test set (80/20 split)
sample_size <- floor(0.80 * nrow(data))
training_set <- sample(seq_len(nrow(data)), size = sample_size, replace = FALSE)

train <- data[training_set, ]
test <- data[-training_set, ]

# create vectors for dependent and independent variables
train_x <- select(train, -price)
train_y <- select(train, price)
test_x <- select(test, -price)
test_y <- select(test, price)

# clear tempData from memory
rm(data, test, train, sample_size, training_set)

# write csv's if they do not already exist
if(!file.exists('used_cars_train_x.csv')) {
  write.csv(train_x, "used_cars_train_x.csv", row.names = FALSE)
}

if(!file.exists('used_cars_train_y.csv')) {
  write.csv(train_y, "used_cars_train_y.csv", row.names = FALSE)
}

if(!file.exists('used_cars_test_x.csv')) {
  write.csv(test_x, "used_cars_test_x.csv", row.names = FALSE)
}

if(!file.exists('used_cars_test_y.csv')) {
  write.csv(test_y, "used_cars_test_y.csv", row.names = FALSE)
}

# set up a file-backed big.matrix for train_x if not already done
if(!file.exists('used_cars_train_x.bin')) {
  train_x.bm <- setupX("used_cars_train_x.csv", header = T)
}

# retrieve train_x.bm and train_y
train_x.bm <- attach.big.matrix("used_cars_train_x.desc")
train_y <- read.csv("used_cars_train_y.csv")[,1]

# fit LASSO model with 5-fold cross validation in parallel
system.time({cvfit <- cv.biglasso(train_x.bm, train_y, seed = 1234, 
                                  ncores = 10, nfolds = 5, trace = TRUE)})

# save model for later use
saveRDS(cvfit, "used_cars_lasso_model.rds")

# load model
cvfit <- readRDS("used_cars_lasso_model.rds")

# save coefficients
coef <- coef(cvfit)

# find number of coefficients > 0 (minus 1 for intercept)
length(coef[which(coef != 0)]) - 1

# export csv with coefficients = 0
df_coef <- as.data.frame(as.matrix(coef))
names(df_coef)[1] <- "value"
df_coef_zero <- df_coef %>% filter(value == 0)
write.csv(df_coef_zero, "lasso_coef_zero.csv", row.names = TRUE)

# create CV plots
par(mfrow = c(2, 2), mar = c(3.5, 3.5, 3, 1), mgp = c(2.5, 0.5, 0))
plot(cvfit, type = "all")

# set up a file-backed big.matrix for test_x if not already done
if(!file.exists('used_cars_test_x.bin')) {
  test_x.bm <- setupX("used_cars_test_x.csv", header = T)
}

# retrieve test_x.bm and test_y
test_x.bm <- attach.big.matrix("used_cars_test_x.desc")
test_y <- read.csv("used_cars_test_y.csv")[,1]

# make predictions with test_x.bm
fit_lasso <- predict(cvfit, test_x.bm, lambda = "lambda.min")

# calculate out-of-sample performance
R2_lasso_out <- 1 - (sum((test_y - fit_lasso)^2)/sum((test_y - mean(test_y))^2))

#------------------------------------------------------------------------------#
# OLS-Model with only 5% of the data
#------------------------------------------------------------------------------#

# clear R environment
rm(list = ls())

# load clean data set (V3)
data <- fread("used_cars_data_clean_V3.csv", data.table = FALSE)

# set seed for replicability
set.seed(1234)

# split data into training and test set (5/95 split)
sample_size <- floor(0.05 * nrow(data))
training_set <- sample(seq_len(nrow(data)), size = sample_size, replace = FALSE)

train_small <- data[training_set, ]
test_small <- data[-training_set, ]

# fit small OLS model
ols_small <- lm(price ~ ., data = train_small)

# view summary of small OLS
summary(ols_small)

# make predictions on the test set
pred_small <- predict(ols_small, newdata = test_small)

# calculate out-of-sample performance
R2_ols_small_out <- 1 - (((sum((test_small$price - pred_small)^2))/
                            (sum((test_small$price - mean(test_small$price))^2))))

#------------------------------------------------------------------------------#
# OLS-Model with only 5% of the data without car models (no models = nm)
#------------------------------------------------------------------------------#

# clear R environment
rm(list = ls())

# load clean data set (V3)
data <- fread("used_cars_data_clean_V3.csv", data.table = FALSE)

# remove model dummies
data <- data[,1:395]

# add age_squared again
data$age_squared <- (data$age)^2

# set seed for replicability
set.seed(1234)

# split data into training and test set (5/95 split)
sample_size <- floor(0.05 * nrow(data))
training_set <- sample(seq_len(nrow(data)), size = sample_size, replace = FALSE)

train_nm_small <- data[training_set, ]
test_nm_small <- data[-training_set, ]

# fit small OLS model
ols_nm_small <- lm(price ~ ., data = train_nm_small)

# view summary of small OLS
summary(ols_nm_small)

# make predictions on the test set
pred_nm_small <- predict(ols_nm_small, newdata = test_nm_small)

# calculate out-of-sample performance
R2_ols_nm_small_out <- 1 - (((sum((test_nm_small$price - pred_nm_small)^2))/
                            (sum((test_nm_small$price - mean(test_nm_small$price))^2))))

#------------------------------------------------------------------------------#
# OLS without car models (no models = nm)
#------------------------------------------------------------------------------#

# clear R environment
rm(list = ls())

# load clean data set (V3)
data <- fread("used_cars_data_clean_V3.csv", data.table = FALSE)

# remove model dummies
data <- data[,1:395]

# add age_squared again
data$age_squared <- (data$age)^2

# set seed for replicability
set.seed(1234)

# split data into training and test set (80/20 split)
sample_size <- floor(0.8 * nrow(data))
training_set <- sample(seq_len(nrow(data)), size = sample_size, replace = FALSE)

train_nm <- data[training_set, ]
test_nm <- data[-training_set, ]

# fit OLS model and record computation time
system.time({ols_nm <- lm(price ~ ., data = train_nm)})

# view summary of OLS
summary(ols_nm)

# make predictions on the test set
pred_nm <- predict(ols_nm, newdata = test_nm)

# calculate out-of-sample performance
R2_ols_nm_out <- 1 - (((sum((test_nm$price - pred_nm)^2))/
                            (sum((test_nm$price - mean(test_nm$price))^2))))

#------------------------------------------------------------------------------#
# OLS-Model with ULURU
#------------------------------------------------------------------------------#

# # REMARK: 
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # since we have a very wide data frame with many dummy variables,
# # there are a lot of columns where all rows are equal to zero,
# # what leads to multicollinearity (singular matrix)
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# The model
# beta_uluru <-
#   function(X_subs, y_subs, X_rem, y_rem) {
# 
#     # compute beta_fs (this is simply OLS applied to the subsample)
#     XXi_subs <- solve(crossprod(X_subs, X_subs))
#     Xy_subs <- crossprod(X_subs, y_subs)
#     b_fs <- XXi_subs  %*% Xy_subs
# 
#     # compute \mathbf{R}_{rem}
#     R_rem <- y_rem - X_rem %*% b_fs
# 
#     # compute \hat{\beta}_{correct}
#     b_correct <- (nrow(X_subs)/(nrow(X_rem))) * XXi_subs %*%
#       crossprod(X_rem, R_rem)
# 
#     # beta uluru
#     return(b_fs + b_correct)
#   }
# 
# # load train data
# X <- as.matrix(fread("used_cars_train_x.csv", data.table = FALSE))
# y <- as.matrix(fread("used_cars_train_y.csv", data.table = FALSE))
# 
# # add intercept
# X <- cbind(1, X)
# 
# # load test data
# X_test <- as.matrix(fread("used_cars_test_x.csv", data.table = FALSE))
# y_test <- as.matrix(fread("used_cars_test_y.csv", data.table = FALSE))
# 
# # add intercept
# X_test <- cbind(1, X_test)
# 
# # set size of subsample
# n_subs <- 1000 # 1000 was example of Matter
# 
# # select subsample and remainder
# n_obs <- nrow(X)
# X_subs <- X[1L:n_subs,]
# y_subs <- y[1L:n_subs]
# X_rem <- X[(n_subs+1L):n_obs,]
# y_rem <- y[(n_subs+1L):n_obs]
# 
# # apply the uluru estimator
# betas_uluru <- beta_uluru(X_subs, y_subs, X_rem, y_rem)
# 
# # make predictions with the test set
# pred_uluru <-  X_test %*% betas_uluru
# 
# # calculate out of sample performance
# R2_uluru_out <- 1 - (sum((y_test - pred_uluru)^2)/
#                        sum((y_test - mean(y_test))^2))

#------------------------------------------------------------------------------#
# OLS with sparse matrix (MatrixModels package)
#------------------------------------------------------------------------------#

# # REMARK: 
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # Due to memory limitations we were not able to perform ols with all car 
# # models / dummies with sparse matrices
# # ('R_Realloc' could not re-allocate memory)
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# # clear R environment
# rm(list = ls())
# 
# # load clean data set (V3)
# data <- fread("used_cars_data_clean_V3.csv", data.table = FALSE)
# 
# # set seed for replicability
# set.seed(1234)
# 
# # split data into training and test set (80/20 split)
# sample_size <- floor(0.80 * nrow(data))
# training_set <- sample(seq_len(nrow(data)), size = sample_size, replace = FALSE)
# 
# train <- data[training_set, ]
# test <- data[-training_set, ]
# 
# # find names of columns with sum equal to zero in train set
# i <- (colSums(train, na.rm=T) == 0)
# names_i <- colnames(train[, i])
# 
# # find names of columns with sum equal to zero in test set
# j <- (colSums(test, na.rm=T) == 0)
# names_j <- colnames(test[, j])
# 
# # remove columns with sum equal to zero in train & test set
# train <- select(train, -names)
# test <- select(test, -names)
# train <- select(train, -names_j)
# test <- select(test, -names_j)
# 
# # add intercept
# train <- cbind(1, train)
# test <- cbind(1, test)
# 
# # create sparse matrices
# y_train <- train$price
# x_train <- as.matrix(select(train, -price))
# X_train <- as(x_train, "sparseMatrix")
# 
# y_test <- test$price
# x_test <- as.matrix(select(test, -price))
# X_test <- as(x_test, "sparseMatrix")
# 
# # calculate ols coefficients with sparse matrix
# coef_sparse <- MatrixModels:::lm.fit.sparse(X, y)
# 
# # make predictions with the test set
# pred_sparse <-  X_test %*% coef_sparse
# 
# # calculate out of sample performance
# R2_sparse <- 1 - (sum((y_test - pred_sparse)^2)/
#                     sum((y_test - mean(y_test))^2))

#------------------------------------------------------------------------------#
# End of Script
#------------------------------------------------------------------------------#
