rm(list = ls())
library(data.table)

library(ggplot2)
library(Hmisc)
library(MASS)
library(pROC)

data <-fread('data/repayment_data.csv')
dim(data)
summary(data) # no delay -> 1, small delays -> 2, ... after 90 days -> 5 (default)

unique(data$month) # last days of the months to identify the month.
head(data)

# add first month for each customer
data[, first_month := min(month), by = ID]

# calculate the difference between the actual and first month
month_since_reference <- function(d) {
  lt <- as.POSIXlt(d)
  lt$year * 12 + lt$mon
}
month_difference <- function(d1, d2) {
  month_since_reference(d2) - month_since_reference(d1)
}

data[, month_number := month_difference(first_month, month) + 1]
head(data, 10)

# calculate window size: pairwise minimum (not the whole column)
data[, month_to_use := pmin(month_number, 6)]
head(data, 10)

# calculate past behaviour using window
data <- data[order(ID, -month)]

## max status in the past six months
library(zoo)
data[, max_status_6m := rollapply(status, month_to_use, FUN = max, align = "left")]
data[, delayed := max_delay > 0]
data[, nr_delays_6m := rollapply(delayed, month_to_use, FUN = sum, align = "left")]
data[, avg_delay_6m := rollapply(max_delay, month_to_use, FUN = mean, align = "left")]
data[, max_delay_6m := rollapply(max_delay, month_to_use, FUN = max, align = "left")]

# create target variable

## last month of customer
data[, last_month := max(month_number), by = ID]
## window size for target
data <- data[order(ID, month)]
data[, month_to_use_target := pmin(last_month - month_number + 1, 7)]
data[, max_status_future := rollapply(status, month_to_use_target, FUN = max, align = "left")]
data[max_status_future == 5, default := 1]
data[max_status_future < 5, default := 0]
summary(data)

# data cleaning
data[, c("first_month", "last_month", "month_to_use", "month_to_use_target", "max_status_future") := NULL]

range(data$month) # we have to drop all the data where default is not 1, remove last 6 months
data <- data[month <= "2007-08-31" | default == 1, ]
summary(data)

# are there any missing values? --> No
sapply(data, function(x) sum(is.na(x)))

# indicate if 6 months passed or not
data[, repayment_month_over_6 := ifelse(month_number > 6, 1, 0)]

# ADD CUSTOMER DATA
library(skimr)
customer_data <- fread("data/customer_data.csv")
dim(customer_data)
summary(customer_data)
head(customer_data) %>% View()
skim(customer_data)

# join data
setkey(customer_data, ID)
setkey(data, ID)

all_data <- merge(data, customer_data)

# FEATURE ENGINEERING

#customer age
all_data[, customer_age := month_difference(paste(substr(all_data$birth_date, 7, 10),
                                                  substr(all_data$birth_date, 1, 2),
                                                  substr(all_data$birth_date, 4, 5),
                                                  sep = "-"), month) / 12]

library(ggplot2)
ggplot(all_data, aes(x = customer_age)) +
  geom_histogram()

ggplot(all_data, aes(x = car_age)) +
  geom_histogram()

all_data[, car_used := ifelse(car_age < 12, 0, 1)]

# TRAINING TEST PARTITIONS
ids <- unique(all_data$ID)
sample <- sort(sample(length(ids), length(ids) * 0.8))

train_ids <- ids[sample]
training_set <- all_data[ID %in% train_ids, ]
test_set <- all_data[!(ID %in% train_ids), ]

# Did we lose any records? --> No
dim(training_set)[1] + dim(test_set)[1] == dim(all_data)[1]

# calculate the ratio of the default customers within a set
default_customer_ratio <- function(input_df) {
  mean(input_df[, max(default), by = .(ID)]$V1)
}

default_customer_ratio(training_set)
default_customer_ratio(test_set)

# Brands

brands <- training_set[, .(default_ratio = mean(default), count = .N), by = .(brand)]

brands[order(-count), ]
brands[order(-default_ratio), ]

brands2 <- training_set[, .(default_ratio = mean(default), count = .N), by = .(brand, car_used)]
brands2[order(-default_ratio)]

brands_to_keep <-  brands[count > 10000, ]$brand
saveRDS(brands_to_keep, file = 'brands.rds')

training_set[, brand := ifelse(brand %in% brands_to_keep, brand, "OTHER")]
training_set[, .(default_ratio = mean(default), count = .N), by = .(brand)]

test_set[, brand := ifelse(brand %in% brands_to_keep, brand, "OTHER")]
test_set[, .(default_ratio = mean(default), count = .N), by = .(brand)]

# data exploration
library(Hmisc)
rcorr(as.matrix(training_set[, .(default, max_delay, max_status_6m, max_delay_6m,
                                 month_number, delayed, customer_age, car_age, car_used)]))

training_df <- training_set[, .(default,
                                max_delay, status, month_number,
                                max_status_6m, nr_delays_6m, avg_delay_6m, max_delay_6m, repayment_month_over_6,
                                debt_amount, own_resource, duration, payment_type, car_age, brand, engine_capacity_cat,
                                residence_type, region, customer_age, car_used)]

# convert characters to factors
sapply(training_df, class)

columns_to_convert <- c("default", "repayment_month_over_6", "debt_amount", "payment_type", "brand", "engine_capacity_cat",
     "residence_type", "region", "car_used")

training_df[, (columns_to_convert) := lapply(.SD, as.factor), .SDcols = columns_to_convert]
test_set[, (columns_to_convert) := lapply(.SD, as.factor), .SDcols = columns_to_convert]

# BUILDING A MODEL
model <- glm(default ~ ., data = training_df, family = binomial(link = "logit"))

summary(model)
coef(model)


library(caret)
varimp <- varImp(model)
varimp

# Predictions
train_prediction <- (cbind(training_df, predict(model, training_df, type = "response")))[, .(default, V2)]
test_prediction <- (cbind(test_set, predict(model, test_set, type = "response")))[, .(default, V2)]

# ROC, AUC
library(pROC)
plot(roc(train_prediction$default, train_prediction$V2), print.auc = TRUE)
plot(roc(test_prediction$default, test_prediction$V2), print.auc = TRUE)

# Kolmogorov-Smirnov
KS_transform <-  function(df) {
  good <- df[default == 0, .(V2)]
  bad  <- df[default == 1, .(V2)]
  
  good_score_counts <- good[, .(good_count = .N), by = .(V2)]
  bad_score_counts  <- bad[,  .(bad_count = .N), by = .(V2)]
  
  score_count <- merge(good_score_counts, bad_score_counts, all = TRUE)
  score_count[is.na(score_count)] <- 0
  
  score_count[, good_cumsum := cumsum(good_count)]
  score_count[, bad_cumsum  := cumsum(bad_count)]
  
  score_count[, good_ratio := good_cumsum / sum(good_count)]
  score_count[, bad_ratio  := bad_cumsum  / sum(bad_count)]
  
  score_count[, diff := good_ratio - bad_ratio]
  
  return(score_count)
}

# KS on the train set
ks_training <- KS_transform(train_prediction)

cut_off <- ks_training[diff == max(ks_training$diff), ][1]$V2

ggplot() +
  geom_line(data = ks_training, aes(x = V2, y = good_ratio), color = "blue") +
  geom_line(data = ks_training, aes(x = V2, y = bad_ratio),  color = "red") +
  geom_vline(xintercept = cut_off)



# KS on the test set
ks_test <- KS_transform(test_prediction)


ggplot() +
  geom_line(data = ks_test, aes(x = V2, y = good_ratio), color = "blue") +
  geom_line(data = ks_test, aes(x = V2, y = bad_ratio),  color = "red") +
  geom_vline(xintercept = cut_off)

ks_test[abs(V2-cut_off) < 0.00001, ]

# LIFT
lift_values <- function(df, nr) {
  df$default <- as.numeric(df$default) - 1
  ordered_df <- df[order(-V2)]
  bin <-  dim(ordered_df)[1] / nr
  ordered_df[, bin := floor(index(ordered_df) / bin)]
  
  lift_val_df <- ordered_df[, .(default_sum = sum(default)), by = .(bin)]
  lift_val_df[, default_cumsum := cumsum(default_sum)]
  lift_val_df[, random_expected := sum(df$default) * (bin + 1) / nr]
  
  lift_val_df[, lift_value := default_cumsum / random_expected]
  lift_val_df[, contact_percent := bin * (100 / nr)]
  
  return(lift_val_df)
}

# lift train
lift_training <- lift_values(train_prediction, 20)

ggplot(data = lift_training, aes(x = contact_percent, y = lift_value, group = 1)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "blue")

# lift test
lift_test <- lift_values(test_prediction, 20)

ggplot(data = lift_test, aes(x = contact_percent, y = lift_value, group = 1)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "blue")

# ROI (with random profit)
roi_transform <- function(df) {
  roi_df <- df[order(-V2)]
  roi_df[, cost := 100]
  roi_df[, potential_win := runif(dim(roi_df)[1], 200, 1000)] # random values between 200-1000
  roi_df[, win := ifelse(default == 1, potential_win - cost, 0 - cost)]
  roi_df[, roi := cumsum(win)]
  roi_df[, percentile := 100 * as.numeric(rownames(roi_df)) / dim(roi_df)[1]]
  
  return(roi_df)
}

roi_train <- roi_transform(train_prediction)
roi_optimal_score <- roi_train[roi == max(roi_train$roi), .(V2)]

ggplot(data = roi_train, aes(x = V2, y = roi, group = 1)) +
  geom_line() +
  geom_point() +
  scale_x_reverse() +
  geom_vline(xintercept = roi_optimal_score$V2)

ggplot(data = roi_train, aes(x = percentile, y = roi, group = 1)) +
  geom_line() +
  geom_point()

roi_test <- roi_transform(test_prediction)
ggplot(data = roi_test[sample(.N, 10000)], aes(x = percentile, y = roi)) +
  geom_line() +
  geom_point()

# Predicting the required provision value (random capital)
test_prediction[, capital := runif(dim(test_prediction)[1], 200000, 1000000)]

test_prediction[, req_provision := ifelse(default == 0, 0, capital)]
test_prediction[, predicted_provision := capital * V2]

sum(test_prediction$predicted_provision) / sum(test_prediction$req_provision) * 100

# Save the model
saveRDS(model, file = 'credit_model.rds')

model2 <- readRDS('credit_model.rds')
