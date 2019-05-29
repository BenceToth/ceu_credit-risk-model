library(data.table)

data <- fread("data/repayment_data.csv")

data[, first_month := min(month), by = ID]

month_since_reference <- function(d) {
  lt <- as.POSIXlt(d)
  lt$year * 12 + lt$mon
}
month_difference <- function(d1,d2) {
  month_since_reference(d2) - month_since_reference(d1)
}

data[, month_number := month_difference(first_month, month) + 1]
data[, month_to_use := pmin(month_number, 6)]

library(zoo)

# Past Behavior -----
data <- data[order(ID, -month)]
data[, max_status_6m := rollapply(status, month_to_use, FUN = max, align = "left")]
data[, delayed := max_delay > 0]
data[, nr_delays_6m := rollapply(delayed, month_to_use, FUN = sum, align = "left")]
data[, avg_delay_6m := rollapply(max_delay, month_to_use, FUN = mean, align = "left")]
data[, max_delay_6m := rollapply(max_delay, month_to_use, FUN = max, align = "left")]
data[, c("first_month", "month_to_use") := NULL]

# keep last month data
data <- data[month == "2008-02-29" & status < 5, ]
data[, repayment_month_over_6 := ifelse(month_number > 6, 1, 0)]

# read customer data
customer_data <- fread("data/customer_data.csv")

setkey(customer_data, ID)
setkey(data, ID)

all_data <- merge(data, customer_data)

all_data[, customer_age := month_difference(paste(substr(all_data$birth_date, 7, 10),
                                                 substr(all_data$birth_date, 1, 2),
                                                 substr(all_data$birth_date, 4, 5), 
                                                 sep="-"), month) / 12
         ]

all_data[, car_used := ifelse(car_age < 12, 0, 1)]
brands_to_keep <- readRDS("brands.rds")
all_data[, brand := ifelse(brand %in% brands_to_keep, brand, "OTHER")]

columns_to_convert <- c("repayment_month_over_6", "debt_amount", "payment_type",
                        "brand", "engine_capacity_cat", "residence_type", "region", "car_used")
all_data[, (columns_to_convert) := lapply(.SD, as.factor), .SDcols = columns_to_convert]

# read model
model <- readRDS("credit_model.rds")
all_data_with_probabilities <- (cbind(all_data, predict(model, all_data, type = "response")))

# add capital values
capital_data <- fread("data/capital.csv")

setkey(all_data_with_probabilities, ID)
setkey(capital_data, ID)

data_with_capitals <- merge(all_data_with_probabilities, capital_data)

data_with_capitals[, required_provision := V2 * capital]

# required provision
sum(data_with_capitals$required_provision)
