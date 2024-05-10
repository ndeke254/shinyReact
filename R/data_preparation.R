# Import the necessary packages
library(data.table)
library(nycflights13)

# Process flights data
flights <- as.data.table(nycflights13::flights)

# Check the head of the flights data
head(flights)

# Check for NA values
na_counts <- lapply(flights, function(x) sum(is.na(x)))
na_summary <- data.table(column = names(flights), na_count = unlist(na_counts))

# Print the summary of NA values
#print(na_summary)

# Remove rows with NA values
flights <- na.omit(flights)

# Filter columns of interest
flights <- flights[, .(month, day, arr_delay, dep_delay, origin)]

# Check the head of the processed data
head(flights)
