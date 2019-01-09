col_classes <- c('VendorID' = "factor",
                 'tpep_pickup_datetime' = "character",
                 'tpep_dropoff_datetime' = "character",
                 'passenger_count' = "integer",
                 'trip_distance' = "numeric",
                 'pickup_longitude' = "numeric",
                 'pickup_latitude' = "numeric",
                 'RateCodeID' = "factor",
                 'store_and_fwd_flag' = "factor",
                 'dropoff_longitude' = "numeric",
                 'dropoff_latitude' = "numeric",
                 'payment_type' = "factor",
                 'fare_amount' = "numeric",
                 'extra' = "numeric",
                 'mta_tax' = "numeric",
                 'tip_amount' = "numeric",
                 'tolls_amount' = "numeric",
                 'improvement_surcharge' = "numeric",
                 'total_amount' = "numeric")

input_csv <- 'yellow_tripdata_2016-01.csv'

input_csv <- 'yellow_tripdata_2016-01.csv'

# we take a chunk of the data and load it as a data.frame (good for testing things)
nyc_sample_df <- read.csv('D:/迅雷下载/yellow_tripdata_2018-01.csv', nrows = 1000)
head(nyc_sample_df)