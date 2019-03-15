library(stringr)
library(tidyr)
library(dplyr)

file_names <- list.files("./Data", full.names = T)  #get filenames in folder "Data", change directory accordingly
#####################################################################################################################
##make sure to put only the files u need in the folder, cause "file_names" will be used later, extra files will cause
#inconsistency and errors
#####################################################################################################################


#asset_value is the FTSE_100 index values (stock price) for each date
asset_value <- read.csv("./FTSE\\ftse.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, colClasses = c("NULL", NA, "NULL"))
asset_value <- as.numeric(asset_value[-c(2:length(asset_value)),])

#a list for dataFrames of different strike prices of CALL or PUT options
daily_prices <- list()


for(ii in seq(from = 1, to = length(file_names))){  #for ii in range 1 to 2 (call.csv and put.csv)
  tmp1 <- read.csv(file_names[ii], header = TRUE, sep = ",", stringsAsFactors = FALSE)
   #stringsAsFactors=FALSE to ensure extraction of column headers as strings
  tmp1 <- tmp1[-c(1),]  #delete the first row, since its useless info
  callnames <- colnames(tmp1)  #get columnnames as vector
  tmp1[, ] <- sapply(tmp1[, ], as.numeric)  #sapply applies "numeric" format to all column values, values is those below header
  #side note: there are various formats, e.g. character, factors, etc..
  names <- str_remove(callnames, ".ESX.JAN19.")  #remove the words in between
  names <- names[-1]  #remove first column of names vector, because its date, not strike prices
  names(tmp1) <- c("date",names)  #names of dataFrame is "date", followed by names vector
  tmp1$date <- tmp1$date - min(tmp1$date)  #calibrate dates value to start from 0 to....
  rownames(tmp1) <- NULL  #remove rownames as its an extra column

  option_name <- str_extract(names(tmp1), "\\D{3,4}[0-9]{4}")  #actually does nothing, but tries to extract
  #a pattern of four characters (\D), followed by numbers [0-9], four numbers {4}.

  for (jj in seq(from = 2, to = length(tmp1))){  #for each file, now extract all data related to
                                                #each strike prices and create a dataFrame
    tmp <- data.frame(matrix(0, ncol = 4, nrow = nrow(tmp1)))  #create a dataFrame of 4 columns, and rows same as tmp1, as
                                                              #tmp1 has rows of length of time series
    colnames(tmp) <- c("date", "option_price", "asset_value", "id")  #set column names
    tmp$date <- tmp1$date  #assign dates to date column
    tmp$option_price <- as.numeric(tmp1[,jj])  #assign option prices for each strike price
    tmp$id <- names(tmp1)[jj]  #assign the name of strike price e.g.(CALL9200) as id, basically all rows will have same id
                               #since each dataFrame is of one strike price (e.g. CALL9200)
    tmp$asset_value <- asset_value

    daily_prices[[option_name[jj]]] <- tmp  #push dataFrame to list
  }
}


# Merge together
tidy_data <- Reduce(function(...) rbind(...), daily_prices)  #bind/compress all dataFrames together as one big dataFrame
tidy_data$id <- as.factor(tidy_data$id)  #set id column as Factor variables
tidy_data
tidy_data <- mutate(tidy_data, putcall = str_extract(id, "[CALL,PUT]{3,4}"),
                    strike_price = as.numeric(str_extract(id, "[0-9]{4}")))
#mutate enables us to insert a new column to dataFrame, we insert "putcall" column, which consist of CALL/PUT option
#for each option price (e.g. CALL for option price ??? at date ???, id ???)
tidy_data

data_wide <- tidy_data %>%  # %>% is like object functions in Python, like x.add()
    select(-c(putcall, strike_price)) %>%  #for data_wide (later used for joining), delete putcall and strike_price columns
    spread(id, option_price)  #and then spread, spread maps key to value, which in this case maps each id to option_price
                              #which means tidy up the messy data so that we have columns of id, with their corresponding option_price
                              #this is essentially the same format as when we do read.csv at the start
data_wide
asset <- data.frame(date = data_wide$date,  #asset to store asset_value, which is stock prices (FTSE100_index values)
                   value = data_wide$asset_value)
asset
t_mat <- length(asset$date) + 1  #get length of time series
t_quarter <- ceiling(t_mat/4)  #get T/4 (time series length/ 4)

asset$lag_date <- asset$date - t_quarter  #minus off t_quarter so later on we can see where to start measuring volatility
                                          #in essence, lag_date for date = 0 will be e.g. -56, up to one point it will be 0,
                                          #that will be our starting point for measuring volatility
index <- asset$lag_date < 0  #indexes where lag_date < 0, then set to NA
asset$lag_date[index] <- NA
asset$log_returns <-  c(NA, diff(log(asset$value)))  #calculate log_returns for asset_value, diff does the difference
                                                     #between this and the next element

asset

# Volatilities and standard error along window
volatilities_raw <- apply(asset, 1, function(x) {  #apply a function which takes "asset" as "x"

    # Account for gaps of up to 4 days
    if (!(x[["lag_date"]] %in% asset$date)) {  #this part gets the which/what lag_date is present in date and store
        for (i in 1:4) {
            if ((x[["lag_date"]] + i) %in% asset$date) {
                x[["lag_date"]] <- x[["lag_date"]] + i
                break
            }
            if (i == 4) return(list(volatility = NA, volatility_se = NA))
        }
    }

    # Compute time window
    min <- which(asset$date == x[["lag_date"]])  #get indexes for lag_date starting from 0 to ...
    max <- which(asset$date == x[["date"]])  #get indexes for the corresponding date
    #example: lag_date = 0, index = 56, corresponding date is date = 56. What index is date = 56?
    window <- asset$log_returns[min:max]  #get the log returns between this t - T/4 window (56 - 56 = 0)

    #
    s <- sd(window, na.rm = T)  #get standard deviation for this window of data
    tau <- ((asset$date[max] - asset$date[min]) / length(window)) / 365
    vol <- s / sqrt(tau) # volatility
    vol_se <- vol / sqrt(2 * length(window)) # standard error
    return(list(volatility = vol, volatility_se = vol_se))
})

volatilities <- volatilities_raw %>%
    unlist() %>%
    matrix(nrow = 2) %>%
    t() %>%
    as.data.frame()
names(volatilities) <- c("volatility", "volatility_se")  #convert to matrix
summary(volatilities)

volatilities


asset <- bind_cols(asset, volatilities)  #bind column wise

asset
# Combine volatility output into asset dataframe (and wide option values)
asset <- left_join(asset, data_wide)  #left join
tidy_data <- full_join(select(asset, date, volatility, volatility_se), tidy_data)
#full join
tidy_data

train_ii <- !is.na(asset$volatility)

train_ii
save(asset, tidy_data, train_ii, file = "tidy_data.RData")
