## Project Submission Generation

library(tidyverse) # metapackage of all tidyverse packages
library(readr) # importing data
library(forecast) # time series
library(foreach) # parallel computing
library(doParallel)
## FUNCTIONS

## Data filter
data_creator <- function(df, ...){
  output <- df %>% group_by(...) %>% 
    summarize_at(.vars = vars,
                 .funs = c(sum="sum"))
  return(output)
}

## Model Definition
single_series_model <- function(model_data, fact_1, fact_2, file){
  
  simple_series <- select(model_data, sales, date)
  
  mod_xts <- msts(simple_series$sales, seasonal.periods = 7)
  
  fit <- auto.arima(mod_xts)
  
  percentiles <- c(50, 67, 95, 99)
  fc <- forecast(fit, h = 28, level = percentiles)
  
  
  nums <- 1:28
  paste0("F", nums)
  row_labs<- paste0("F", nums)
  rownames(fc$lower) <- row_labs
  rownames(fc$upper) <- row_labs
  
  res <- cbind(fc$lower[,(ncol(fc$lower)):1], fc$mean ,fc$upper[,1:(ncol(fc$upper))])
  
  #res
  
  col_vals <- c("0.005", "0.025", "0.165", "0.250", "0.500", "0.750", "0.835", "0.975", "0.995")
  
  
  for (i in 1:length(col_vals)){
    col_vals[i] <- paste0(fact_1, "_", fact_2, "_", col_vals[i], "_", file)
  }
  
  
  colnames(res) <- col_vals
  
  rownames(res) <- row_labs
  output <- t(res)
  return(output)
}

## Builds all models for each data section
data_builder <- function(base_data, calendar_data, expected_cols, file){
  
  test_val <- ncol(base_data) - expected_cols
  if(test_val == 0){
    fact_1 <- "Total"
    fact_2 <- "X"
  } else if(test_val == 1){
    fact_1 <- unlist(unique(base_data[,1]))
    fact_2 <- "X"
  } else if(test_val == 2){
    fact_1 <- unlist(unique(base_data[,1]))
    fact_2 <- unlist(unique(base_data[,2]))
  }
  
  results_list <- list()
  counter = 1
  for (i in 1:length(fact_1)){
    for (j in 1:length(fact_2)){
      
      if(test_val == 0){
        one_series <- base_data[counter,]
      } else if(test_val == 1){
        one_series <- base_data[counter,2:ncol(base_data)]
      } else if(test_val == 2){
        one_series <- base_data[counter,3:ncol(base_data)]
      }
      
      one_series_long <- one_series %>%  
        pivot_longer(cols = starts_with("d_"),
                     names_to = "day",
                     names_prefix = "d_",
                     values_to = "sales") %>% 
        mutate(day = str_replace(day, "_sum", ""))
      
      model_data <- left_join(one_series_long, calendar_data, by = "day")
      
      #results_list[[counter]] <- single_series_model(model_data, fact_1[i], fact_2[j], file = file)
      results_list[[counter]] <- list(data = model_data,
                                      factor_1 = fact_1[i],
                                      factor_2 = fact_2[j],
                                      file = file)
      counter = counter + 1
    }
  }
  
  #final_vals <- do.call(rbind, results_list)
  
  return(results_list)
}

## Timing for testing:
start_time <- Sys.time()


## IMPORT FILES.
## Files commented out to save memory
path <- "data"
calendar <- read_csv(file.path(path,"calendar.csv"))
sales_train_evaluation <- read_csv(file.path(path, "sales_train_evaluation.csv"))
sales_train_validation <- read_csv(file.path(path, "sales_train_validation.csv"))
sample_submission <- read_csv(file.path(path, "sample_submission.csv"))
sell_prices <- read_csv(file.path(path, "sell_prices.csv"))

col_tf <- colnames(sales_train_validation) %>% stringr::str_detect("d_")
vars <- colnames(sales_train_validation)[col_tf]


L1 <-  data_creator(sales_train_validation)
L2 <-  data_creator(sales_train_validation, state_id)
L3 <-  data_creator(sales_train_validation, store_id)
L4 <-  data_creator(sales_train_validation, cat_id)
L5 <-  data_creator(sales_train_validation, dept_id)
L6 <-  data_creator(sales_train_validation, state_id, cat_id)
L7 <-  data_creator(sales_train_validation, state_id, dept_id)
L8 <-  data_creator(sales_train_validation, store_id, cat_id)
L9 <-  data_creator(sales_train_validation, store_id, dept_id)
#L10 <- data_creator(sales_train_validation, item_id)
#L11 <- data_creator(sales_train_validation, item_id, state_id)
#L12 <- data_creator(sales_train_validation, item_id, store_id)

data_level_list <- list(L1, L2, L3, L4, L5, L6, L7, L8, L9) #, L10, L11, L12)

calendar <- mutate(calendar, day = str_replace(d, "d_", ""),
                   sporting_event = case_when(event_type_1 == "Sporting" | event_type_2 == "Sporting" ~ 1,
                                              TRUE ~ 0),
                   cultural_event = case_when(event_type_1 == "Cultural" | event_type_2 == "Cultural" ~ 1,
                                              TRUE ~ 0),
                   national_event = case_when(event_type_1 == "National" | event_type_2 == "National" ~ 1,
                                              TRUE ~ 0),
                   religious_event = case_when(event_type_1 == "Religious" | event_type_2 == "Religious" ~ 1,
                                               TRUE ~ 0)) %>%
  select(day, wday, date, snap_CA, snap_TX, snap_WI, sporting_event, cultural_event, national_event, religious_event) %>%
  select(day, date)

data_list <- list()
for (i in 1:length(data_level_list)){
  
  data_list[[i]] <- data_builder(data_level_list[[i]], calendar, ncol(L1), "validation")
}

registerDoParallel(cores=4)

output_results <- list()
for (j in 1: length(data_list)){
  output_results[[j]] <- foreach(i = 1:length(data_list[[j]]),
                            .combine = 'rbind',
                            .verbose = FALSE,
                            .packages = c('xts', 'forecast', 'dplyr')) %dopar% single_series_model(data_list[[j]][[i]]$data, 
                                                        data_list[[j]][[i]]$factor_1, 
                                                        data_list[[j]][[i]]$factor_2, 
                                                        data_list[[j]][[i]]$file)
}


csv_results <- do.call(rbind, output_results)


end_time <- Sys.time()
print(end_time - start_time)


csv_results <- as.data.frame(csv_results)
 
write.csv(csv_results, "validation_submission.csv")


