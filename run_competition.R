library(readr)
library(dplyr)
source("Competition_Functions.R")

competition_run("validation")
competition_run("evaluation")

evaluation <- read_csv("evaluation_submission.csv")
validation <- read_csv("validation_submission.csv")

full_data <- bind_rows(evaluation, validation)

colnames(full_data)[1] <- "id"

write_csv(full_data, "full_submission.csv")