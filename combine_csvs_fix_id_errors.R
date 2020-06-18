library(readr)
library(dplyr)
library(stringr)

evaluation <- read_csv("evaluation_submission.csv")
validation <- read_csv("validation_submission.csv")

evaluation_okay <- evaluation[1:28827,]
evaluation_items_state <- evaluation[28828:111150,] %>% 
  mutate(id = case_when(str_detect(id, "FOODS") ~ paste0(substr(id, (5 + 8), ( 5 + 10)), 
                                                         substr(id, (1), (5 + 7)),
                                                         substr(id, (5 + 11), (stringr::str_length(id)))),
                        str_detect(id, "HOBBIES") ~ paste0(substr(id, (7 + 8), ( 7 + 10)), 
                                                         substr(id, (1), (7 + 7)),
                                                         substr(id, (7 + 11), (stringr::str_length(id)))),
                        str_detect(id, "HOUSEHOLD") ~ paste0(substr(id, (9 + 8), ( 9 + 10)), 
                                                         substr(id, (1), (9 + 7)),
                                                         substr(id, (9 + 11), (stringr::str_length(id))))))

evaluation_items_store <- evaluation[111151:length(evaluation$id),] #%>%
  # mutate(id = case_when(str_detect(id, "FOODS") ~ paste0(substr(id, (5 + 8), ( 5 + 12)), 
  #                                                        substr(id, (1), (5 + 7)),
  #                                                        substr(id, (5 + 13), (stringr::str_length(id)))),
  #                       str_detect(id, "HOBBIES") ~ paste0(substr(id, (7 + 8), ( 7 + 12)), 
  #                                                          substr(id, (1), (7 + 7)),
  #                                                          substr(id, (7 + 13), (stringr::str_length(id)))),
  #                       str_detect(id, "HOUSEHOLD") ~ paste0(substr(id, (9 + 8), ( 9 + 12)), 
  #                                                            substr(id, (1), (9 + 7)),
  #                                                            substr(id, (9 + 13), (stringr::str_length(id))))))

validation_okay <- validation[1:28827,]
validation_items_state <- validation[28828:111150,] %>% 
  mutate(id = case_when(str_detect(id, "FOODS") ~ paste0(substr(id, (5 + 8), ( 5 + 10)), 
                                                         substr(id, (1), (5 + 7)),
                                                         substr(id, (5 + 11), (stringr::str_length(id)))),
                        str_detect(id, "HOBBIES") ~ paste0(substr(id, (7 + 8), ( 7 + 10)), 
                                                           substr(id, (1), (7 + 7)),
                                                           substr(id, (7 + 11), (stringr::str_length(id)))),
                        str_detect(id, "HOUSEHOLD") ~ paste0(substr(id, (9 + 8), ( 9 + 10)), 
                                                             substr(id, (1), (9 + 7)),
                                                             substr(id, (9 + 11), (stringr::str_length(id))))))

validation_items_store <- validation[111151:length(validation$id),] #%>%
  # mutate(id = case_when(str_detect(id, "FOODS") ~ paste0(substr(id, (5 + 8), ( 5 + 12)), 
  #                                                        substr(id, (1), (5 + 7)),
  #                                                        substr(id, (5 + 13), (stringr::str_length(id)))),
  #                       str_detect(id, "HOBBIES") ~ paste0(substr(id, (7 + 8), ( 7 + 12)), 
  #                                                          substr(id, (1), (7 + 7)),
  #                                                          substr(id, (7 + 13), (stringr::str_length(id)))),
  #                       str_detect(id, "HOUSEHOLD") ~ paste0(substr(id, (9 + 8), ( 9 + 12)), 
  #                                                            substr(id, (1), (9 + 7)),
  #                                                            substr(id, (9 + 13), (stringr::str_length(id))))))


full_data <- bind_rows(evaluation_okay, evaluation_items_state, evaluation_items_store, 
                       validation_okay, validation_items_state, validation_items_store)

test <- left_join(sample_submission, full_data, by = "id")

write_csv(full_data, "full_submission.csv")
