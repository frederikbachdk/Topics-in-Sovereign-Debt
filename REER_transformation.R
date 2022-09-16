library(readxl)
library(tidyverse)
library(lubridate)

path <- "REER_RAW.xlsx"
data <- read_xlsx(path) 
colnames(data) <- c("country", "country_code", "date", "reer")

data <- data %>% mutate(date = 
                  paste0(substr(date,6,7),"-",substr(date,1,4)))

data_tib <- data %>% 
  pivot_wider(id_cols = "date", names_from = "country", values_from = "reer") %>%
  mutate(date = 
           case_when(
             substr(date,1,2) == "1-" ~ paste0("31-01-",substr(date,3,6)),
             substr(date,1,2) == "2-" ~ paste0("28-02-",substr(date,3,6)),
             substr(date,1,2) == "3-" ~ paste0("31-03-",substr(date,3,6)),
             substr(date,1,2) == "4-" ~ paste0("30-04-",substr(date,3,6)),
             substr(date,1,2) == "5-" ~ paste0("31-05-",substr(date,3,6)),
             substr(date,1,2) == "6-" ~ paste0("30-06-",substr(date,3,6)),
             substr(date,1,2) == "7-" ~ paste0("31-07-",substr(date,3,6)),
             substr(date,1,2) == "8-" ~ paste0("31-08-",substr(date,3,6)),
             substr(date,1,2) == "9-" ~ paste0("30-09-",substr(date,3,6)),
             substr(date,1,2) == "10" ~ paste0("31-10-",substr(date,4,7)),
             substr(date,1,2) == "11" ~ paste0("30-11-",substr(date,4,7)),
             substr(date,1,2) == "12" ~ paste0("31-12-",substr(date,4,7))
           ))

write.csv(data_tib, "reer.csv")

