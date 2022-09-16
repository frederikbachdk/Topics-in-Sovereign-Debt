library(readxl)
library(tidyverse)
library(lubridate)

path <- "expbalance.xlsx"
data <- read_xlsx(path) 
colnames(data) <- c("country", "date", "E_govbal", "E_debtgdp")

data <- data %>% mutate(date = 
                  paste0(substr(date,6,7),"-",substr(date,1,4)))

E_govbal <- data %>% 
  pivot_wider(id_cols = "date", names_from = "country", values_from = "E_govbal") %>%
  mutate(date = 
           case_when(
             substr(date,1,2) == "01" ~ paste0(substr(date,4,7),"-01-31"),
             substr(date,1,2) == "05" ~ paste0(substr(date,4,7),"-05-31"),
             substr(date,1,2) == "09" ~ paste0(substr(date,4,7),"-09-30"),
             substr(date,1,2) == "07" ~ paste0(substr(date,4,7),"-07-31"))
           )

E_debtgdp <- data %>% 
  pivot_wider(id_cols = "date", names_from = "country", values_from = "E_debtgdp") %>%
  mutate(date = 
           case_when(
             substr(date,1,2) == "01" ~ paste0(substr(date,4,7),"-01-31"),
             substr(date,1,2) == "05" ~ paste0(substr(date,4,7),"-05-31"),
             substr(date,1,2) == "09" ~ paste0(substr(date,4,7),"-09-30"),
             substr(date,1,2) == "07" ~ paste0(substr(date,4,7),"-07-31"))
  )

write.csv(E_govbal, "Estimation/govbal.csv")
write.csv(E_debtgdp, "Estimation/debtgdp.csv")



