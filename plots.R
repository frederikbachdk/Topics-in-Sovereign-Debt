library(readxl)
library(tidyverse)
library(devtools)
library(ggbiplot)
library(gridExtra)
library(cowplot)

######### DATA LOAD ###############################################################
path <- "/Users/frederikbach/OneDrive - Bach Teknik ApS/cand.polit/2. semester/Topics in Sov. Debt/R project/figurefile.xlsx"
spreads <- read_xlsx(path, sheet = "spread")
bidask <- read_xlsx(path, sheet = "relative_bidaskspread")
vix <- read_xlsx(path, sheet = "VIX")
reer <- read_xlsx(path, sheet = "REER")

path2 <- "/Users/frederikbach/OneDrive - Bach Teknik ApS/cand.polit/2. semester/Topics in Sov. Debt/R project/Country data/Austria_data.csv"
PCA <- read_csv(path2)
 
#### FIGURE 1 - COUNTRY SPREADS
spreads_long <- spreads %>% pivot_longer(Austria:Spain, 
                                         names_to = "country", values_to = "spread") %>%
  select(dates = Dates, country, spread)

# saving some space here
source("Figures/figure1.R")

grid.arrange(Austria, Belgium, Finland, France, Netherlands,
             Greece, Ireland, Italy, Portugal, Spain, 
             nrow = 2)

#### FIGURE 2 - VIX
vix %>%
  ggplot() + aes(x = Dates, VIX) + geom_line() +
  xlab("") + ylab("VIX") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=20))

#### FIGURE 2 - PC2
PCA %>% select(date, PC2_FS) %>%
  ggplot() + aes(x = date, y = PC2_FS) + geom_line() +
  xlab("") + ylab("PC2") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

#### FIGURE 4 - BID-ASK SPREADS
bidask_long <- bidask %>% pivot_longer(Austria:Spain, 
                                         names_to = "country", values_to = "spread") %>%
  select(dates = Dates, country, spread)

source("Figures/figure2.R")

grid.arrange(Austria, Belgium, Finland, France, Netherlands,
             Greece, Ireland, Italy, Portugal, Spain, 
             nrow = 2)


##### FIGURE 5 - REER
reer_long <- reer %>% pivot_longer(Austria:Spain, 
                                       names_to = "country", values_to = "reer") %>%
  select(dates = Dates, country, reer)

source("Figures/figure5.R")
grid.arrange(Austria, Belgium, Finland, France, Netherlands,
             Greece, Ireland, Italy, Portugal, Spain, 
             nrow = 2)

