library(tidyverse)
library(naniar)

library(helpers)
source("lib/helpers.R")

census_2016 <- haven::read_sav("data/pumf-98M0001-E-2016-individuals-spss/pumf-98M0001-E-2016-individuals_F1.sav") %>%
  set_names(names(.) %>% toupper)

## for once I get into replicate weight stuff
#census_2016_wg <- census_2016 %>%
#  gather_weights()

census_2016_w <- census_2016 %>%
  select(-WT1:-WT16)
