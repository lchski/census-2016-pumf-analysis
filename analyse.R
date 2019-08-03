library(tidyverse)
library(naniar)

census_2016 <- foreign::read.spss("data/pumf-98M0001-E-2016-individuals-spss/pumf-98M0001-E-2016-individuals_F1.sav") %>%
  as_tibble() %>%
  set_names(names(.) %>% toupper)

census_2016 %>%
  select(SEX, AGEGRP, BFNMEMB, PR, EMPIN) %>%
  replace_with_na(replace = list(EMPIN = c(99999999, 88888888))) %>%
  group_by(BFNMEMB, SEX) %>%
  summarize(count = n(), min = min(EMPIN, na.rm = TRUE), max = max(EMPIN, na.rm = TRUE), mean = mean(EMPIN, na.rm = TRUE), median = median(EMPIN, na.rm = TRUE)) %>%
  select(BFNMEMB, SEX, count, min:median)



replicate_weights <- paste0("WT",seq(1,16))

all_weights <- c("WEIGHT",replicate_weights)

gather_weights <- function(data) {
  data %>% gather(key="weight",value="Value",all_weights)
}

estimate_and_standard_error <- function(data,...){
  standard_deviation <- function(data,ws) {
    (as.matrix(data[ws]) %>% matrixStats::rowSds())/sqrt(length(ws))
  }
  
  data %>%
    group_by(weight,...) %>%
    summarize(Value=sum(Value),cases=n()) %>%
    ungroup %>%
    mutate(gggg=1) %>%
    left_join((.) %>%
                filter(weight!="WEIGHT") %>% 
                group_by(...,gggg) %>%
                select(...,gggg,Value) %>%
                summarize(standard_error=sd(Value)/sqrt(length(replicate_weights)))) %>%
    select(-gggg)
}

compute_shares <- function(data,...) {
  data %>%
    group_by(weight,...) %>%
    mutate(Total=sum(Value)) %>%
    mutate(Share=Value/Total) %>%
    ungroup %>%
    mutate(gggg=1) %>%
    left_join((.) %>%
                filter(weight!="WEIGHT") %>% 
                group_by(...,gggg) %>%
                select(...,gggg,Total) %>%
                summarize(total_error=sd(Total)/sqrt(length(replicate_weights)))) %>%
    select(-gggg) 
}

estimate_and_standard_error_share <- function(data,...){
  data %>%
    left_join((.) %>%
                filter(weight!="WEIGHT") %>% 
                group_by(...) %>%
                summarize(share_error=sd(Share)/sqrt(length(replicate_weights)))
    ) 
}

census_2016 %>% filter(
  TENUR=="Rented or Band housing",
  PRIHM=="Person is primary maintainer",
  !(DTYPE=="Apartment" & CONDO=="No, not part of a condominium development"),
  SUBSIDY == "No, not a subsidized dwelling",
)

census_2016 %>%
  select(Sex, AGEGRP, BFNMEMB, PR, EmpIn) %>%
  replace_with_na(replace = list(EmpIn = c(99999999, 88888888))) %>%
  left_join(sex_labels) %>%
  left_join(BFNMEMB_labels) %>%
  ggplot() +
    geom_histogram(mapping = aes(x = EmpIn, fill = Sex_label)) +
    xlim(c(0, 250000)) +
    facet_grid(rows = vars(BFNMEMB_label), scales = "free")


