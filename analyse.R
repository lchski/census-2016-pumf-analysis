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
) %>%
  mutate_at(.vars = all_weights,
            .funs = list(~(.*SHELCO*12)))

census_2016 %>%
  select(SEX, AGEGRP, BFNMEMB, PR, EMPIN) %>%
  replace_with_na(replace = list(EMPIN = c(99999999, 88888888))) %>%
  ggplot() +
    geom_histogram(mapping = aes(x = EMPIN, fill = SEX)) +
    xlim(c(0, 250000)) +
    facet_wrap(facets = vars(BFNMEMB), scales = "free", strip.position = "right")

replace_labeled_values_with_na <- function(data) {
  data %>%
    replace_with_na(replace = list(
      EMPIN = c(99999999, 88888888),
      EICBN = c(99999999, 88888888),
      GOVTI = c(99999999, 88888888),
      GTRFS = c(99999999, 88888888),
      INCTAX = c(99999999, 88888888),
      INVST = c(99999999, 88888888),
      CQPPB = c(99999999, 88888888),
      MRKINC = c(99999999, 88888888),
      OASGI = c(99999999, 88888888),
      OTINC = c(99999999, 88888888),
      RETIR = c(99999999, 88888888),
      SEMPI = c(99999999, 88888888),
      TOTINC = c(99999999, 88888888),
      TOTINC_AT = c(99999999, 88888888),
      WAGES = c(99999999, 88888888)
    )) %>%
    replace_with_na(replace = list(
      KOL = c("Not available")
    ))
}

## currently this is very slow: https://github.com/njtierney/naniar/issues/143
replace_labeled_values_with_na_2 <- function(data) {
  data %>%
    replace_with_na_at(
      .vars = c("EMPIN", "EICBN", "GOVTI", "GTRFS", "INCTAX", "INVST"),
      condition = ~ .x %in% c(99999999, 88888888)
    ) %>%
    replace_with_na(replace = list(
      KOL = c("Not available")
    ))
}

histogram_groups <- function(data, ...) {
  data %>%
    select(SEX, EMPIN, ...) %>%
    replace_labeled_values_with_na %>%
    ggplot() +
    geom_histogram(mapping = aes(x = EMPIN, fill = SEX)) +
    xlim(c(0, 150000)) +
    facet_wrap(facets = vars(...), scales = "free", strip.position = "right")
}


## replicating http://sda.chass.utoronto.ca/sdaweb/dli2/cc16/cc16i/more_doc/UserGuide.pdf pg. 129, ex. 2
## see also: https://github.com/mountainMath/doodles/blob/e2f7dd0f22ad9e02857a0df8198260d285e50b58/content/posts/2019-02-27-tax-speculations.Rmarkdown#L139-L166
census_2016_w %>%
  filter(as_factor(CMA) == "Montréal") %>% ## NB! can also do `filter(CMA == 462)`
  mutate(is_immigrant = as_factor(IMMSTAT) == "Immigrants") %>% ## here, too, `IMMSTAT == 2`
  group_by(is_immigrant) %>%
  summarize(count = sum(WEIGHT)) %>%
  mutate(prop = count / sum(count))

## ex. 3
census_2016_w %>%
  filter(CMA == 933) %>%
  filter(SEX == 2) %>%
  filter(8 <= AGEGRP & AGEGRP <= 12) %>%
  group_by(MARSTH) %>%
  summarize(count = sum(WEIGHT)) %>%
  mutate(prop = count / sum(count))


summary(lm(
  OASGI ~ AGEGRP,
  data = census_2016_w %>% replace_labeled_values_with_na() %>% mutate(AGEGRP = as_factor(AGEGRP)),
  weights = WEIGHT
))

