census_2016 %>%
  select(SEX, AGEGRP, BFNMEMB, PR, EMPIN) %>%
  replace_with_na(replace = list(EMPIN = c(99999999, 88888888))) %>%
  group_by(BFNMEMB, SEX) %>%
  summarize(count = n(), min = min(EMPIN, na.rm = TRUE), max = max(EMPIN, na.rm = TRUE), mean = mean(EMPIN, na.rm = TRUE), median = median(EMPIN, na.rm = TRUE)) %>%
  select(BFNMEMB, SEX, count, min:median)



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


