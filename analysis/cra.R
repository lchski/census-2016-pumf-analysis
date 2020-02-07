census_2016 %>%
  filter(PR == 35) %>% ## ON
  filter(CFSTAT == 6) %>% ## Lives alone
  replace_labeled_values_with_na() %>%
  filter(TOTINC <= 10000) %>% ## Total income less than 10,000
  ggplot(aes(y = TOTINC, weight = WEIGHT)) +
  geom_boxplot()

census_2016 %>%
  filter(PR == 35) %>% ## Ontario
  filter(CFSTAT == 6) %>% ## Lives alone
  replace_labeled_values_with_na() %>%
  filter(TOTINC <= 10000) %>% ## Total income less than 10,000
  filter(TOTINC >= 0) %>% ## Total income more than 0 [WARNING: This may remove valid/relevant cases]
  ggplot(aes(x = TOTINC, weight = WEIGHT)) +
  geom_histogram()

census_2016 %>%
  filter(PR == 35) %>% ## Ontario
  filter(CFSTAT == 6) %>% ## Lives alone
  replace_labeled_values_with_na() %>%
  filter(TOTINC <= 10000) %>% ## Total income less than 10,000
  filter(TOTINC >= 0) %>%
  count_group_wt()






census_2016 %>%
  filter(PR == 35) %>% ## Ontario
  filter(CFSTAT == 6) %>% ## Lives alone
  replace_labeled_values_with_na() %>%
  filter(TOTINC <= 10000) %>% ## Total income less than 10,000
  filter(TOTINC >= 0) %>% ## Total income more than 0 [WARNING: This may remove valid/relevant cases]
  ggplot(aes(x = TOTINC, weight = WEIGHT)) +
  geom_histogram()

census_2016 %>%
  filter(PR == 35) %>% ## Ontario
  filter(CFSTAT %in% c(6, 7)) %>% ## Single, no dependents
  filter(AGEGRP > 6) %>% ## over 15 to 17
  filter(TOTINC <= 12000) %>%
  count_group_wt()
  
census_2016 %>%
  filter(AGEGRP > 6) %>% ## over 17
  filter(HHINC <= 35000) %>%
  count_group_wt()


census_2016 %>%
  filter(PR == 35) %>% ## Ontario
  filter(CFSTAT %in% c(6, 7)) %>% ## Single, no dependents
  filter(AGEGRP > 6) %>% ## over 15 to 17
  filter(TOTINC <= 12000) %>%
  mutate(taxdiff = TOTINC - TOTINC_AT) %>%
  count_group_wt(taxdiff)

