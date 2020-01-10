count_group_wt <- function(dataset, ...) {
  dataset %>%
    group_by(...) %>%
    summarize(count = sum(WEIGHT)) %>%
    mutate(prop = count / sum(count)) %>%
    arrange(-prop)
}

