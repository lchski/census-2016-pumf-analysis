count_group_wt <- function(dataset, ...) {
  dataset %>%
    group_by(...) %>%
    summarize(count_unweighted = n(), count = sum(WEIGHT)) %>%
    mutate(prop = count / sum(count)) %>%
    arrange(-prop)
}

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



## from: https://github.com/mountainMath/doodles/blob/e2f7dd0f22ad9e02857a0df8198260d285e50b58/content/posts/2019-02-27-tax-speculations.Rmarkdown
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

