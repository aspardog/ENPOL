event_study <- function(data.df = data_subset.df,
                        var_groups = "National",
                        var_analysis,
                        section,
                        subsection,
                        name) {
  
  
  variables2analyze <- c(var_groups)
  
  variables2summarise <- c(var_analysis)
  
  master_data.df <- data_subset.df %>%
    mutate(period = 
             case_when(
               years_since_NSJP < -1 & years_since_NSJP > -2 ~ "one_year_before",
               years_since_NSJP < -2 & years_since_NSJP > -3 ~ "two_years_before",
               years_since_NSJP < -3 & years_since_NSJP > -4 ~ "three_years_before",
               years_since_NSJP < -4 & years_since_NSJP > -5 ~ "four_years_before",
               years_since_NSJP < -5 & years_since_NSJP > -6 ~ "five_years_before",
               years_since_NSJP < -6 & years_since_NSJP > -7 ~ "six_years_before",
               years_since_NSJP < -7 & years_since_NSJP > -8 ~ "seven_years_before",
               years_since_NSJP < -8 & years_since_NSJP > -9 ~ "eight_years_before",
               years_since_NSJP < -9 & years_since_NSJP > -10 ~ "nine_years_before",
               years_since_NSJP < -10 & years_since_NSJP > -11 ~ "ten_years_before",
               years_since_NSJP > -1 & years_since_NSJP < 1  ~ "implementation_year",
               years_since_NSJP > 1 & years_since_NSJP < 2 ~ "one_year_after",
               years_since_NSJP > 2 & years_since_NSJP < 3 ~ "two_years_after",
               years_since_NSJP > 3 & years_since_NSJP < 4 ~ "three_years_after",
               years_since_NSJP > 4 & years_since_NSJP < 5 ~ "four_years_after",
               years_since_NSJP > 5 & years_since_NSJP < 6 ~ "five_years_after",
               years_since_NSJP > 6 & years_since_NSJP < 7 ~ "six_years_after",
               years_since_NSJP > 7 & years_since_NSJP < 8 ~ "seven_years_after",
               years_since_NSJP > 8 & years_since_NSJP < 9 ~ "eight_years_after",
               years_since_NSJP > 9 & years_since_NSJP < 10 ~ "nine_years_after",
               years_since_NSJP > 10 & years_since_NSJP < 11 ~ "ten_years_after",
               years_since_NSJP > 11 & years_since_NSJP < 12 ~ "eleven_years_after",
               years_since_NSJP > 12 & years_since_NSJP < 13 ~ "twelve_years_after"
             )) %>%
    filter(!is.na(period)) %>%
    arrange(years_since_NSJP) %>%
    mutate(National = "National") %>%
    group_by(Estado_arresto) %>%
    summarise(max_time_implementation = max(years_since_NSJP, na.rm = T))
  
  data2analysis <- lapply(variables2analyze, function(vars){
    
    data_subset.df <- master_data.df %>%
      mutate(var_name = as.character(vars)) %>%
      rename(group = all_of({{vars}}))
    
    changes_time <- data_subset.df %>% 
      group_by(period, group, var_name) %>%
      summarise(
        across(all_of(variables2summarise),
               ~ mean(.x, na.rm = TRUE))) %>%
      mutate(order_value = 
               case_when(
                 period == "ten_years_before"    ~ -10,
                 period == "nine_years_before"   ~ -9,
                 period == "eight_years_before"  ~ -8,
                 period == "seven_years_before"  ~ -7,
                 period == "six_years_before"    ~ -6,
                 period == "five_years_before"   ~ -5,
                 period == "four_years_before"   ~ -4,
                 period == "three_years_before"  ~ -3,
                 period == "two_years_before"    ~ -2,
                 period == "one_year_before"     ~ -1,
                 period == "implementation_year" ~ 0,
                 period == "one_year_after"      ~ 1,
                 period == "two_years_after"     ~ 2,
                 period == "three_years_after"   ~ 3,
                 period == "four_years_after"    ~ 4,
                 period == "five_years_after"    ~ 5,
                 period == "six_years_after"     ~ 6,
                 period == "seven_years_after"   ~ 7,
                 period == "eight_years_after"   ~ 8,
                 period == "nine_years_after"    ~ 9,
                 period == "ten_years_after"     ~ 10,
                 period == "eleven_years_after"  ~ 11,
                 period == "twelve_years_after"   ~ 12,
                 
               )
      ) %>%
      arrange(order_value)
    
  })
  
}
