postprocessing <- function(data) {
    hours_key = c(
        "Never" = 0,
        "Less than 30 minutes" = 1,
        "30-60 minutes" = 2,
        "1-2 hours" = 3,
        "2-3 hours" = 4,
        "3-4 hours" = 5,
        "4-5 hours" = 6,
        "2-5 hours" = NA,
        "More than 5 hours" = 7
    )
    
    data = data %>% mutate(
        parent_weekday_ipad_use_numeric = recode(parent_weekday_ipad_use, !!!hours_key),
        parent_weekday_computer_use_numeric = recode(parent_weekday_computer_use, !!!hours_key)
        )
    
    return(data)
    
}



