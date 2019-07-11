postprocessing <- function(data) {
    parent_use_key <-
        c(
            'Never' = 0,
            "Less than 30 minutes" = 0.25,
            "30-60 minutes" = 0.75,
            "1-2 hours" = 1.5,
            "2-3 hours" = 2.5,
            "3-4 hours" = 3.5,
            '4-5 hours' = 4.5,
            'More than 5 hours' = 6,
            "not used" = 0,
            .default = NA
        )
    
    data = data %>% mutate(parent_weekday_ipad_use_numeric = recode(parent_weekday_ipad_use, !!!parent_use_key))
    
    return(data)
    
}



