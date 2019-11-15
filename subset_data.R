# out = subset_data(rawdata, hourbool=TRUE, hourrange = c(18,24), agebool = TRUE, agerange=c(0,100),
#             sitesbool = TRUE, sitesrange = c("PM", "UM"), progressbool = TRUE, progressrange = c(0,100),
#             qualitybool = TRUE, qualityrange = c(0, 100)
#             )

subset_data <-
    function(rawdata,
             hourbool=FALSE,
             hourrange=NULL,
             agebool=FALSE,
             agerange=NULL,
             sitesbool=FALSE,
             sitesrange=NULL,
             progressbool=FALSE,
             progressrange=NULL,
             qualitybool=FALSE,
             qualityrange=NULL) {
        output = list(tud = rawdata$tud$preprocessed,
                      maq = rawdata$maq$preprocessed,
                      alldata = rawdata$alldata_complete,
                      chronicle = rawdata$chronicle$preprocessed
                      )
        
        # subset hours
        if (hourbool) {
            print("hours")
            subset = rawdata$tud$summarised %>% 
                mutate(in_range = total_time >= hourrange[1] & total_time <= hourrange[2]) %>%
                select(child_id, day_id, in_range)
            subset_child = subset %>%
                group_by(child_id, in_range) %>%
                summarise(
                    child_in_range = sum(in_range) > 0
                )
            
            output$tud <- output$tud %>%
                left_join(subset, by = c("child_id", "day_id", "study")) %>%
                filter(in_range == TRUE) %>% select(-c(in_range))
            output$maq <- output$maq %>%
                left_join(subset_child, by = c("child_id")) %>%
                filter(is.na(child_in_range) | child_in_range == TRUE) %>%
                select(-c(in_range, child_in_range))
            output$alldata <- output$alldata %>%
                left_join(subset_child, by = c("child_id")) %>%
                filter(is.na(child_in_range) | child_in_range == TRUE) %>%
                select(-c(in_range, child_in_range))
            output$chronicle <- output$chronicle %>%
                left_join(subset_child, by = c("child_id")) %>%
                filter(is.na(child_in_range) | child_in_range == TRUE) %>%
                select(-c(in_range, child_in_range))
            print(paste("    dimensions after hour subsetting:", paste0(dim(output$tud), collapse=" x ")))
        }
        
        # subset sites
        if (sitesbool) {
            print("sites")
            sites = paste0(sitesrange, collapse = "|")
            output$tud <-
                output$tud %>% filter(str_detect(study, sites))
            output$maq <-
                output$maq %>% filter(str_detect(study, sites))
            output$alldata <- output$alldata %>%
                filter(str_detect(study, sites))
            output$chronicle <- output$chronicle %>%
                filter(str_detect(study, sites))
            print(paste("    dimensions after sites subsetting:", dim(output$tud)))
        }
        
        if (progressbool) {
            print("progress")
            subset = rawdata$tud$summarised %>% 
                mutate(in_range = progress >= progressrange[1] &
                           progress >= progressrange[2]) %>%
                select(child_id, day_id, in_range)
            subset_child = subset %>%
                group_by(child_id, in_range) %>%
                summarise(
                    child_in_range = sum(in_range) > 0
                )
            
            output$tud <- output$tud %>%
                left_join(subset, by = c("child_id", "day_id", "study")) %>%
                filter(in_range == TRUE) %>%
                select(-c(in_range))
            output$maq <- output$maq %>%
                left_join(subset_child, by = c("child_id")) %>%
                filter(is.na(child_in_range) | child_in_range == TRUE) %>%
                select(-c(in_range, child_in_range))
            output$alldata <- output$alldata %>%
                left_join(subset_child, by = c("child_id")) %>%
                filter(is.na(child_in_range) | child_in_range == TRUE) %>%
                select(-c(in_range, child_in_range))
            output$chronicle <- output$chronicle %>%
                left_join(subset_child, by = c("child_id")) %>%
                filter(is.na(child_in_range) | child_in_range == TRUE) %>%
                select(-c(in_range, child_in_range))
            print(paste("    dimensions after progress subsetting:", paste0(dim(output$tud), collapse=" x ")))
        }
        
        if (agebool) {
            print("age")
            subset <- rawdata$maq$preprocessed %>%
                filter(age_months > agerange[1] &
                           age_months < agerange[2]) %>% 
                filter(!is.na(child_id)) %>%
                select("child_id")
            output$tud <- output$tud %>%
                filter(child_id %in% subset$child_id)
            output$maq <- output$maq %>%
                filter(child_id %in% subset$child_id)
            output$alldata <- output$alldata %>%
                filter(child_id %in% subset$child_id)
            output$chronicle <- output$chronicle %>%
                filter(child_id %in% subset$child_id)
        }
        
        if (qualitybool) {
            print("quality")
            subset <- rawdata$maq$preprocessed %>%
                mutate(in_range = mean_quality >= qualityrange[1] |
                           mean_quality <= qualityrange[2]) %>% 
                 select(child_id, in_range)
            subset_child = subset %>%
                group_by(child_id, in_range) %>%
                summarise(
                    child_in_range = sum(in_range) > 0
                )
            
            output$tud <- output$tud %>%
                left_join(subset, by = c("child_id")) %>%
                filter(in_range == TRUE) %>%
                select(-c(in_range))
            output$maq <- output$maq %>%
                left_join(subset_child, by = c("child_id")) %>%
                filter(is.na(child_in_range) | child_in_range == TRUE) %>%
                select(-c(in_range, child_in_range))
            output$alldata <- output$alldata %>%
                left_join(subset_child, by = c("child_id")) %>%
                filter(is.na(child_in_range) | child_in_range == TRUE) %>%
                select(-c(in_range, child_in_range))
            output$chronicle <- output$chronicle %>%
                left_join(subset_child, by = c("child_id")) %>%
                filter(is.na(child_in_range) | child_in_range == TRUE) %>%
                select(-c(in_range, child_in_range))
            print(paste("    dimensions after quality subsetting:", paste0(dim(output$tud), collapse=" x ")))

        }
        
        output[['n_act']] = dim(output$tud)[1]
        output[['n_child']] = dim(output$alldata)[1]
        output[['n_nodes']] = length(names(rawdata$tud$nodes)) + length(names(rawdata$maq$edges))
        output$summary = summarise_data(output$tud, shinysesh = FALSE)

        return(output)
    }
