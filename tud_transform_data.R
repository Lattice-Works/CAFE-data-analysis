## combine everything
process_activities <- function(rawdata, shinysesh = TRUE) {
    if (rawdata$auth == FALSE) {
        return (tibble())
    }
    
    if (shinysesh){
        html("statusupdate", "Processing Time Use Diary")    
    }

    activity <- process_activity(rawdata)

    relatives <- process_relatives(rawdata)
    device_by_activity <- process_devices(rawdata)
    media_exposure_by_activity <- process_media_exposure(rawdata, activity)
    adults_by_activity <- process_adult_use(rawdata)
    locations_by_activity <- process_locations(rawdata)
    sites_by_activity <- process_sites(rawdata)
    metadata_by_activity <- process_metadata(rawdata)
    recruitment_by_activity <- process_recruitment(rawdata)
    
    
    new = activity %>% 
        left_join(media_exposure_by_activity, by ='primary_activity_id') %>%
        mutate(
            background_media_weighted_duration = duration*background_media_mean_percentage,
            background_media_tv_weighted_duration = duration*background_media_tv_mean_percentage,
            background_media_audio_weighted_duration = duration*background_media_audio_mean_percentage,
            background_media_other_weighted_duration = duration*background_media_other_mean_percentage
        ) %>%
        select(
            duration, background_media_tv_weighted_duration, background_media_tv_mean_percentage, background_media_tv
        )
    
    activity <- activity %>%
        left_join(relatives, by = "primary_activity_id") %>%
        left_join(device_by_activity, by = "primary_activity_id") %>%
        left_join(media_exposure_by_activity, by = "primary_activity_id") %>%
        left_join(adults_by_activity, by = "primary_activity_id") %>%
        left_join(locations_by_activity, by = "primary_activity_id") %>%
        left_join(sites_by_activity, by = "primary_activity_id") %>%
        left_join(metadata_by_activity, by = "primary_activity_id") %>%
        left_join(recruitment_by_activity, by = "primary_activity_id") %>%
        select(-c(child_id)) %>% rename(study = site, child_id = nc.SubjectIdentification)

    # factor vars to factor
    ndist <- activity %>%
        summarise_all(funs(n_distinct(.)))
    colndistinct <- as_tibble(cbind(nms = names(ndist), t(ndist)))
    names(colndistinct) <- c("nms", "cnt")
    colndistinct <- colndistinct %>% mutate(nums = as.numeric(cnt))
    factcols <-
        colndistinct %>% filter(nums >= 4) %>% filter(nums <= 6)
    boolcols <- colndistinct %>% filter(nums <= 3)
    
    activity <- activity %>%
        mutate_at(factcols$nms, as.factor) %>%
        mutate_at(boolcols$nms, as.logical)

    return(activity)
}

# summarise relatives related to primary activities

process_relatives <- function(rawdata) {
    if (dim(rawdata$tud$edges$relatives_primary_activity)[1] == 0) {
        return (tibble(primary_activity_id = as.character()))
    }
    adults_by_activity <-
        rawdata$tud$edges$relatives_primary_activity %>%
        # add relatives
        left_join(rawdata$tud$nodes$relatives, by = c(src = "openlattice.@id")) %>%
        # add activities
        left_join(
            rawdata$tud$nodes$primary_activity %>% select("openlattice.@id"),
            by = c(dst = "openlattice.@id")
        ) %>%
        # summarise
        rename(primary_activity_id = dst) %>%
        group_by(primary_activity_id) %>%
        summarise(
            adults_present = sum(
                str_count(
                    ol.name,
                    "Mother figure|Father figure|Grandparent|Childcare"
                )
            ) > 0,
            kids_present = sum(str_count(ol.name, "Sibling|kid"), na.rm =
                                   TRUE) > 0,
            alone = sum(str_detect(ol.name, "Alone"), na.rm = TRUE),
            not_alone = sum(!str_detect(ol.name, "Alone"), na.rm = TRUE)
        )
    return(adults_by_activity)
}

# summarise devices by media exposures by activity

process_devices <- function(rawdata) {
    if (dim(rawdata$tud$edges$devices_media_exposure)[1] == 0) {
        return (tibble(primary_activity_id = as.character()))
    }
    device_by_activity <- rawdata$tud$edges$devices_media_exposure %>%
        # add devices
        left_join(rawdata$tud$nodes$devices, by = c(src = "openlattice.@id")) %>%
        # add media_exposure
        left_join(rawdata$tud$nodes$media_exposure,
                  by = c(dst = "openlattice.@id")) %>%
        rename(media_exposure_id = dst) %>%
        # add activities
        left_join(rawdata$tud$edges$media_exposure_primary_activity,
                  by = c(media_exposure_id = "src")) %>%
        left_join(rawdata$tud$nodes$primary_activity,
                  by = c(dst = "openlattice.@id")) %>%
        rename(primary_activity_id = dst) %>%
        # summarise
        group_by(primary_activity_id) %>%
        summarise(
            primary_devices_number = n(),
            primary_device = paste(ol.name.x, collapse = " and "),
            primary_tv = sum(str_detect(ol.name.x, 'TV') > 0, na.rm = TRUE),
            primary_computer = sum(str_detect(ol.name.x, 'Computer') > 0, na.rm =
                                       TRUE),
            primary_smartphone = sum(str_detect(ol.name.x, 'Smartphone') > 0, na.rm =
                                         TRUE),
            primary_tablet = sum(str_detect(ol.name.x, 'Tablet') > 0, na.rm =
                                     TRUE),
            primary_book = sum(str_detect(ol.name.x, 'Book') > 0, na.rm = TRUE),
            primary_video = sum(str_detect(ol.name.x, 'Video') > 0, na.rm = TRUE),
            primary_console = sum(str_detect(ol.name.x, 'Console') > 0, na.rm =
                                      TRUE),
            primary_handheldgame = sum(str_detect(ol.name.x, 'HandheldGamingDevice') > 0, na.rm =
                                           TRUE),
            primary_radio = sum(str_detect(ol.name.x, 'RadioCD') > 0, na.rm =
                                    TRUE),
            primary_theater = sum(str_detect(ol.name.x, 'Theater') > 0, na.rm =
                                      TRUE),
            primary_other = sum(str_detect(ol.name.x, 'Other') > 0, na.rm = TRUE)
        )
    return(device_by_activity)
}

# summarise media exposures by activity

process_media_exposure <- function(rawdata, activity) {
    if (dim(rawdata$tud$edges$media_exposure_primary_activity)[1] == 0) {
        return (tibble(primary_activity_id = as.character()))
    }
    media_exposure_by_activity <-
        rawdata$tud$edges$media_exposure_primary_activity %>%
        # add media_exposure
        left_join(rawdata$tud$nodes$media_exposure,
                  by = c(src = "openlattice.@id")) %>%
        # add primary activities
        left_join(
            rawdata$tud$nodes$primary_activity %>% select("openlattice.@id"),
            by = c(dst = "openlattice.@id")
        ) %>%
        # summarise
        rename(primary_activity_id = dst) %>%
        mutate(percentage_background = as.numeric(ol.duration)) %>%
        left_join(activity, by = "primary_activity_id") %>%
        arrange(primary_activity_id) %>%
        mutate(
            weighted_background = ifelse(is.na(percentage_background), NA, percentage_background*duration/100)/60,
            duration = duration/60
        ) %>%
        group_by(primary_activity_id) %>%
        summarise(
            background_media = sum(str_detect(ol.priority, "secondary")) > 0,
            background_media_tv = sum(str_detect(ol.type, "television") &&
                str_detect(ol.priority, "secondary")) > 0 ,
            background_media_audio = sum(str_detect(ol.type, "audio")>0 &&
                str_detect(ol.priority, "secondary"))>0,
            background_media_other = sum(str_detect(ol.type, "other") &&
                str_detect(ol.priority, "secondary"))>0,
            
            background_media_mean_percentage = mean(percentage_background[background_media], na.rm=TRUE),
            background_media_tv_mean_percentage = mean(percentage_background[background_media_tv], na.rm=TRUE),
            background_media_audio_mean_percentage = mean(percentage_background[background_media_audio], na.rm=TRUE),
            background_media_other_mean_percentage = mean(percentage_background[background_media_other], na.rm=TRUE),
            
            background_media_sum_percentage = sum(percentage_background[background_media]),
            background_media_tv_sum_percentage = sum(percentage_background[background_media_tv]),
            background_media_audio_sum_percentage = sum(percentage_background[background_media_audio]),
            background_media_other_sum_percentage = sum(percentage_background[background_media_other]),

            background_media_mean_weighted_hours = mean(weighted_background[background_media], na.rm=TRUE),
            background_media_tv_mean_weighted_hours = mean(weighted_background[background_media_tv], na.rm=TRUE),
            background_media_audio_mean_weighted_hours = mean(weighted_background[background_media_audio], na.rm=TRUE),
            background_media_other_mean_weighted_hours = mean(weighted_background[background_media_other], na.rm=TRUE),
            
            background_media_full_hours = min(sum(duration[background_media]), mean(duration)),
            background_media_tv_full_hours = min(sum(duration[background_media_tv]), mean(duration)),
            background_media_audio_full_hours = min(sum(duration[background_media_audio]), mean(duration)),
            background_media_other_full_hours = min(sum(duration[background_media_other]), mean(duration)),
            
            primary_media_age = paste(ol.category[ol.priority == "primary" &
                                                      !is.na(ol.category)], collapse = " and "),
            primary_media_age_child = str_detect(ol.category, "Child's age") &&
                str_detect(ol.priority, "primary"),
            primary_media_age_younger = str_detect(ol.category, "Younger") &&
                str_detect(ol.priority, "primary"),
            primary_media_age_older = str_detect(ol.category, "Older") &&
                str_detect(ol.priority, "primary"),
            primary_media_age_adult = str_detect(ol.category, "Adults") &&
                str_detect(ol.priority, "primary"),
            
            secondary_media_age = paste(ol.category[ol.priority == "secondary" &
                                                        !is.na(ol.category)], collapse = " and "),
            secondary_media_age_child = str_detect(ol.category, "Child's age") &&
                str_detect(ol.priority, "secondary"),
            secondary_media_age_younger = str_detect(ol.category, "Younger") &&
                str_detect(ol.priority, "secondary"),
            secondary_media_age_older = str_detect(ol.category, "Older") &&
                str_detect(ol.priority, "secondary"),
            secondary_media_age_adult = str_detect(ol.category, "Adults") &&
                str_detect(ol.priority, "secondary"),
            screen = sum(str_detect(
                ol.type, "television|video|Video|internet|movie|games|Watched|computer"
            )) > 0,
            videochat = sum(str_detect(
                ol.type, "Video chat"
            )) > 0
        )
    return(media_exposure_by_activity)
}

# summarise adult coviewing by activity

process_adult_use <- function(rawdata) {
    if (dim(rawdata$tud$edges$primary_activity_adult_use)[1] == 0) {
        return (tibble(primary_activity_id = as.character()))
    }
    adult_use_by_activity <-
        rawdata$tud$edges$primary_activity_adult_use %>%
        # add media_exposure
        left_join(
            rawdata$tud$nodes$primary_activity %>% select("openlattice.@id"),
            by = c(src = "openlattice.@id")
        ) %>%
        # add primary activities
        left_join(rawdata$tud$nodes$adult_use, by = c(dst = "openlattice.@id")) %>%
        mutate(duration = as.numeric(ol.duration)) %>%
        # summarise
        rename(primary_activity_id = src) %>%
        group_by(primary_activity_id) %>%
        summarise(
            adult_use = str_detect(ol.status, "Yes"),
            adult_no_use = str_detect(ol.status, "No"),
            adult_social_media = str_detect(ol.reason, "Social media"),
            adult_social_call = str_detect(ol.reason, "Social call"),
            adult_work_media = str_detect(ol.reason, "work|Work"),
            adult_call = str_detect(ol.reason, "call"),
            adult_entertainment = str_detect(ol.reason, "Entertainment"),
            adult_duration = sum(duration)
        )
    return(adult_use_by_activity)
}

# summarise metadata
process_metadata <- function(rawdata) {
    if (dim(rawdata$tud$edges$primary_activity_survey_metadata)[1] == 0) {
        return (tibble(primary_activity_id = as.character()))
    }
    metadata_by_activity <-
        rawdata$tud$edges$primary_activity_survey_metadata %>%
        # add media_exposure
        left_join(
            rawdata$tud$nodes$primary_activity %>% select("openlattice.@id"),
            by = c(src = "openlattice.@id")
        ) %>%
        # add primary activities
        left_join(rawdata$tud$nodes$survey_metadata, by = c(dst = "openlattice.@id")) %>%
        # summarise
        rename(primary_activity_id = src) %>%
        group_by(primary_activity_id) %>%
        summarise(
            progress = mean(as.numeric(ol.status), na.rm=TRUE)
        )
        return(metadata_by_activity)
}

process_recruitment <- function(rawdata) {
    if (dim(rawdata$tud$edges$survey_recruitment_primary_activity)[1] == 0) {
        return (tibble(primary_activity_id = as.character()))
    }
    recruitment_by_activity <-
        rawdata$tud$edges$survey_recruitment_primary_activity %>%
        # add media_exposure
        left_join(
            rawdata$tud$nodes$primary_activity %>% select("openlattice.@id"),
            by = c(dst = "openlattice.@id")
        ) %>%
        # add primary activities
        left_join(rawdata$tud$nodes$survey_recruitment, by = c(src = "openlattice.@id")) %>%
        # summarise
        rename(primary_activity_id = dst) %>%
        group_by(primary_activity_id) %>%
        summarise(
            recruitment = ol.id
        )
    return(recruitment_by_activity)
}

# clean up activity rawdata (need to add child ID to find next sleep !)

process_activity <- function(rawdata) {
    if (dim(rawdata$tud$edges$people_primary_activity)[1] == 0) {
        return (tibble(primary_activity_id = as.character()))
    }
    
    ppl <- rawdata$tud$edges$people_primary_activity %>%
        left_join(rawdata$tud$nodes$people, by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$tud$nodes$primary_activity,
                  by = c(dst = "openlattice.@id"), all=TRUE) %>%
        rename(primary_activity_id = dst, child_id = src)
    vis <- rawdata$tud$edges$survey_visits_primary_activity %>%
        left_join(rawdata$tud$nodes$survey_visits, by = c(src = "openlattice.@id")) %>%
        left_join(
            rawdata$tud$nodes$primary_activity %>% select("openlattice.@id"),
            by = c(dst = "openlattice.@id")
        ) %>%
        rename(primary_activity_id = dst, visit_id = src)
    res <- rawdata$tud$edges$survey_respondents_primary_activity %>%
        left_join(rawdata$tud$nodes$survey_respondents,
                  by = c(src = "openlattice.@id")) %>%
        left_join(
            rawdata$tud$nodes$primary_activity %>% select("openlattice.@id"),
            by = c(dst = "openlattice.@id")
        ) %>%
        rename(primary_activity_id = dst,
               respondent_id = src)
    activity <- ppl %>%
        left_join(vis, by="primary_activity_id") %>%
        left_join(res, by = c("primary_activity_id","study"))
    
    activity <- activity %>%
        mutate(starttime = ymd_hms(ol.datetimestart),
               endtime = ymd_hms(ol.datetimeend)) %>%
        mutate(duration = as.numeric(endtime - starttime)/60) %>%
        mutate(duration = ifelse(duration < 0, duration + 24*60, duration)) %>%
        mutate(duration = ifelse(duration > 24*60, duration %% 24*60, duration)) %>%
        unite_("day_id", c('child_id', 'visit_id', 'respondent_id'), sep="-", remove=FALSE) %>%
        arrange(day_id, starttime) %>%
        mutate(table_access = (table_access.x & table_access.y))
    
    
    ## add time to sleep (OMG THIS WAS A DIFFICULT FUNCTION :-o )
    activity <- activity %>%
        group_by(day_id) %>%
        nest() %>%
        mutate(data = map(data, add_time_to_sleep)) %>%
        unnest() %>%
        select(
            "primary_activity_id",
            "nc.SubjectIdentification",
            "child_id",
            "respondent_id",
            "visit_id",
            "day_id",
            "time_to_sleep",
            "starttime",
            "endtime",
            "duration",
            "ol.activity",
            "table_access"
        )
    return(activity)
}

## sites

process_sites <- function(rawdata) {
    if (dim(rawdata$tud$edges$primary_activity_sites)[1] == 0) {
        return (tibble(primary_activity_id = as.character(),
                       site = as.character()))
    }
    adults_by_activity <-
        rawdata$tud$edges$primary_activity_sites %>%
        # add relatives
        left_join(
            rawdata$tud$nodes$primary_activity %>% select("openlattice.@id"),
            by = c(src = "openlattice.@id")
        ) %>%
        # add activities
        left_join(rawdata$tud$nodes$sites, by = c(dst = "openlattice.@id")) %>%
        # summarise
        rename(primary_activity_id = src, site_id = dst) %>%
        group_by(primary_activity_id) %>%
        summarise(site = names(which.max(table(general.id))))
    return(adults_by_activity)
}

## locations

process_locations <- function(rawdata) {
    if (dim(rawdata$tud$edges$primary_activity_locations)[1] == 0) {
        return (tibble(primary_activity_id = as.character(),
                       site = as.character()))
    }
    adults_by_activity <-
        rawdata$tud$edges$primary_activity_locations %>%
        # add relatives
        left_join(rawdata$tud$nodes$primary_activity,
                  by = c(src = "openlattice.@id")) %>%
        # add activities
        left_join(rawdata$tud$nodes$locations, by = c(dst = "openlattice.@id")) %>%
        # summarise
        rename(primary_activity_id = dst) %>%
        group_by(primary_activity_id) %>%
        summarise(location = names(which.max(table(location.name))))
    
    return(adults_by_activity)
}



## HELPER FUNCTIONS

add_time_to_sleep <- function(df) {
    sleeps <-
        df %>% filter(str_detect(ol.activity, "Sleep")) %>% pull(starttime)
    df['time_to_sleep'] <-
        df$endtime %>% lapply(get_closest, sleeps) %>% unlist()
    return(df)
}

get_closest <- function(startdt, slaapkes) {
    difs <- difftime(slaapkes, startdt, units = "hours")
    if (length(difs) == 1) {
        return (difs)
    }
    if (length(difs[difs >= 0]) == 0) {
        return (NA)
    }
    closest_sleep <- min(difs[difs >= 0], na.rm = TRUE)
    return(closest_sleep)
}
