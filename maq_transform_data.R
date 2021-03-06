process_maq <- function(rawdata, shinysesh = TRUE) {
    if (rawdata$auth == FALSE) {
        return (tibble())
    }
    
    if (shinysesh){
        html("statusupdate", "Processing MAQ data: children demographics")    
    }

    children = recombine(list("Respondents", "Children"), rawdata) %>%
        select(child_id, respondent_id, study_id)
    

    childrendemographics <-
        childdemographics_transform(rawdata, children)
    deviceuse <- deviceuse_transform(rawdata, children)
    mobile_deviceuse <- mobile_deviceuse_transform(rawdata, children)
    devices <- devices_transform(rawdata, children)
    devicelocations <- devicelocations_transform(rawdata)
    
    if (shinysesh){
        html("statusupdate", "Processing MAQ data: children language")    
    }

    children_age_sex = childrendemographics %>% 
        group_by(sex, age_months, child_id) %>% 
        select(c(sex, age_months, child_id)) %>% 
        ungroup()
    childlanguage <-
        childlanguage_transform(rawdata, children_age_sex)
    
    if (shinysesh){
        html("statusupdate", "Processing MAQ data: media use")    
    }

    mediacontent <- mediacontent_transform(rawdata, children)
    mediadeviceuse <- mediadeviceuse_transform(rawdata)
    
    mediauseconcerns <- mediaconcerns_transform(rawdata)
    
    if (shinysesh){
        html("statusupdate", "Processing MAQ data: parent media attitudes")    
    }
    parentsmediause <- parentsmediause_transform(rawdata, children)
    parentsmediaexposure <-
        parentsmediaexposure_transform(rawdata, children)
    parentmediaattitudes <-
        parentsmediaattitudes_transform(rawdata, children)
    parentsmediation <-
        parentsmediation_transform(rawdata, children)
    parentingmood <- parentingmood_transform(rawdata, children)

    if (shinysesh){
        html("statusupdate", "Processing MAQ data: parenting scales")    
    }

    pm <- pm_transform(rawdata, children)
    psi <- psi_transform(rawdata, children)
    qa <- qa_transform(rawdata, children)

    if (shinysesh){
        html("statusupdate", "Processing MAQ data: parent demographics")    
    }

    respondentdetails <-
        respondentdetails_transform(rawdata, children)
    income <-
        incomes_transform(rawdata, children)
    public_assistance <-
        public_assistance_transform(rawdata, children)
    education <- education_transform(rawdata, children)
    employment <- employment_transform(rawdata, children)
    sleep <- childsleep_transform(rawdata)
    
    videochat <- videochat_transform(rawdata, children)
    
    households <- household_transform(rawdata)
    childcare <- childcare_transform(rawdata)
    householdcommunication <- household_communication_transform(rawdata)
    

    children = recombine(list("Respondents", "Children"), rawdata) %>%
        select(child_id, respondent_id, study_id) %>%
                   group_by(child_id, study_id) %>%
                   summarise(count = n()) %>% select(-c(count))
    children$study_id[children$study_id == "BYU"] = "PM"
    
    if (shinysesh){
        html("statusupdate", "Processing MAQ data: putting the data together")    
    }

    maq <-  children %>% 
        left_join(childrendemographics, by = "child_id") %>%
        left_join(deviceuse, by = "child_id") %>% 
        left_join(devices, by = "child_id") %>% 
        left_join(mobile_deviceuse, by = "child_id") %>%
        left_join(devicelocations, by = "child_id") %>%
        left_join(childlanguage, by = "child_id") %>% 
        left_join(householdcommunication, by = "child_id") %>% 
        left_join(mediacontent, by = "child_id") %>%
        left_join(mediadeviceuse, by = "child_id") %>%
        left_join(mediauseconcerns, by = "child_id") %>%
        left_join(parentsmediause, by = "child_id") %>%
        left_join(parentsmediaexposure, by = "child_id") %>%
        left_join(parentmediaattitudes, by = "child_id") %>%
        left_join(parentingmood, by = "child_id") %>%
        left_join(parentsmediation, by = "child_id") %>% 
        left_join(pm, by = "child_id") %>%
        left_join(psi, by = "child_id")  %>%
        left_join(qa, by = "child_id") %>%
        left_join(respondentdetails, by = "child_id")  %>%
        left_join(income, by = "child_id")  %>%
        left_join(education, by = "child_id")  %>%
        left_join(public_assistance, by = "child_id")  %>%
        left_join(employment, by = "child_id")  %>% 
        left_join(sleep, by = "child_id") %>%
        left_join(households, by = "child_id") %>%
        left_join(childcare, by = "child_id") %>%
        left_join(videochat, by = "child_id")
    
    # factor vars to factor
    numericcols <- maq %>% select_if(is.numeric) %>% names()
    ndist <- lapply(maq, n_distinct)
    colndistinct <-
        as_tibble(cbind(nms = names(ndist), unlist(ndist)))
    names(colndistinct) <- c("nms", "cnt")
    colndistinct <- colndistinct %>% mutate(nums = as.numeric(cnt))
    factcols <-
        colndistinct %>% filter(nums >= 4) %>% filter(nums <= 15) %>% filter(!(nms %in% numericcols))
    boolcols <-
        colndistinct %>% filter(nums <= 3) %>% filter(!(nms %in% c(numericcols, "sex")))
    
    maq <- maq %>%
        ungroup() %>%
        mutate_at(factcols$nms, as.factor) %>%
        mutate_at(boolcols$nms, as.logical) %>%
        mutate(study = study_id)
    
    maq <- maq %>%
        mutate_at(c("mean_quality"), as.numeric) %>%
        drop_na(child_id)
    
    oldnames = names(maq)
    newnames = str_replace_all(oldnames, " |\\(|\\)|/", "_") %>% tolower()
    maq = maq %>% rename_at(vars(oldnames), ~newnames)
    
    if (shinysesh){
        html("statusupdate", "Finished loading the data...")    
    }
    
    return(maq)
}
