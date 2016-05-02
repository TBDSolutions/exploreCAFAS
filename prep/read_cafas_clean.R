
# Export from FASoutcomes, path:
  # Data Export > CAFAS > Export Type = XML

# Insert path to XML file here in quotes.  Must use forward slashes (i.e. / not \).
# For example:
  file <- "C:/Path/to/CAFAS/CAFAS_ExportData_etc.xml"

### The remainder of this code can be run as is

# Load packages
  library(xml2)
  library(magrittr)
  library(dplyr)

# Parse XML
  cafas <- read_xml(file)

# Subset Client Data
  clients <- cafas %>% xml2::xml_find_all(xpath = "//response/organization/clients")

cafas_demog <-
  dplyr::data_frame(
    org = clients %>% xml_find_all(xpath = "//basicInfo/orgID") %>% xml_text(),
    id = clients %>% xml_find_all(xpath = "//basicInfo/id1") %>% xml_text(),
    dob = clients %>% xml_find_all("//basicInfo/dob") %>% xml_text(),
    gender = clients %>% xml_find_all("//basicInfo/gender") %>% xml_text(),
    zip = clients %>% xml_find_all("//client/basicInfo/zip") %>% xml_text(),
    start_dt = clients %>% xml_find_all("//client/basicInfo/creationDT") %>% xml_text()
  )

cafas_assess <-
  dplyr::data_frame(
    id = clients %>% xml_find_all(xpath = "//regularAssessment/assessment/assessDetails/id1") %>% xml_text(),
    episode_num = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Episode_Number") %>% xml_text(),
    episode_days = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Days_since_start_of_episode") %>% xml_text(),
    assess_num = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/adminRefNum") %>% xml_text(),
    assess_date = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Assessment_Date") %>% xml_text(),
    assess_status = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Assessment_Status") %>% xml_text(),
    assess_delete = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Assessment_is_Deleted") %>% xml_text(),
    assess_type = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Assessment_Administration_Type") %>% xml_text(),
    assess_period = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Time_Period_Rated") %>% xml_text(),
    most_recent = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/No_Subsequent_Assessment") %>% xml_text(),
    liv_arrange = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Youth_Current_Living_Arrangement") %>% xml_text(),
    cafas_total = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/CAFAS_Total_Score") %>% xml_text(),
    tier = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/CAFAS_Tier") %>% xml_text(),
    child_mgmt = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/CAFAS_Improving_Child_Management_Skills") %>% xml_text(),
    behavioral = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/CAFAS_Pervasive_Behavioral_Impairment") %>% xml_text(),
    psychosis_risk = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Potentially_psychotic_or_Organic_symptoms_in_the_context_of_") %>% xml_text(),
    severe_sud = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Substance_use_rated_at_severe_impairment_level") %>% xml_text(),
    suicide_risk = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Has_made_a_serious_suicide_attempt_or_is_considered_to_be_po") %>% xml_text(),
    suicide_ideation = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Possibly_suicidal_as_suggested_by_ideations_verbalizations_o") %>% xml_text(),
    aggressive = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Aggressive_or_threatening_behavior_in_School_at_Home_or_in_t") %>% xml_text(),
    sexual = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Sexual_Behavior") %>% xml_text(),
    fire_setting = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Fire_setting_or_playing_with_fire") %>% xml_text(),
    runaway = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Runaway_behavior") %>% xml_text(),
    exceed_primary = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Youths_needs_far_exceed_primary_caregivers_resources") %>% xml_text(),
    exceed_noncustody = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Youths_needs_far_exceed_noncustodial_caregivers_resources") %>% xml_text(),
    exceed_surrogate = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Youths_needs_far_exceed_surrogate_caregivers_resources") %>% xml_text(),
    total_impair = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Total_Severe_Impairments") %>% xml_text(),
    total_diff = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Difference_in_Total_Score") %>% xml_text(),
    improvement = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Meaningful_and_Reliable_Improvement") %>% xml_text(),
    change_impair = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Change_in_Severe_Impairments") %>% xml_text(),
    change_behavior = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Change_in_Pervasive_Behavioral_Impairment") %>% xml_text(),
    improve_one_more = clients %>% xml_find_all("//regularAssessment/assessment/assessDetails/Improvement_in_one_or_more_indicators") %>% xml_text(),
    subscale_school = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/School_Subscale_Score") %>% xml_text(),
    subscale_home = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/Home_Subscale_Score") %>% xml_text(),
    subscale_community = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/Community_Subscale_Score") %>% xml_text(),
    subscale_behavior = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/Behavior_Toward_Others_Subscale_Score") %>% xml_text(),
    subscale_mood = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/Mood_Subscale_Score") %>% xml_text(),
    subscale_selfharm = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/SelfHarmful_Subscale_Score") %>% xml_text(),
    subscale_substance = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/Substance_Use_Subscale_Score") %>% xml_text(),
    subscale_thinking = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/Thinking_Subscale_Score") %>% xml_text(),    
    material_primary = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/Primary_Caregiver_Resources_Material_Needs") %>% xml_text(),    
    social_primary = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/Primary_Caregiver_Resources_FamilySocial_Support") %>% xml_text(),    
    material_noncustody = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/NonCustodial_Caregiver_Resources_Material_Needs") %>% xml_text(),    
    social_noncustody = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/NonCustodial_Caregiver_Resources_FamilySocial_Support") %>% xml_text(),    
    material_surrogate = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/Surrogate_Caregiver_Resources_Material_Needs") %>% xml_text(),    
    social_surrogate = clients %>% xml_find_all("//regularAssessment/assessment/subscaleInfo/Surrogate_Caregiver_Resources_FamilySocial_Support") %>% xml_text()    
  )

# Join CAFAS Demographic and Assessment tables
  sub_cafas <- cafas_assess %>% left_join(cafas_demog, by = "id")

# Assign classes to vectors, currently all "chr" from .xml parsing
  sub_cafas$id <- as.factor(sub_cafas$id)
  sub_cafas$episode_num <- as.factor(sub_cafas$episode_num)
  sub_cafas$episode_days <- as.integer(sub_cafas$episode_days)
  sub_cafas$assess_num <- as.factor(sub_cafas$assess_num)
  sub_cafas$assess_date <- lubridate::mdy(sub_cafas$assess_date)
  sub_cafas$assess_status <- as.factor(sub_cafas$assess_status)
  sub_cafas$assess_delete <- sub_cafas$assess_delete == "True"
  sub_cafas$assess_type <- as.factor(sub_cafas$assess_type)
  sub_cafas$assess_period <- as.factor(sub_cafas$assess_period)
  sub_cafas$most_recent <- sub_cafas$most_recent == "True"
  sub_cafas$cafas_total <- as.integer(sub_cafas$cafas_total)
  sub_cafas$tier <- as.factor(sub_cafas$tier)
  sub_cafas$child_mgmt <- sub_cafas$child_mgmt == "True"
  sub_cafas$behavioral <- sub_cafas$behavioral == "True"
  sub_cafas$psychosis_risk <- sub_cafas$psychosis_risk == "True"
  sub_cafas$severe_sud <- sub_cafas$severe_sud == "True"
  sub_cafas$suicide_risk <- sub_cafas$suicide_risk == "True"
  sub_cafas$suicide_ideation <- sub_cafas$suicide_ideation == "True"
  sub_cafas$aggressive <- sub_cafas$aggressive == "True"
  sub_cafas$sexual <- sub_cafas$sexual == "True"
  sub_cafas$fire_setting <- sub_cafas$fire_setting == "True"
  sub_cafas$runaway <- sub_cafas$runaway == "True"
  sub_cafas$exceed_primary <- sub_cafas$exceed_primary == "True"
  sub_cafas$exceed_noncustody <- sub_cafas$exceed_noncustody == "True"
  sub_cafas$exceed_surrogate <- sub_cafas$exceed_surrogate == "True"
  sub_cafas$total_impair <- as.integer(sub_cafas$total_impair)
  sub_cafas$total_diff <- as.integer(sub_cafas$total_diff)
  sub_cafas$improvement <- as.integer(sub_cafas$improvement)
  sub_cafas$change_impair <- as.integer(sub_cafas$change_impair)
  sub_cafas$change_behavior <- as.integer(sub_cafas$change_behavior)
  sub_cafas$improve_one_more <- as.integer(sub_cafas$improve_one_more)
  sub_cafas$subscale_school <- as.integer(sub_cafas$subscale_school)
  sub_cafas$subscale_home <- as.integer(sub_cafas$subscale_home)
  sub_cafas$subscale_community <- as.integer(sub_cafas$subscale_community)
  sub_cafas$subscale_behavior <- as.integer(sub_cafas$subscale_behavior)
  sub_cafas$subscale_mood <- as.integer(sub_cafas$subscale_mood)
  sub_cafas$subscale_selfharm <- as.integer(sub_cafas$subscale_selfharm)
  sub_cafas$subscale_substance <- as.integer(sub_cafas$subscale_substance)
  sub_cafas$subscale_thinking <- as.integer(sub_cafas$subscale_thinking)
  sub_cafas$material_primary <- as.integer(sub_cafas$material_primary)
  sub_cafas$material_noncustody <- as.integer(sub_cafas$material_noncustody)
  sub_cafas$material_surrogate <- as.integer(sub_cafas$material_surrogate)
  sub_cafas$social_primary <- as.integer(sub_cafas$social_primary)
  sub_cafas$social_noncustody <- as.integer(sub_cafas$social_noncustody)
  sub_cafas$social_surrogate <- as.integer(sub_cafas$social_surrogate)
  sub_cafas$org <- as.factor(sub_cafas$org)
  sub_cafas$dob <- lubridate::mdy(sub_cafas$dob)
  sub_cafas$gender <- as.factor(sub_cafas$gender)
  sub_cafas$zip <- as.factor(sub_cafas$zip)
  sub_cafas$start_dt <- lubridate::mdy(sub_cafas$start_dt)

# Write to a csv file in your working drive
# CHANGE THIS TO AVOID OVERWRITING PREVIOUS VERSIONS
  write.csv(sub_cafas, file = "C://path/to/file.csv")
  