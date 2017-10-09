## explore CAFAS server.R ##

shinyServer(
  function(input, output) {
    
    # Make Reactive Datasets
    
    cafasInput <- reactive({  
      
      # Select variables for use with selected episode methodology
      
      if (input$select_episode == "Common Episodes") {
        # Replace default col names with values from revised episode groups
        scrub_fas %<>%
          select(-fake_episode_id,-episode_num,
                 -episode_start,-episode_end,
                 -assess_ord,-episode_elapsed,-episode_length) %>%
          rename(fake_episode_id = rev_fake_episode_id,
                 episode_num = rev_episode_num,
                 episode_start = rev_episode_start,
                 episode_end = rev_episode_end,
                 assess_ord = rev_assess_ord,
                 episode_elapsed = rev_episode_elapsed,
                 episode_length = rev_episode_length)
      } else if (input$select_episode == "System Default") { 
        # Remove variables produced by revised episode groups
        scrub_fas %<>%
          select(-rev_fake_episode_id,-rev_episode_num,
                 -rev_episode_start,-rev_episode_end,
                 -rev_assess_ord,-rev_episode_elapsed,-rev_episode_length)
      } else input$select_episode
      
      # Define lists based on inputs for use in %in% statements belows
      
      tool_version <-
        if (input$select_version == "Both versions") {c("CAFAS","PECFAS")
        } else if (input$select_version == "CAFAS") { c("CAFAS")
        } else if (input$select_version == "PECFAS") { c("PECFAS")
        } else print(paste0("Error.  Unrecognized input."))
      
      tx_status <- 
        if (input$radio_status == "Either") {c("Active","Inactive","Inferred Inactive")
        } else if (input$radio_status == "Inactive") { c("Inactive","Inferred Inactive")
        } else input$radio_status
      
      filt_type <-
        if (input$radio_type == "Initial") {c("Initial CAFAS","Initial PECFAS","Revised Initial")
        } else if (input$radio_type == "All") {unique(scrub_fas$assess_type)
        } else input$radio_type
      
      prog_type <- 
        if (input$select_prog == "All") {
          # Include all programs if "All" is selected
          levels(unique(scrub_fas$program_type))
        } else input$select_prog
      
      # Apply selected filters
      # Note that the 'Agency:' filter is not included here, since a number of
      # the agency views still require use of the entire dataset for purposes 
      # of model building and other comparisons
      
      scrub_fas %>%
        filter(as.Date(max_date) >= input$dateRange[1]
               & as.Date(max_date) <= input$dateRange[2]
               & client_status %in% tx_status
               & assess_type %in% filt_type
               & version %in% tool_version
               & program_type %in% prog_type)
      
    })
    
    hist_df <- reactive({
      
      if (input$agency == "All") {
        fas_filt <- cafasInput()
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        fas_filt <- cafasInput() %>% filter(cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Filter to only include most recent assessments based on selection
      if (input$most_recent == T) {
        fas_filt <- 
          fas_filt %>%
          ungroup() %>% 
          group_by(fake_episode_id) %>%
          filter(assess_ord == max(assess_ord))
      } else fas_filt <- fas_filt
      
      # Filter discharge assessments based on selection
      if (input$remove_dc == T) {
        fas_filt <- 
          fas_filt %>%
          # For episodes with an end date, incl assessments not done on end date
          # For episodes without an end date, incl assessments not marked as 'Exit'
          filter(is.na(episode_end) == F & assess_date != episode_end
                 | is.na(episode_end) == T & grepl("^Exit ",assess_type) == F) %>% 
          ungroup() %>% droplevels()
      } else fas_filt <- fas_filt
      
      fas_filt %<>%
        filter(assess_ord >= input$assess_num[1]
               & assess_ord <= input$assess_num[2]) %>%
        filter(days_between >= input$days_between[1]
               & days_between <= input$days_between[2]) %>%
        # No service data prior to 2014-10-01
        filter(assess_date >= "2014-10-01")
      
      fas_filt
      
    })
    
    outcome_df <- reactive({
      
      if (input$agency == "All") {
        fas_filt <- cafasInput()
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        fas_filt <- cafasInput() %>% filter(cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Create dataframe for calculating measures
      fas_filt %>%
        group_by(fake_episode_id,fake_id,cmh,episode_num) %>%
        mutate(init_CAFAS = assess_ord == min(assess_ord), # for included assessments
               last_CAFAS = assess_ord == max(assess_ord)) %>%
        filter(init_CAFAS == T | last_CAFAS == T) %>%
        mutate(interval = ifelse(init_CAFAS == T, "Initial", "Most Recent"),
               score_total_diff = score_total - lag(score_total), #doesn't work if start of episode not in dataset
               meaning_improve = total_diff >= 20, # total_diff is calc by episode
               # Tag youth with improved Risk Factors between init and most recent
               child_mgmt_improve = child_mgmt == F & lag(child_mgmt) == T,
               behavioral_improve = behavioral == F & lag(behavioral) == T,
               psychosis_risk_improve = psychosis_risk == F & lag(psychosis_risk) == T,
               severe_sud_improve = severe_sud == F & lag(severe_sud) == T,
               suicide_risk_improve = suicide_risk == F & lag(suicide_risk) == T,
               suicide_ideation_improve = suicide_ideation == F & lag(suicide_ideation) == T,
               aggressive_improve = aggressive == F & lag(aggressive) == T,
               sexual_improve = sexual == F & lag(sexual) == T,
               fire_setting_improve = fire_setting == F & lag(fire_setting) == T,
               runaway_improve = runaway == F & lag(runaway) == T,
               risk_improve_num = child_mgmt_improve + behavioral_improve 
               + psychosis_risk_improve + severe_sud_improve 
               + suicide_risk_improve + suicide_ideation_improve
               + aggressive_improve + sexual_improve
               + fire_setting_improve + runaway_improve,
               # Calc diff in subscales from init to most recent
               subscale_school_diff = subscale_school - lag(subscale_school),
               subscale_home_diff = subscale_home - lag(subscale_home),
               subscale_community_diff = subscale_community - lag(subscale_community),
               subscale_behavior_diff = subscale_behavior - lag(subscale_behavior),
               subscale_mood_diff = subscale_mood - lag(subscale_mood),
               subscale_selfharm_diff = subscale_selfharm - lag(subscale_selfharm),
               subscale_substance_diff = subscale_substance - lag(subscale_substance),
               subscale_thinking_diff = subscale_thinking - lag(subscale_thinking),
               # Tag youth with changes in any subscale
               change_subscale_log = subscale_school_diff < 0 
               | subscale_home_diff < 0
               | subscale_community_diff < 0
               | subscale_behavior_diff < 0
               | subscale_mood_diff < 0
               | subscale_selfharm_diff < 0
               | subscale_substance_diff < 0
               | subscale_thinking_diff < 0,
               # Define den and num for Severe Impairment measure
               severe_impair_den = lag(subscale_school) == 30
               | lag(subscale_home) == 30 
               | lag(subscale_community) == 30 
               | lag(subscale_behavior) == 30
               | lag(subscale_mood) == 30
               | lag(subscale_selfharm) == 30
               | lag(subscale_substance) == 30
               | lag(subscale_thinking) == 30,
               severe_impair_num = (lag(subscale_school) == 30
                                    | lag(subscale_home) == 30 
                                    | lag(subscale_community) == 30 
                                    | lag(subscale_behavior) == 30
                                    | lag(subscale_mood) == 30
                                    | lag(subscale_selfharm) == 30
                                    | lag(subscale_substance) == 30
                                    | lag(subscale_thinking) == 30)
               & (subscale_school < 30
                  & subscale_home < 30 
                  & subscale_community < 30 
                  & subscale_behavior < 30
                  & subscale_mood < 30
                  & subscale_selfharm < 30
                  & subscale_substance < 30
                  & subscale_thinking < 30),
               # Define den and num for Pervasive Behavioral Impairment measure
               change_pbi_den_log = lag(subscale_school) %in% c(20:30) 
               & lag(subscale_home) %in% c(20:30)
               & lag(subscale_behavior) %in% c(20:30),
               change_pbi_num_log = lag(subscale_school) %in% c(20:30) 
               & lag(subscale_home) %in% c(20:30)
               & lag(subscale_behavior) %in% c(20:30)
               & subscale_school %in% c(0:19) 
               & subscale_home %in% c(0:19)
               & subscale_behavior %in% c(0:19)
        ) %>%
        ungroup() %>%
        select(fake_episode_id,fake_id,cmh,client_status,episode_num,assess_ord,
               episode_start,episode_end,episode_elapsed,episode_length,
               assess_type,assess_date,interval,max_date,
               subscale_school:subscale_thinking,
               subscale_school_diff:change_subscale_log,
               severe_impair_den,severe_impair_num,
               change_pbi_den_log, change_pbi_num_log,
               score_total,total_diff,score_total_diff,meaning_improve,improvement,
               child_mgmt_improve:risk_improve_num,change_impair,change_behavior,
               improve_one_more) %>%
        suppressMessages()
      
    })
    
    outcome_agg <- reactive({
      
      tx_status <- 
        if (input$radio_status == "Either") {c("Active","Inactive","Inferred Inactive")
        } else if (input$radio_status == "Inactive") { c("Inactive","Inferred Inactive")
        } else input$radio_status
      
      filt_type <-
        if (input$radio_type == "Initial") {c("Initial CAFAS","Initial PECFAS","Revised Initial")
        } else if (input$radio_type == "All") {unique(scrub_fas$assess_type)
        } else input$radio_type
      
      outcome_df <- outcome_df()
      
      if (input$agency == "All") {
        outcome_df %<>% 
          filter(as.Date(max_date) >= input$dateRange[1]
                 & as.Date(max_date) <= input$dateRange[2]
                 & client_status %in% tx_status
                 & assess_type %in% filt_type
                 )
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        outcome_df %<>% 
          filter(as.Date(max_date) >= input$dateRange[1]
                 & as.Date(max_date) <= input$dateRange[2]
                 & client_status %in% tx_status
                 & assess_type %in% filt_type
                 & cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Aggregate data
      
      outcome_df %>%
        filter(is.na(score_total_diff) == F) %>% # rm init assessments & episodes w/ 1 rating
        group_by(cmh) %>%
        summarize(
          meaningimprove_d = n_distinct(fake_episode_id),
          meaningimprove_n = sum(meaning_improve, na.rm = T),
          changeimpair_d = sum(severe_impair_den, na.rm = T),
          changeimpair_n = sum(severe_impair_num, na.rm = T),
          changepbi_d = sum(change_pbi_den_log, na.rm = T),
          changepbi_n = sum(change_pbi_num_log, na.rm = T),
          changesubscale_d = n_distinct(fake_episode_id),
          changesubscale_n = sum(change_subscale_log, na.rm = T)
        ) %>%
        mutate(
          meaningimprove_p = round(meaningimprove_n / meaningimprove_d * 100, digits = 1),
          changeimpair_p = round(changeimpair_n / changeimpair_d * 100, digits = 1),
          changepbi_p = round(changepbi_n / changepbi_d * 100, digits = 1),
          changesubscale_p = round(changesubscale_n / changesubscale_d * 100, digits = 1)
        ) %>%
        filter(is.na(cmh) == F) %>%
        ungroup() %>% 
        droplevels() %>%
        gather(name,num,meaningimprove_d:changesubscale_p) %>%
        mutate(measure = gsub("_.*$", "", name),
               numtype = gsub("^.*_", "", name),
               numtype = recode(numtype,
                                "'d' = 'denominator';
                                'n' = 'numerator';
                                'p' = 'percent'")) %>%
        select(cmh,measure,numtype,num) %>%
        spread(numtype,num) %>%
        mutate(measure_desc = recode(measure,
                                     "'changeimpair' = 'No longer severely impaired';
                                     'changepbi' = 'No longer pervasively behaviorally impaired';
                                     'changesubscale' = 'Improvement on one or more subscale';
                                     'meaningimprove' = 'Meaningful overall improvement'"))
      
    })
    
    outcome_avg <- reactive({
      
      tx_status <- 
        if (input$radio_status == "Either") {c("Active","Inactive","Inferred Inactive")
        } else if (input$radio_status == "Inactive") { c("Inactive","Inferred Inactive")
        } else input$radio_status

      filt_type <-
        if (input$radio_type == "Initial") {c("Initial CAFAS","Initial PECFAS","Revised Initial")
        } else if (input$radio_type == "All") {unique(scrub_fas$assess_type)
        } else input$radio_type
      
      if (input$agency == "All") {
        outcome_df %<>% 
          filter(as.Date(assess_date) >= input$dateRange[1]
                 & as.Date(assess_date) <= input$dateRange[2]
                 & client_status %in% tx_status
                 & assess_type %in% filt_type
                 )
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        outcome_df %<>% 
          filter(as.Date(assess_date) >= input$dateRange[1]
                 & as.Date(assess_date) <= input$dateRange[2]
                 & client_status %in% tx_status
                 & assess_type %in% filt_type
                 & cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Identify individuals with 2 assessments in a given episode
      # Since comparison can only be made across 2 or more assessments
      has_2 <- 
        outcome_df %>% 
        group_by(fake_episode_id) %>% 
        summarize(n = n()) %>%
        filter(n == 2)
      
      outcome_avg <-
        outcome_df %>%
        filter(fake_episode_id %in% has_2$fake_episode_id) %>%
        group_by(cmh,interval) %>%
        summarize(n = n(),
                  School = mean(subscale_school),
                  Home = mean(subscale_home),
                  Community = mean(subscale_community),
                  Behavior = mean(subscale_behavior),
                  Mood = mean(subscale_mood),
                  SelfHarm = mean(subscale_selfharm),
                  Substance = mean(subscale_substance),
                  Thinking = mean(subscale_thinking)) %>%
        gather(subscale,avg,School:Thinking) %>%
        ungroup() %>%
        filter(is.na(cmh) == F) %>%
        select(-n)
      
    })
    
    model_df <- reactive({
      
      # This function builds the initial dataframe to be used in the 
      # linear model outputs, based on user-selected variables
      #  It is not used directly in visualizations, but passed through 
      # 'model_df_rev' based on user selection to include/exclude outliers
      
      # Filter by date range to ensure comaparable time frames
      if (input$predictor == "Length of episode") {
        # Since LOS is derived from CAFAS data, allow full date range
        df <- cafasInput()
      } else 
        df <- cafasInput() %>%
        # No service data prior to 2014-10-01
        filter(assess_date >= "2014-10-01")
      
      # Exclude potential data issues
      if (input$rm_data_issues2 == T) {
        df <-
          df %>%
          filter(no_svcs_ever == F & no_xwalk == F) %>%
          droplevels()
      } else df <- df
      
      df %<>%
        mutate(episode = as.factor(fake_episode_id)) %>%
        group_by(episode,cmh) %>%
        summarize(
          avg_score = mean(score_total, na.rm = T),
          max_score = max(score_total, na.rm = T),
          init_score = max(score_total[assess_ord == 1],na.rm = T),
          init_three = max(score_total[assess_ord <= 3],na.rm = T),
          last_score = max(score_total[assess_date == max(assess_date)],na.rm = T),
          los = max(episode_length),
          est_hrs = sum(est_hrs),
          est_cost = sum(est_cost, na.rm = T),
          days_between = sum(days_between, na.rm = T)
        ) %>%
        mutate(
          chg_score = last_score - init_score,
          init_score = ifelse(is.infinite(init_score), NA, init_score),
          init_three = ifelse(is.infinite(init_three), NA, init_three),
          est_cost_mo = round(est_cost/days_between*30, digits = 2),
          est_hrs_mo = est_hrs/days_between*30 
        ) %>%
        ungroup() %>%
        filter(los >= 0)
      
      
      # Select predictor variable for use 
      
      if (input$predictor == "Initial CAFAS Score in Episode") {
        df %<>% rename(predictor = init_score)
      } else if (input$predictor == "Highest of Initial 3 Scores in Episode") {
        df %<>% rename(predictor = init_three)
      } else if (input$predictor == "Average CAFAS Score during Episode") {
        df %<>% rename(predictor = avg_score)
      } else if (input$predictor == "Highest CAFAS Score during Episode") {
        df %<>% rename(predictor = max_score)
      } else if (input$predictor == "Change in CAFAS Score during Episode") {
        df %<>% rename(predictor = chg_score)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Select response variable for use
      
      if (input$response == "Length of episode") {
        df %<>% rename(response = los)
      } else if (input$response == "Estimated cost of episode") {
        df %<>% rename(response = est_cost)
      } else if (input$response == "Estimated monthly cost") {
        df %<>% rename(response = est_cost_mo)
      } else if (input$response == "Estimated hours of service") {
        df %<>% rename(response = est_hrs)
      } else if (input$response == "Estimated hours of service per month") {
        df %<>% rename(response = est_hrs_mo)
      } else print(paste0("Error.  Unrecognized input."))

      
      # Filter out infinite and NA values
      df %<>% filter(is.infinite(response) == F & is.na(response) == F)
      
      # Manage output for "All" vs. individual agency selection
      
      if (input$agency == "All") {
        
        df 
        
      } else if (input$agency %in% levels(unique(scrub_fas$cmh))) {
        
        # If an individual agency is selected, build a model showing interaction 
        # of each other agency with the predictor
        
        # dmyVars1 creates data frame with dummy variable for each level of cmh
        dmyVars <- data.frame(predict(dummyVars(" ~ cmh", data = df), newdata = df))
        
        # Additional dummy variable for each cmh interaction with predictor
        dmyVars %<>% mutate_each(funs(pred = .*df$predictor), starts_with("cmh"))
        
        df %>%
          # column bind original data frame with dmyVars
          cbind(dmyVars) %>%
          # Remove columns associated with selected cmh
          select(-contains(input$agency)) 
        
      } else print(paste0("Error.  Unrecognized input."))
      
    })
    
    model_op <- reactive({
      
      # This function builds the initial linear model to be used in the 
      # linear model outputs, based on user-selected variables
      #  It is not used directly in visualizations, but passed through 
      # 'model_op_rev' based on user selection to include/exclude outliers
      
      # Manage output for "All" vs. individual agency selection
      
      if (input$agency == "All") {
        
        # Apply linear model for each CMH
        
        m <- 
          model_df() %>% 
          group_by(cmh) %>% 
          do(fit = lm(response ~ predictor, .))
        
      } else if (input$agency %in% levels(unique(scrub_fas$cmh))) {
        
        # Create linear model
        m <- 
          model_df() %>%
          # Subset response, original predictor column and created dummy variables
          select(response,predictor,starts_with("cmh.")) %>%
          lm(response ~ ., data = .)
        
      } else print(paste0("Error.  Unrecognized input."))
      
    })
    
    model_df_rev <- reactive({
      
      if (input$remove_out == F) {
        
        # Keep original data
        model_df()
        
      } else if (input$remove_out == T) {
        
        # Manage output for "All" vs. individual agency selection
        
        if (input$agency == "All") {
          
          # Make new dataset with outliers removed
          model_op() %>%
            augment(fit, se = TRUE) %>%
            # Define outliers as > 4x mean cooksd
            mutate(outlier = .cooksd > 4*mean(.cooksd)) %>%
            # Rm outliers
            filter(outlier == F) %>%
            # Rm values from model and outlier flag
            select(-starts_with("."), -outlier) 
          
        } else if (input$agency %in% levels(unique(scrub_fas$cmh))) {
          
          # Make new dataset with outliers removed
          model_op() %>%
            augment(model_df(), se = TRUE) %>%
            # Define outliers as > 4x mean cooksd
            mutate(outlier = .cooksd > 4*mean(.cooksd)) %>%
            # Rm outliers
            filter(outlier == F) %>%
            # Rm values from model and outlier flag
            select(-starts_with("."), -outlier)
          
        } else print(paste0("Error.  Unrecognized input."))
        
      } else print(paste0("Error.  Unrecognized input."))
      
    })
    
    model_op_rev <- reactive({
      
      if (input$agency == "All") {
        
        model_df_rev() %>%
          # Rebuild models for each group
          group_by(cmh) %>%
          do(fit = lm(response ~ predictor, .)) 
        
      } else if (input$agency %in% levels(unique(scrub_fas$cmh))) {
        
        # Create linear model
        model_df_rev() %>%
          # Subset response, original predictor column and created dummy variables
          select(response,predictor,starts_with("cmh.")) %>%
          lm(response ~ ., data = .)
        
      } else print(paste0("Error.  Unrecognized input.")) 
      
    })
    
    elig_df <- reactive({
      
      if (input$agency == "All") {
        fas_filt <- cafasInput()
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        fas_filt <- cafasInput() %>% filter(cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Exclude all CAFAS scores where LOC is marked as "Assessment only"
      if (input$exclude_ao == T) {
        fas_filt <- 
          fas_filt %>%
          filter(LOC != "Assessment Only") %>%
          droplevels()
      } else fas_filt <- fas_filt
      
      # Exclude potential data issues
      if (input$rm_data_issues3 == T) {
        fas_filt <-
          fas_filt %>%
          filter(no_svcs_ever == F & no_xwalk == F) %>%
          droplevels()
      } else fas_filt <- fas_filt
      
      if (input$only_init_elig == T) {
        fas_filt <- 
          fas_filt %>%
          filter(assess_type %in% c("Initial CAFAS","Initial PECFAS")) %>%
          droplevels()
      } else fas_filt <- fas_filt
      
      fas_filt %>%
        filter(
          days_between >= input$days_btwn_elig[1]
          & days_between <= input$days_btwn_elig[2]) %>%
        group_by(cmh,elig_status) %>%
        summarize(
          n = n()
        ) %>%
        ungroup() %>%
        group_by(cmh) %>%
        mutate(
          all = sum(n),
          pct = round(n / all * 100, digits = 1)
        ) %>%
        ungroup()
      
    })
    
    all_line <- reactive({
      
      # Filter by selected agency
      if (input$agency == "All") {
        fas_filt <- cafasInput()
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        fas_filt <- cafasInput() %>% filter(cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
      fas_filt %>%
        filter(fake_episode_id %in% sample(unique(fake_episode_id), 
                                           size = input$num_kids)) %>%
        select(fake_episode_id, episode_elapsed, score_total) %>%
        arrange(fake_episode_id, episode_elapsed) %>% 
        spread(fake_episode_id,score_total)
      
    })
    
    clusterInput <- reactive({
      
      cluster_df <- 
        cafasInput() %>% 
        filter(is.na(fake_id) == FALSE) 
      
      # Filter based on selection
      if (input$need_filt == "intake only") {
        cluster_df <- 
          cluster_df %>% # Remove empty randomized IDs
          # Exclude discharge assessments, where assessment date is episode end date
          filter(assess_date != episode_end) %>% 
          ungroup() %>% droplevels()
      } else if (input$need_filt == "all assessments") {
        cluster_df <- cluster_df %>% ungroup() %>% droplevels()
      } else paste0("Error: Unexpected input.")
      
      #  Make dataframe for use in cluster analyses
      cluster_df %>%
        select(starts_with("subscale_")) %>%
        rename(school = subscale_school, 
               home = subscale_home, 
               community = subscale_community,
               behavior = subscale_behavior, 
               mood = subscale_mood, 
               selfharm = subscale_selfharm, 
               substance = subscale_substance,
               thinking = subscale_thinking) %>%
        mutate_each(funs( as.numeric(scale(.) ))) %>% 
        filter(complete.cases(.))
      
    })
    
    cluster_km <- reactive({
      
      clusterInput() %>%
        kmeans(centers = input$need_rows)
      
    })
    
    ## Build Reactive UI Elements
    
    # Workaround for 'shinydashboard' weirdness, initialize an empty UI-chunk
    # so that uiOutput() can render in menuSubItem()
    output$select_prog <- renderUI({})
    outputOptions(output, "select_prog", suspendWhenHidden = FALSE)
    
    output$select_prog <- renderUI({
      
      if (input$agency == "All") {
        selectInput(
          "select_prog",
          label = "Select Program Type:",
          # Force selection of all program types at regional level, since definitions vary
          choices = c("All"), 
          selected = "All"
        )
      } else 
        selectInput(
          "select_prog",
          label = "Select Program Type:",
          choices = c("All", levels(droplevels(scrub_fas$program_type[scrub_fas$cmh == input$agency]))), 
          selected = "All"
        )
    })
    
    # Workaround for 'shinydashboard' weirdness, initialize an empty UI-chunk
    # so that uiOutput() can render in menuSubItem()
    output$select_episode <- renderUI({})
    outputOptions(output, "select_episode", suspendWhenHidden = FALSE)
    
    output$select_episode <- renderUI({
      selectInput("select_episode",
                  label = "Select an episode grouper:",
                  choices = c("Common Episodes","System Default"), 
                  selected = "Common Episodes")
    })
    
    # Workaround for 'shinydashboard' weirdness, initialize an empty UI-chunk
    # so that uiOutput() can render in menuSubItem()
    output$select_version <- renderUI({})
    outputOptions(output, "select_version", suspendWhenHidden = FALSE)
    
    output$select_version <- renderUI({
      
      if (input$select_episode == "System Default") {
        
        selectInput("select_version",
                    label = "Version of tool:",
                    choices = c(levels(unique(as.factor(scrub_fas$version)))), 
                    selected = "CAFAS")
        
      } else if (input$select_episode == "Common Episodes") {
        
        selectInput("select_version",
                    label = "Version of tool:",
                    choices = c("Both versions",
                                levels(unique(as.factor(scrub_fas$version)))), 
                    selected = "Both versions")
        
      } else paste0("Error: Unexpected input.")
    })
    
    output$select_measure <- renderUI({
      
      validate(
        need(expr = !is.na(outcome_agg()$measure_desc),
             message = "Building dataframes...")
      )
      
      selectInput("select_measure",
                  label = "Select a measure:",
                  choices = levels(unique(as.factor(outcome_agg()$measure_desc))), 
                  selected = "meaningimprove")
      
    })
    
    output$num_kids <- renderUI({
      
      numericInput(
        inputId = "num_kids", 
        label = NULL,
        value = 500,
        min = 100,
        max = 1000,
        step = 100,
        width = '70px'
      )
      
    })
    
    output$assess_num <- renderUI({
      
      sliderInput(
        "assess_num", 
        "Select assessments by their order in an episode:", 
        min = min(cafasInput()$assess_ord), 
        max = max(cafasInput()$assess_ord), 
        value = c(min(cafasInput()$assess_ord),
                  max(cafasInput()$assess_ord)),
        step = 1
      )
      
    })
    
    output$assess_num_again <- renderUI({
      sliderInput(
        "assess_num_again", 
        "Select assessments by their order in an episode:", 
        min = min(cafasInput()$assess_ord), 
        max = max(cafasInput()$assess_ord), 
        value = c(min(cafasInput()$assess_ord),
                  max(cafasInput()$assess_ord)),
        step = 1
      )
    })
    
    output$days_between <- renderUI({
      sliderInput(
        "days_between",
        "Select assessments by the number of days occurring between consecutive assessments:",
        min = min(cafasInput()$days_between),
        max = max(cafasInput()$days_between),
        value = c(min(cafasInput()$days_between),
                  max(cafasInput()$days_between))
      )
    })
    
    output$days_btwn_elig <- renderUI({
      sliderInput(
        "days_btwn_elig",
        "Number of days between consecutive assessments:",
        min = min(cafasInput()$days_between),
        max = max(cafasInput()$days_between),
        value = c(30,365)
      )
    })
    
    output$days_between_again <- renderUI({
      sliderInput(
        "days_between_again",
        "Select assessments by the number of days occurring between consecutive assessments:",
        min = min(cafasInput()$days_between),
        max = max(cafasInput()$days_between),
        value = c(min(cafasInput()$days_between),
                  max(cafasInput()$days_between))
      )
    })
    
    output$k_vars <- renderUI({
      
      # Create multiple dropdowns, each displaying a different colname
      
      tagList(
        selectInput("kmPlot_x", "X-axis: ",
                    choices = names(clusterInput()),
                    selected = names(clusterInput())[1]),
        selectInput("kmPlot_y", "Y-axis: ",
                    choices = names(clusterInput()),
                    selected = names(clusterInput())[2]),
        selectInput("kmPlot_z", "Z-axis: ",
                    choices = names(clusterInput()),
                    selected = names(clusterInput())[3]),
        selectInput("kmPlot_size", "Size: ",
                    choices = names(clusterInput()),
                    selected = names(clusterInput())[4])
      )
      
    })
    
    ## Build DataViz
    
    output$outcome_bar <- renderPlotly({
      
      validate(
        need(input$select_measure != "", "Processing measures...")
      )
      
      withProgress(message = 'Calculating outcomes',
                   detail = 'based on selected filters',
                   value = 0.1, 
                   {outcome_agg() %>%
                       filter(measure_desc == input$select_measure) %>%
                       # Reorder factor by value for barchart ordering
                       mutate(cmh = fct_reorder(cmh, x = percent, 
                                                fun = max, .desc = T),
                              avg_rate = round(sum(numerator, na.rm = T)/sum(denominator, na.rm = T)*100,
                                               digits = 1)) %>%
                       plot_ly(x = ~cmh, y = ~percent) %>% 
                       add_bars(name = input$select_measure,
                                colors = "#00A08A",
                                hoverinfo = "text",
                                text = ~paste("Percent: ", percent,
                                              "<br>Numerator: ", numerator,
                                              "<br>Denominator: ", denominator)) %>%
                       add_lines(x = ~cmh,
                                 y = ~avg_rate,
                                 line = list(dash = 5,
                                             color = "#555555"),
                                 name = "Weighted Average",
                                 yaxis = "y") %>%
                       layout(xaxis = list(title = "",
                                           showticklabels = T),
                              yaxis = list(title = "%", 
                                           range = c(0, 100)),
                              legend = list(xanchor = "right", yanchor = "top", x = 1, y = 1, 
                                            font = list(size = 10)),
                              margin = list(b = 100))  
                   })
    })
    
    output$hist_fas <- renderPlotly({

      if ( input$agency == "All" ) {
        fas_filt <- hist_df()
        notetxt <- "Distribution of total scores<br>across all CMHSPs"
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        fas_filt <- hist_df() %>% filter(cmh == input$agency)
        notetxt <- paste0("Distribution of total scores<br>at ",input$agency)
      } else
        print(paste0("Error.  Unrecognized input."))
      
      # Exclude potential data issues
      if (input$rm_data_issues1 == T) {
        fas_filt <-
          fas_filt %>%
          filter(no_svcs_ever == F & no_xwalk == F) %>%
          droplevels()
      } else fas_filt <- fas_filt
      
      
      minx <- min(fas_filt$score_total)
      maxx <- max(fas_filt$score_total)
      sizex <- (maxx - minx) / input$sni_bins
      
      if ( input$split == "Stacked") {
        
        hist <-
          fas_filt %>%
          group_by(LOC) %>%
          plot_ly(x = ~score_total) %>%
          add_histogram(opacity = 0.6, 
                        autobinx = F,
                        color = ~LOC,
                        colors = soft_12,
                        xbins = list(start = minx, 
                                     end = maxx, 
                                     size = sizex),
                        text = ~paste("kids with scores in this range received ", 
                                      tolower(LOC), " following the assessment"),
                        hoverinfo = "y+text", 
                        showlegend = T) %>%
          layout(title = "Distribution of Scores by Level of Care",
                 barmode = "stack",
                 xaxis = list(title = "CAFAS Total Score", 
                              tickmode = "array",range = c(minx, maxx), autorange = F,
                              autotick = F, tick0 = minx, dtick = sizex),
                 yaxis = list(title = "Number of assessments", showgrid = F),
                 legend = list(xanchor = "right", yanchor = "top", x = 1, y = 1, 
                               font = list(size = 10)),
                 annotations = list(x = minx, xanchor = "left", 
                                    y = 1, yanchor = "top", yref = "paper",
                                    showarrow = F, align = "left",
                                    text = notetxt)) 
        
        max_hist <- max(hist(fas_filt$score_total,
                             breaks = input$sni_bins)$counts,na.rm = TRUE)
        
        ifelse(
          input$central == "Mean",
          yes = hist <- hist %>% add_lines(x = rep(mean(fas_filt$score_total), 
                                                   each = 2), 
                                           y = c(0,max_hist),
                                           line = list(dash = 5),
                                           marker = list(color = "#DA824F"),
                                           name = "Mean score",
                                           hoverinfo = "x",
                                           xaxis = "x"),
          no  = hist <- hist %>% add_lines(x = rep(median(fas_filt$score_total), 
                                                   each = 2), 
                                           y = c(0,max_hist),
                                           line = list(dash = 5),
                                           marker = list(color = "#DA824F"),
                                           name = "Median score",
                                           hoverinfo = "x",
                                           xaxis = "x")
        )
        
        if (input$select_version == "CAFAS") {
          
          hist %>%
            add_lines(
              x = ~rep(120, each = 2),
              y = c(0,max_hist),
              line = list(dash = 5),
              color = I("gray"),
              text = "Minimum score for SED Waiver",
              hoverinfo = "x+text",
              xaxis = "x",
              showlegend = FALSE
            ) %>%
            add_lines(
              x = ~rep(80, each = 2),
              y = c(0,max_hist),
              line = list(dash = 5),
              color = I("gray"),
              text = "Minimum score for Home-based Services",
              hoverinfo = "x+text",
              xaxis = "x",
              showlegend = FALSE
            )
          
        } else if (input$select_version == "PECFAS") {
          
          hist %>%
            add_lines(
              x = ~rep(90, each = 2),
              y = c(0,max_hist),
              line = list(dash = 5),
              color = I("gray"),
              text = "Minimum score for SED Waiver",
              hoverinfo = "x+text",
              xaxis = "x",
              showlegend = FALSE
            )
          
        } else 
          
          hist
        
      } else if ( input$split == "Facetted" ) {
        
        one_plot <- function(d) {
          
          max_hist <- max(hist(d$score_total)$counts,na.rm = TRUE)
          
          hist <-
            d %>%
            group_by(LOC) %>%
            plot_ly(x = ~score_total) %>%
            add_histogram(
              opacity = 0.6,
              autobinx = F,
              color = ~LOC,
              colors = soft_12,
              xbins = list(start = minx, 
                           end = maxx, 
                           size = sizex),
              text = ~paste("kids with scores in this range received ", 
                            tolower(LOC), " following the assessment"),
              hoverinfo = "y+text"
            ) %>%
            layout(
              title = "Distribution of Scores by Level of Care"
            )

          ifelse(
            input$central == "Mean",
            yes = hist <- hist %>% add_lines(x = ~rep(round(mean(d$score_total), digits = 1), each = 2),
                                             y = c(0,max_hist),
                                             line = list(dash = 5),
                                             color = I("black"),
                                             name = "Mean score",
                                             text = "Mean score",
                                             hoverinfo = "x+text",
                                             xaxis = "x",
                                             showlegend = FALSE),
            no  = hist <- hist %>% add_lines(x = ~rep(round(median(d$score_total), digits = 1), each = 2),
                                             y = c(0,max_hist),
                                             line = list(dash = 5),
                                             color = I("black"),
                                             name = "Median score",
                                             text = "Median score",
                                             hoverinfo = "x+text",
                                             xaxis = "x",
                                             showlegend = FALSE)
          )

          if(input$select_version == "CAFAS") {

            hist %>%
              add_lines(
                x = ~rep(120, each = 2),
                y = c(0,max_hist),
                line = list(dash = 5),
                color = I("gray"),
                text = "Minimum score for SED Waiver",
                hoverinfo = "x+text",
                xaxis = "x",
                showlegend = FALSE
              ) %>%
              add_lines(
                x = ~rep(80, each = 2),
                y = c(0,max_hist),
                line = list(dash = 5),
                color = I("gray"),
                text = "Minimum score for Home-based Services",
                hoverinfo = "x+text",
                xaxis = "x",
                showlegend = FALSE
              )

          } else if (input$select_version == "PECFAS") {

            hist %>%
              add_lines(
                x = ~rep(90, each = 2),
                y = c(0,max_hist),
                line = list(dash = 5),
                color = I("gray"),
                text = "Minimum score for SED Waiver",
                hoverinfo = "x+text",
                xaxis = "x",
                showlegend = FALSE
              )

          } else

            hist

        }
        
        fas_filt %>%
          split(.$LOC) %>%
          lapply(one_plot) %>% 
          subplot(
            nrows = 7, 
            shareX = TRUE, 
            titleX = FALSE
          )
        
      } else print(paste0("Error.  Unrecognized input."))
      
    })
    
    output$by_cmh_dt <- renderDataTable({
      
      df <-
      cafasInput() %>%
        filter(
          assess_ord >= input$assess_num_again[1]
          & assess_ord <= input$assess_num_again[2]
          & days_between >= input$days_between_again[1]
          & days_between <= input$days_between_again[2]) %>%
        group_by(cmh) %>%
        summarize(n = n(),
                  med = median(score_total),
                  mean = round(mean(score_total), digits = 1),
                  sd = round(sd(score_total), digits = 1)) %>%
        ungroup() %>%
        arrange(desc(med))
      
      # Make datatable
      df %>%
        datatable(rownames = FALSE,
                  caption = 'Summary of Total Scores by Agency',
                  colnames = c('Agency','Number','Median','Avg','St. Dev.'),
                  extensions = c('Responsive','Buttons'),
                  options = list(dom = 't', buttons = c('colvis'),
                                 pageLength = length(unique(df$cmh)))) %>%
        #formatStyle(var_names, backgroundColor = styleInterval(brks, clrs)) %>%
        formatStyle('med',
                    background = styleColorBar(df$med, 'lightsteelblue'),
                    backgroundSize = '100% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
      
    })
    
    output$kw_isdiff <- renderText({
      
      df <- cafasInput() %>% 
        filter(assess_ord >= input$assess_num_again[1]
               & assess_ord <= input$assess_num_again[2]
               & days_between >= input$days_between_again[1]
               & days_between <= input$days_between_again[2])
      
      # kruskal-wallis (test of medians)
      pval <- kruskal.test(score_total ~ cmh, data = df)$p.value
      
      paste0("Analysis indicates that ",
             ifelse(pval <= 0.05,
                    yes = "the differences between some of the agencies' medians are statistically significant.",
                    no = "there is not a significant difference between the agencies' medians."),
             "  Expand the tab on the right for comparisons between each pair of agencies to see which ones are different."
             )
      
    })
    
    output$by_cmh_kw <- renderPlotly({
      
      df <- cafasInput() %>% 
        filter(assess_ord >= input$assess_num_again[1]
               & assess_ord <= input$assess_num_again[2]
               & days_between >= input$days_between_again[1]
               & days_between <= input$days_between_again[2])
      
      pairwise <- 
        pairwise.wilcox.test(df$score_total, 
                             df$cmh, 
                             p.adjust.method = "bonferroni")$p.value %>% 
        round(., digits = 5)
      
      pairwise %>%
        t() %>%
        plot_ly(
          x = colnames(.), y = row.names(.), z = .
        ) %>% 
        add_heatmap(
          colors = colorRamp(c("lightsteelblue", "white")),
          name = "Agencies with Significant Differences"
        ) %>%
        colorbar(
          title = "p-value",
          limits = c(0, 0.00075758)
        ) %>%
        layout(
          margin = list(l = 140, b = 120),
          xaxis = list(title = "Agency", tickangle = 45),
          yaxis = list(title = "Compared to...")
        )
      
    })
    
    output$hist_box_cmh <- renderPlotly({
      if (input$By_CMH_display == "Facetted histogram") {
        
        one_plot <- function(d) {
          
          max_hist <- max(hist(d$score_total)$counts,na.rm = TRUE)
          
          plot_ly(d, x = ~score_total) %>%
            add_histogram(
              color = ~cmh,
              colors = soft_12,
              text = ~paste("youth",
                            '</br>', cmh),
              hoverinfo = "y+text") %>%
            add_lines(
              x = ~rep(round(mean(d$score_total), digits = 1), each = 2), 
              y = c(0,max_hist),
              line = list(dash = 5),
              color = I("gray"),
              name = "Mean score",
              hoverinfo = "x",
              xaxis = "x",
              showlegend = FALSE
            ) %>%
            layout(
              title = "Distribution of Scores by Agency"
            )
          
        }
        
        # Note that this is not filtered by Agency, in order to allow for comparisons
        
        cafasInput() %>%
          filter(
            assess_ord >= input$assess_num_again[1]
            & assess_ord <= input$assess_num_again[2]
            & days_between >= input$days_between_again[1]
            & days_between <= input$days_between_again[2]) %>%
          split(.$cmh) %>%
          lapply(one_plot) %>% 
          subplot(
            nrows = 3, 
            shareX = TRUE, 
            titleX = FALSE) 
        
      } else if ( input$By_CMH_display == "Boxplot" ) {
        
        # Filter by selected agency
        if (input$agency == "All") {
          
          fas_filt <- cafasInput() %>%
            filter(
              assess_ord >= input$assess_num_again[1]
              & assess_ord <= input$assess_num_again[2]
              & days_between >= input$days_between_again[1]
              & days_between <= input$days_between_again[2]
            )
          
        } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
          
          fas_filt <- cafasInput() %>% 
            filter(
              cmh %in% input$agency
              & assess_ord >= input$assess_num_again[1]
              & assess_ord <= input$assess_num_again[2]
              & days_between >= input$days_between_again[1]
              & days_between <= input$days_between_again[2]
            )
          
        } else print(paste0("Error.  Unrecognized input."))
        
        p <-
          fas_filt %>%
          plot_ly(y = ~score_total, 
                  #color = I("black"), 
                  alpha = 0.2, boxpoints = "suspectedoutliers")
        
        p1 <- p %>% add_boxplot(x = "Overall", color = I("gray80"))
        p2 <- p %>% add_boxplot(x = ~cmh, color = ~cmh)
        
        subplot(
          p1, p2, shareY = TRUE,
          widths = c(0.2, 0.8), margin = 0
        ) %>% 
          hide_legend() %>%
          layout(xaxis = list(title = "", showticklabels = T),
                 yaxis = list(title = "Total Score", 
                              range = c(0, 240)),
                 margin = list(b = 100))
        
      }else print(paste0("Error.  Unrecognized input."))
      
    })
    
    output$elig_serv_dt <- renderDataTable({
      
      elig_df() %>%
        mutate(
          met = case_when(
            elig_status == "Eligible, did not receive services" ~ F,
            elig_status == "Not eligible, received services" ~ F,
            elig_status == "Not eligible, did not receive services" ~ T,
            elig_status == "Eligible, received services" ~ T
          )
        ) %>%
        group_by(cmh, met) %>%
        summarize(
          n = sum(n),
          all = max(all),
          pct = sum(pct)
        ) %>%
        ungroup() %>%
        filter(met == T) %>%
        select(-met) %>%
        arrange(pct) %>%
        datatable(
          caption = "Percent of youth whose services meet SED CAFAS criteria",
          rownames = F,
          colnames = c('CMH',
                       '# meeting criteria',
                       '# assessed',
                       'Percentage'),
          extensions = c('Responsive','Buttons'),
          options = list(
            pageLength = 5,
            lengthMenu = c(5, 10, 15),
            buttons = c('colvis')
          )
        ) %>%
        formatStyle(
          'pct',
          background = styleColorBar(c(0,100), "#83AF9B"),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      
    })
    
    output$elig_serv_bar <- renderPlotly({
      
      elig_df() %>%
        plot_ly(
          x = ~cmh, 
          y = ~pct, 
          color = ~elig_status,
          hoverinfo = 'text',
          text = ~paste0(n, " out of ", all," youth (",pct,"%)<br>",
                         "assessed at ",cmh,"<br>",
                         "were ",elig_status,".")
        ) %>% 
        add_bars(colors = c("#FE4365","#C8C8A9","#83AF9B")) %>% # "#FC9D9A",
        layout(
          #title = paste0("Services Provided following SED Eligibility Assessment"),
          legend = list(
            orientation = 'h',
            y = 1.5,
            xanchor = "center",
            x = 0.5
          ),
          margin = list(b = 100,t = 50),
          barmode = 'stack',
          xaxis = list(title = "",tickangle = 45),
          yaxis = list(title = "% of assessments")
        )
      
    })
    
    output$elig_serv_scatter <- renderPlotly({
      
      if (input$agency == "All") {
        fas_filt <- cafasInput()
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        fas_filt <- cafasInput() %>% filter(cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
      fas_filt %>%
        plot_ly(
          x = ~score_total, 
          y = ~units, 
          #type = "histogram2dcontour"
          color = ~elig_status
        ) %>% 
        add_markers(colors = c("#FE4365","#C8C8A9","#83AF9B"))
      
    })
    
    output$eligible_bar <- renderPlotly({
      
      # Filter by selected agency
      if (input$agency == "All") {
        fas_filt <- cafasInput()
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        fas_filt <- cafasInput() %>% filter(cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Exclude potential data issues
      if (input$rm_data_issues3 == T) {
        fas_filt <-
          fas_filt %>%
          filter(no_svcs_ever == F & no_xwalk == F) %>%
          droplevels()
      } else fas_filt <- fas_filt
      
      if (input$radio_elig_recent == "Only most recent") {
        recency <- c(TRUE) 
      } else if (input$radio_elig_recent == "All but most recent") {
        recency <- c(FALSE)
      } else if (input$radio_elig_recent == "Either") {
        recency <- c(TRUE, FALSE)
      } else paste0("Unrecognized input.  Don't you wish computers knew what you really meant?")
      
      df <-
        fas_filt %>%
        filter(is.na(n_crit) == F
               & most_recent %in% recency
        ) %>%
        mutate(interval = dplyr::recode(assess_type, 
                                        `Exit CAFAS` = "Discharge",
                                        `Exit PECFAS` = "Discharge",
                                        `Initial CAFAS` = "Intake",
                                        `Initial PECFAS` = "Intake",
                                        `Revised Initial` = "Intake",
                                        .default = "In treatment"),
               meet_crit = ifelse(n_crit == 0, 
                                  yes = "Ineligible", 
                                  no = "Eligible")) %>%
        group_by(cmh, interval, meet_crit) %>%
        summarize(n = n()) %>%
        spread(meet_crit, n) %>%
        mutate(Total = Eligible + Ineligible,
               Pct_Elig = round(Eligible/Total*100, digits = 1),
               Pct_Inel = round(Ineligible/Total*100, digits = 1)) %>%
        ungroup() %>%
        droplevels()
      
      
      
      if (input$radio_elig_pct == "Number") {
        
        notetxt <- "How many assessments have occurred where kids did not meet eligibility criteria?"
        
        df %>%
          mutate(cmh = fct_reorder(cmh, x = Ineligible, 
                                   fun = sum, .desc = T)) %>%
          plot_ly(x = ~cmh, y = ~Ineligible) %>% 
          add_bars(color = ~interval, colors = "Set3",
                   text = ~paste("% ineligible at interval: ", Pct_Inel,
                                 "<br>Total assessments: ", Total)) %>%
          layout(barmode = "stack",
                 title = "Youth not meeting eligibility criteria, by interval",
                 xaxis = list(title = ""),
                 yaxis = list(title = "# of assessments"), #, range = c(0, 100)
                 legend = list(xanchor = "right", yanchor = "top", x = 1, y = 1, 
                               font = list(size = 10)),
                 annotations = list(x = 0, xanchor = "left", 
                                    y = 1, yanchor = "top", yref = "paper",
                                    showarrow = F, align = "left",
                                    text = notetxt),
                 margin = list(b = 80))
        
      } else if (input$radio_elig_pct == "Percent")  {
        
        notetxt <- "What percent of the assessments showed kids not meeting eligibility criteria?"
        
        df %>%
          mutate(cmh = fct_reorder(cmh, x = Pct_Inel, 
                                   fun = sum, .desc = T)) %>%
          plot_ly(x = ~cmh, y = ~Pct_Inel) %>%
          add_bars(color = ~interval, colors = "Set2",
                   text = ~paste("# ineligible at interval: ", Ineligible,
                                 "<br>Total assessments: ", Total)) %>%
          layout(title = "Youth not meeting eligibility criteria, by interval",
                 xaxis = list(title = ""),
                 yaxis = list(title = "% of assessments", range = c(0, 100)), 
                 legend = list(xanchor = "right", yanchor = "top", x = 1, y = 1, 
                               font = list(size = 10)),
                 annotations = list(x = 0, xanchor = "left", 
                                    y = 1, yanchor = "top", yref = "paper",
                                    showarrow = F, align = "left",
                                    text = notetxt),
                 margin = list(b = 80))
        
      } else paste0("Uh-oh.  Input is neither number or percent.")
      
      
      
    })
    
    output$inel_days_hist <- renderPlotly({
      
      # Filter by selected agency
      if (input$agency == "All") {
        fas_filt <- cafasInput()
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        fas_filt <- cafasInput() %>% filter(cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Exclude potential data issues
      if (input$rm_data_issues3 == T) {
        fas_filt <-
          fas_filt %>%
          filter(no_svcs_ever == F & no_xwalk == F) %>%
          droplevels()
      } else fas_filt <- fas_filt
      
      df <-
        fas_filt %>% 
        filter(is.na(n_crit) == F) %>%
        select(fake_episode_id,fake_id,cmh,episode_num,
               assess_ord,assess_type,episode_elapsed,n_crit) %>%
        group_by(fake_episode_id,fake_id,cmh,episode_num) %>%
        mutate(meet_crit = n_crit > 0,
               post_inel = lag(meet_crit) == F) %>%
        filter(meet_crit == F | post_inel == T) %>%   # either inel or post inel
        mutate(elapsed = episode_elapsed - lag(episode_elapsed),
               elapsed = ifelse(is.na(elapsed) == T, 
                                yes = 0, no = elapsed),
               since_inel = cumsum(elapsed),
               last_inel = assess_ord == max(assess_ord)) %>%
        filter(last_inel == T # just the last per episode
               & since_inel >= 0) %>% 
        ungroup()
      
      max_hist <- max(hist(df$since_inel, breaks = "FD")$counts,na.rm=TRUE)
      
      df %>%
        plot_ly(x = ~since_inel) %>%
        add_histogram(opacity = 0.6, 
                      hoverinfo = "all", 
                      name = "kids",
                      showlegend = F) %>% 
        add_lines(x = rep(mean(df$since_inel), 
                          each = 2), 
                  y = c(0,max_hist),
                  type = "line",
                  line = list(dash = 5, color = "#DA824F"),
                  name = "Mean",
                  hoverinfo = "x",
                  xaxis = "x") %>%
        layout(title = "Days in service following ineligibility (Distribution)",
               legend = list(xanchor = "right", yanchor = "top", x = 1, y = 1, 
                             font = list(size = 10)),
               xaxis = list(title = "Days in service following ineligibility", 
                            tickmode = "array"),
               yaxis = list(title = "# of youth", showgrid = F))
      
    })
    
    output$define <- renderText({
      
      if ( input$select_measure == "No longer severely impaired" ) {
        paste0("This measure shows the percentage of youth who had one or more 
               severe impairments at their initial assessment and did not have 
               any severe impairments at the most recent CAFAS assessment.  
               Severe impairments are defined as any CAFAS subscales with a 
               score of 30. A severe impairment indicates that the child is 
               at risk of danger or of not being able to live in the community.  
               This measure excludes youth who did not have severe impairments 
               at intake, or who have only received a single assessment.  Note 
               that this indicator does not account for the number of areas in 
               which the youth was severely impaired at intake, and will 
               therefore be harder to reach for individuals with more severe 
               impairments at admission.")
      } else if ( input$select_measure == "No longer pervasively behaviorally impaired" ) {
        paste0("This measure looks at the percentage of youth who were 
               identified as being pervasively behaviorally impaired (PBI) at 
               their initial assessment and who no longer met PBI criteria at 
               their most recent assessment. PBI criteria is defined as having 
               scores indicating severe or moderate impairment (within the 
               20-30 range) on three CAFAS subscales: School, Home, and 
               Behavior Toward Others.")
      } else if ( input$select_measure == "Improvement on one or more subscale" ) {
        paste0("This measure looks at the percentage of youth with improved 
               scores on one or more CAFAS subscales between their initial and 
               most recent assessments.  CAFAS subscales include the following 
               domains: School, Home, Community, Behavior Towards Others, Moods, 
               Self-Harm, Substance Use, and Thinking.")
      } else if ( input$select_measure == "Meaningful overall improvement" ) {
        paste0("This measure shows the percentage of youth with an improvement 
               of 20 points or greater on their CAFAS Total Score from their 
               initial to the Most Recent assessment.")
      } else paste0("Error: Unexpected Measure Name")
      
      })
    
    output$def_model <- renderText({
      
      if (input$agency == "All") {
        paste0(
          "Since all agencies are selected, the table and plots here display 
          the output from ", nlevels(scrub_fas$cmh), " separate linear models of ",
          input$predictor, " as a predictor of ", input$response, "."
        )
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        paste0(
          "Since the agency of ",input$agency," has been selected, the table and 
          plots here display the output from a linear model of ",
          input$predictor, " as a predictor of ", input$response, " at that 
          agency.  All other agencies have been included in the model as 
          interaction values.  The interactions allow us to see whether the 
          relationship of ", input$predictor," and ", input$response,
          " differs significantly between ", input$agency, " and other agencies."
        )
      } else print(paste0("Error.  Unrecognized input."))
      
    })
    
    output$def_slope <- renderText({
      
      paste0(
        "What is the expected change in ", input$response, " for a unit change in ", 
        input$predictor, 
        "?  The slope of the line is the average amount that ", 
        input$response, " is predicted to change when ",
        input$predictor, " increases by one unit, with all other predictors held constant. 
        It describes how steep a line is.  ",
        "Negative values indicate that ", input$response, " is expected to decrease as ", 
        input$predictor, " increases." 
      )
      
    })
    
    output$def_intercept <- renderText({
      
      paste0(
        "The intercept is the predicted value of the ", input$response, " when the ",
        input$predictor, " is zero."
      )
      
    })
    
    output$def_r2 <- renderText({
      
      paste0(
        "Goodness-of-fit is another way of referring to the R-squared value.  ",
        "This value indicates the percentage of the variability in ",
        input$response, " that is explained by the variation in ",
        input$predictor, ".  ",
        "If the model explained 100% of the variance, all of the data points 
        would fall exactly on the fitted regression line.  Note that analyses 
        which attempt to predict human behavior, such as this one, commonly have 
        R-squared values lower than 50%.  ",
        "Also, even if your R-squared value is low, you may still be able to 
        draw meaningful conclusions about how changes in ", input$predictor, 
        " are associated with changes in ", input$response, 
        " if you have significant predictors."
      )
      
    })
    
    output$def_pval <- renderText({
      
      if (input$agency == "All") {
        paste0(
          "Significance refers to the p-value, a commonly used statistic.  ",
          "It is asking the skeptic's question: 'If ", input$response,
          " was not at all affected by ", input$predictor, 
          " how likely is it that we'd see the data we're seeing?'  ",
          "For instance, if the p-value was 0.05 you would find the same effect of ",
          input$predictor, " on ", input$response, " in 5% of cases due to random 
          sampling error.  Note that, while a p-value near 0.05 is often used to indicate 
          that the result may be significant, it’s not until it drops to near 
          0.001 that there is a low chance of a false positive."
        )
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        paste0(
          "P-values for the regression model showing the interaction between ", 
          input$agency," and each other agency show whether or not the 
          regression coefficients significantly differ. In other words, the 
          expected change in ",input$response," for a unit change in ",
          input$predictor, " between ",input$agency," and another agency shows a 
          more significant difference as the p-value approaches zero."
        )
      } else print(paste0("Error.  Unrecognized input."))
      
      
      })
    
    output$regress_bar <- renderPlotly({
      
      unit_chg <-
        # label the type of unit
        if (input$response == "Length of episode") {
          "Days"
        } else if (input$response == "Estimated cost of episode") {
          "Dollars ($)"
        } else if (input$response == "Estimated monthly cost") {
          "Dollars ($)"
        } else if (input$response == "Estimated hours of service") {
          "Hours"
        } else if (input$response == "Estimated hours of service per month") {
          "Hours"
        } else print(paste0("Error.  Unrecognized input."))
      
      notetxt <- 
        paste0("On average, how many ", tolower(unit_chg), " does the ", 
               "<br>", tolower(input$response), 
               "<br>increase for every 10 point increase", 
               "<br>in ", input$predictor, "?")  
      
      # Vary handling of model output depending on 
      # whether one or all agencies are selected
      
      if (input$agency == "All") {
        
        coeff <- 
          model_op_rev() %>%
          tidy(fit) %>%
          group_by(cmh,term) %>%
          mutate_all(funs(round(.,digits = 3))) %>%
          select(estimate) %>%
          group_by(cmh) %>%
          spread(key = term, value = estimate) 
        
        names(coeff)[2:3] <- c("intercept","slope")
        
        df_in <-
          model_op_rev() %>% 
          glance(fit) %>%
          group_by(cmh) %>%
          mutate_all(funs(round(.,digits = 3))) %>%
          select(r.squared, p.value) %>%
          left_join(coeff, by = "cmh") %>%
          select(cmh,slope,intercept,r.squared,p.value) %>%
          mutate(slope10 = slope * 10) %>%
          arrange(desc(slope)) %>%
#          filter(p.value <= 0.05) %>%
          ungroup()
          
      } else if (input$agency %in% levels(unique(scrub_fas$cmh))) {
        
        df_in <-
          model_op_rev() %>%
          tidy() %>%
          mutate(term = gsub("cmh.","",term),
                 term = gsub("_pred","",term),
                 term = gsub("predictor",input$agency,term)) %>%
          filter(term != "(Intercept)") %>%
          group_by(term) %>%
          mutate_all(funs(round(.,digits = 3))) %>%
          select(term, estimate, p.value) %>%
          mutate(slope10 = estimate * 10) %>%
#          filter(p.value <= 0.05) %>%
          rename(cmh = term,
                 slope = estimate) %>%
          ungroup()
        
      } else print(paste0("Error.  Unrecognized input."))
      
      df_in %>% 
        mutate(cmh = fct_reorder(cmh, x = slope10, fun = max, .desc = T)) %>%
        plot_ly(x = ~cmh, y = ~slope10
                # , color = p.value >= 0.05, colors=c("#BBB6C1", "#FFB6C1")
                ) %>% 
        # add_bars(color = I("#FFB6C1"),
        add_bars(hoverinfo = "text",
                 text = ~paste("At ", cmh, " each change of 10 pts ",
                               "<br>in ", input$predictor,
                               "<br>corresponds to an average change of ",
                               "<br>",slope10," ",unit_chg, " in ",
                               "<br>",tolower(input$response),".",
                               "<br>p-value =", p.value)) %>%
        layout(title = "Change in Service Use based on Need",
               xaxis = list(title = "",
                            showticklabels = T),
               yaxis = list(title = unit_chg),
               annotations = list(x = 1, xanchor = "right", xref = "paper",
                                  y = 1, yanchor = "top", yref = "paper",
                                  showarrow = F, align = "left",
                                  text = notetxt),
               margin = list(b = 60)) 
      
    })
    
    output$regress_dt <- renderDataTable({
      
      # Vary handling of model output depending on 
      # whether one or all agencies are selected
      
      if (input$agency == "All") {
        
        coeff <- 
          model_op_rev() %>%
          tidy(fit) %>%
          group_by(cmh,term) %>%
          mutate_all(funs(round(.,digits = 3))) %>%
          select(estimate) %>%
          group_by(cmh) %>%
          spread(key = term, value = estimate) 
        
        names(coeff)[2:3] <- c("intercept","slope")
        
        if(length(input$regress_input) > 0){
          df_in <-
            model_op_rev() %>% 
            glance(fit) %>%
            group_by(cmh) %>%
            mutate_all(funs(round(.,digits = 3))) %>%
            #select(r.squared, p.value) %>%
            #select(contains(input$regress_input)) %>%
            left_join(coeff, by = "cmh")%>%
            select(cmh, slope, one_of(input$regress_input)) %>%
            arrange(desc(slope))
        } else {
          df_in <-
            model_op_rev() %>% 
            glance(fit) %>%
            group_by(cmh) %>%
            mutate_all(funs(round(.,digits = 3))) %>%
            #select(r.squared, p.value) %>%
            #select(contains(input$regress_input)) %>%
            left_join(coeff, by = "cmh")%>%
            select(cmh, slope) %>%
            arrange(desc(slope))
        }
        
        
        if('intercept' %in% input$regress_input){
          if('p.value' %in% input$regress_input){
            if('r.squared' %in% input$regress_input){
              df_in %>%
                datatable(caption = paste0("Relationship of ", input$predictor,
                                           " to ", input$response),
                          rownames = FALSE,
                          colnames = c('CMH',
                                       'Slope',
                                       'Intercept',
                                       'Good fit?',
                                       'Significance'),
                          extensions = c('Responsive','Buttons'),
                          options = list(pageLength = nlevels(scrub_fas$cmh),
                                         dom = 't',
                                         buttons = c('colvis'))) %>%
                formatStyle('slope',
                            background = styleColorBar(df_in$slope, 'lightpink'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center') %>%
                formatStyle('p.value',
                            color = styleInterval(c(0.01, 0.05), c('green', 'black', 'red'))) %>%
                formatStyle('r.squared',
                            background = styleColorBar(df_in$r.squared, 'lightgray'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center')
            } else {
              df_in %>%
                datatable(caption = paste0("Relationship of ", input$predictor,
                                           " to ", input$response),
                          rownames = FALSE,
                          colnames = c('CMH',
                                       'Slope',
                                       'Intercept',
                                       'Significance'),
                          extensions = c('Responsive','Buttons'),
                          options = list(pageLength = nlevels(scrub_fas$cmh),
                                         dom = 't',
                                         buttons = c('colvis'))) %>%
                formatStyle('slope',
                            background = styleColorBar(df_in$slope, 'lightpink'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center') %>%
                formatStyle('p.value',
                            color = styleInterval(c(0.01, 0.05), c('green', 'black', 'red')))
            }
          } else {
            if('r.squared' %in% input$regress_input){
              df_in %>%
                datatable(caption = paste0("Relationship of ", input$predictor,
                                           " to ", input$response),
                          rownames = FALSE,
                          colnames = c('CMH',
                                       'Slope',
                                       'Intercept',
                                       'Good fit?'),
                          extensions = c('Responsive','Buttons'),
                          options = list(pageLength = nlevels(scrub_fas$cmh),
                                         dom = 't',
                                         buttons = c('colvis'))) %>%
                formatStyle('slope',
                            background = styleColorBar(df_in$slope, 'lightpink'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center') %>%
                formatStyle('r.squared',
                            background = styleColorBar(df_in$r.squared, 'lightgray'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center')
            } else {
              df_in %>%
                datatable(caption = paste0("Relationship of ", input$predictor,
                                           " to ", input$response),
                          rownames = FALSE,
                          colnames = c('CMH',
                                       'Slope',
                                       'Intercept'),
                          extensions = c('Responsive','Buttons'),
                          options = list(pageLength = nlevels(scrub_fas$cmh),
                                         dom = 't',
                                         buttons = c('colvis'))) %>%
                formatStyle('slope',
                            background = styleColorBar(df_in$slope, 'lightpink'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center')
            }
          }
        }else{
          if('p.value' %in% input$regress_input){
            if('r.squared' %in% input$regress_input){
              df_in %>%
                datatable(caption = paste0("Relationship of ", input$predictor,
                                           " to ", input$response),
                          rownames = FALSE,
                          colnames = c('CMH',
                                       'Slope',
                                       'Good fit?',
                                       'Significance'),
                          extensions = c('Responsive','Buttons'),
                          options = list(pageLength = nlevels(scrub_fas$cmh),
                                         dom = 't',
                                         buttons = c('colvis'))) %>%
                formatStyle('slope',
                            background = styleColorBar(df_in$slope, 'lightpink'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center') %>%
                formatStyle('p.value',
                            color = styleInterval(c(0.01, 0.05), c('green', 'black', 'red'))) %>%
                formatStyle('r.squared',
                            background = styleColorBar(df_in$r.squared, 'lightgray'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center')
            } else {
              df_in %>%
                datatable(caption = paste0("Relationship of ", input$predictor,
                                           " to ", input$response),
                          rownames = FALSE,
                          colnames = c('CMH',
                                       'Slope',
                                       'Significance'),
                          extensions = c('Responsive','Buttons'),
                          options = list(pageLength = nlevels(scrub_fas$cmh),
                                         dom = 't',
                                         buttons = c('colvis'))) %>%
                formatStyle('slope',
                            background = styleColorBar(df_in$slope, 'lightpink'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center') %>%
                formatStyle('p.value',
                            color = styleInterval(c(0.01, 0.05), c('green', 'black', 'red')))
            }
          } else {
            if('r.squared' %in% input$regress_input){
              df_in %>%
                datatable(caption = paste0("Relationship of ", input$predictor,
                                           " to ", input$response),
                          rownames = FALSE,
                          colnames = c('CMH',
                                       'Slope',
                                       'Good fit?'),
                          extensions = c('Responsive','Buttons'),
                          options = list(pageLength = nlevels(scrub_fas$cmh),
                                         dom = 't',
                                         buttons = c('colvis'))) %>%
                formatStyle('slope',
                            background = styleColorBar(df_in$slope, 'lightpink'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center') %>%
                formatStyle('r.squared',
                            background = styleColorBar(df_in$r.squared, 'lightgray'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center')
            } else {
              df_in %>%
                datatable(caption = paste0("Relationship of ", input$predictor,
                                           " to ", input$response),
                          rownames = FALSE,
                          colnames = c('CMH',
                                       'Slope'),
                          extensions = c('Responsive','Buttons'),
                          options = list(pageLength = nlevels(scrub_fas$cmh),
                                         dom = 't',
                                         buttons = c('colvis'))) %>%
                formatStyle('slope',
                            background = styleColorBar(df_in$slope, 'lightpink'),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center')
            }
          }
        }
        
        
      } else if (input$agency %in% levels(unique(scrub_fas$cmh))) {
        
        df_in <-
          model_op_rev() %>%
          tidy() %>%
          mutate(term = gsub("cmh.","",term),
                 term = gsub("_pred","",term),
                 term = gsub("predictor",input$agency,term)) %>%
          filter(term != "(Intercept)") %>%
          group_by(term) %>%
          mutate_all(funs(round(.,digits = 3))) %>%
          select(term, estimate, p.value)
        
        df_in %>%
          datatable(caption = paste0("Relationship of ", input$predictor,
                                     " to ", input$response, " for ",
                                     input$agency),
                    rownames = FALSE,
                    colnames = c('CMH','Slope','Significance'),
                    extensions = c('Responsive','Buttons'),
                    options = list(pageLength = nlevels(scrub_fas$cmh),
                                   dom = 't',
                                   buttons = c('colvis'))) %>%
          formatStyle('term', fontWeight = styleEqual(input$agency, 'bold')) %>%
          formatStyle('estimate',
                      background = styleColorBar(df_in$estimate, 'lightpink'),
                      backgroundSize = '100% 90%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center') %>%
          formatStyle('p.value',
                      color = styleInterval(c(0.01, 0.05), c('green', 'black', 'red')))
        
      } else print(paste0("Error.  Unrecognized input."))
      
    })
    
    output$regress <- renderPlotly({
      
      if (input$agency == "All") {
        
        # Build plot for multiple models
        
        notetxt <- paste0("Plot of ",input$predictor,"<br>",
                          "as a predictor of ",input$response)
        
        df <-
          model_op_rev() %>%
          augment(fit, se = TRUE) %>%
          group_by(cmh)  %>%
          mutate(outlier = .cooksd > 4*mean(.cooksd),
                 outlier = ifelse(outlier == T,"Outlier","Not an outlier"))
        
        if (input$remove_out == T) {
          
          # If outliers are removed, color by CMH
          
          viz <- 
            df %>%
            plot_ly(x = ~predictor, showlegend = T) %>%
            add_markers(y = ~response, 
                        alpha = 0.1, 
                        color = ~cmh, 
                        text = ~paste(input$response,": ",response,
                                      "<br>",input$predictor,": ",predictor),
                        hoverinfo = "text") %>%
            add_lines(y = ~.fitted,
                      color = ~cmh, 
                      text = ~paste("When the ", input$predictor,
                                    " is ", predictor,
                                    "<br>the model predicts a ",
                                    "<br>", input$response, " of ", 
                                    round(.fitted, digits = 1),
                                    "<br>for kids served by ",cmh),
                      hoverinfo = "text")
          
        } else
          
          # If outliers are not removed, display them
          
          viz <-
          df %>%
          plot_ly(x = ~predictor, showlegend = T) %>%
          add_markers(y = ~response, 
                      alpha = 0.1, 
                      color = ~outlier,
                      colors = c("black", "red"),
                      text = ~paste(input$response,": ",response,
                                    "<br>",input$predictor,": ",predictor),
                      hoverinfo = "text") %>%
          add_lines(y = ~.fitted,
                    color = I("black"),
                    name = "Regression line",
                    text = ~paste("When the ", tolower(input$predictor),
                                  " is ", predictor,
                                  "<br>the model predicts a ",
                                  "<br>", input$response, " of ",
                                  round(.fitted, digits = 1),
                                  "<br>for youth served by ", cmh),
                    hoverinfo = "text")
        
        
      } else if (input$agency %in% levels(unique(scrub_fas$cmh))) {
        
        # Build plot for individual agency model
        
        notetxt <- paste0("Plot of ",input$predictor,"<br>",
                          "as a predictor of ",input$response,"<br>",
                          "for ", input$agency)
        
        df <-
          model_op_rev() %>%
          augment(model_df_rev(), se = TRUE) %>%
          # Only plot selected agency values
          filter(cmh == input$agency) %>%
          mutate(outlier = .cooksd > 4*mean(.$.cooksd),
                 outlier = ifelse(outlier == T,"Outlier","Not an outlier")) %>%
          arrange(.fitted) 
        
        if (input$remove_out == T) {
          
          # If outliers are removed don't color by outlier var
          
          viz <-
            df %>%
            plot_ly(x = ~predictor, showlegend = T) %>%
            add_markers(y = ~response, 
                        alpha = 0.1, 
                        name = "Specific episode",
                        text = ~paste(input$response,": ",response,
                                      "<br>",input$predictor,": ",predictor),
                        hoverinfo = "text") %>%
            add_lines(y = ~.fitted,
                      color = I("slateblue"),
                      name = "Regression line",
                      text = ~paste("When the ", tolower(input$predictor),
                                    " is ", predictor,
                                    "<br>the model predicts a ",
                                    "<br>", input$response, " of ",
                                    round(.fitted, digits = 1),
                                    "<br>for youth served by ",input$agency),
                      hoverinfo = "text")
          
        } else 
          
          # If outliers are not removed, display them
          
          viz <-
          df %>%
          plot_ly(x = ~predictor, showlegend = T) %>%
          add_markers(y = ~response, 
                      alpha = 0.1, 
                      color = ~outlier,
                      colors = c("black", "red"),
                      text = ~paste(input$response,": ",response,
                                    "<br>",input$predictor,": ",predictor),
                      hoverinfo = "text") %>%
          add_lines(y = ~.fitted,
                    color = I("black"),
                    name = "Regression line",
                    text = ~paste("When the ", tolower(input$predictor),
                                  " is ", predictor,
                                  "<br>the model predicts a ",
                                  "<br>", input$response, " of ",
                                  round(.fitted, digits = 1)),
                    hoverinfo = "text")
        
      } else print(paste0("Error.  Unrecognized input."))
      
      viz <-
        viz %>%
        layout(
          #title = notetxt, 
          xaxis = list(title = input$predictor,
                       rangemode = "tozero"),
          yaxis = list(title = input$response),
          annotations = list(x = max(df$predictor, na.rm = T), 
                             xanchor = "right", 
                             y = 1, yanchor = "top", yref = "paper",
                             showarrow = F, align = "right", text = notetxt))
      
      if (input$ribbons == F) {
        
        viz
        
      } else if (input$ribbons == T) {
        viz %>%
          add_ribbons(ymin = ~.fitted - 1.96 * .se.fit,
                      ymax = ~.fitted + 1.96 * .se.fit,
                      name = "SE bands",
                      color = I("gray80"))
      } else print(paste0("Error.  Unrecognized input."))
      
    })
    
    output$all_linechart <- renderDygraph({
      
      all_line() %>%
        dygraph() %>%
        dyAxis("y", label = "Functioning (CAFAS Total)", valueRange = c(0, 240)) %>%
        dyAxis("x", label = "Days since start of episode") %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE,
                    highlightSeriesOpts = list(strokeWidth = 3)) %>%
        dyOptions(connectSeparatedPoints = TRUE,
                  colors = RColorBrewer::brewer.pal(6, "Set2")) %>%
        dyLegend(show = "never") %>% 
        dyLimit(limit = 120, label = "High Impairment", 
                labelLoc = "right") %>%
        dyLimit(limit = 80, label = "Medium Impairment",
                labelLoc = "right") %>%
        dyRangeSelector(dateWindow = c("0", "1460"),height = 20, strokeColor = "")
      
    })
    
    output$need_scree <- renderPlotly({
      
      scree <- function(df) {
        wss <- (nrow(df)-1)*sum(apply(df,2,var))
        for (i in 2:15) wss[i] <- sum(kmeans(df,centers=i)$withinss)
        wss %<>% as.data.frame() 
        names(wss)[1] <- "wss"
        wss %<>% mutate(n_clust = row_number())
        return(wss)
      }
      
      clusterInput() %>%
        scree() %>%
        plot_ly(x = ~n_clust, y = ~wss, height = 200) %>%
        add_lines(alpha = 0.5) %>%
        add_markers(name = "Clusters",
                    hoverinfo = "x") %>%
        layout(xaxis = list(title = "Number of clusters"),
               yaxis = list(title = "Within groups <br>sum of squares")) %>%
        hide_legend()
      
    })
    
    output$need_grp_dt <- DT::renderDataTable({
      
      if (input$dt_clust_type == "k-means clusters") {
        
        tidy_grp <- 
          cluster_km() %>% 
          tidy() %>%
          select(cluster,size,everything(),-withinss) %>%
          mutate_each(funs = funs(round(.,digits = 3)),
                      starts_with("x"))
        
        # Define color interval breaks
        brks <- tidy_grp %>% select(starts_with("x")) %>% 
          quantile(probs = seq(.05, .95, .05), na.rm = TRUE)
        
        clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
        {paste0("rgb(255,", ., ",", ., ")")}  
        
        # Select var names to apply color gradient
        var_names <- tidy_grp %>% select(starts_with("x")) %>% colnames()
        
      } else if (input$dt_clust_type == "hierarchical clusters") {
        
        # Use default dist and hclust methodds to match d3heatmap output
        distances <- dist(clusterInput(), method = "euclidean")
        hc <- hclust(distances, method = "complete")
        cluster <- cutree(hc, k = input$need_rows)
        
        n_obs <-
          clusterInput() %>%
          cbind(cluster) %>%
          mutate(cluster = as.factor(cluster)) %>%
          group_by(cluster) %>%
          summarize(size = n()) 
        
        tidy_grp <-
          clusterInput() %>%
          cbind(cluster) %>%
          mutate(cluster = as.factor(cluster)) %>%
          group_by(cluster) %>%
          summarize_at(vars(school:thinking), mean) %>%
          left_join(n_obs, by = "cluster") %>%
          select(cluster,size,everything()) %>%
          mutate_each(funs = funs(round(.,digits = 3)),
                      school:thinking)
        
        # Define color interval breaks
        brks <- tidy_grp %>% select(school:thinking) %>% as.matrix() %>%
          quantile(probs = seq(.05, .95, .05), na.rm = TRUE)
        
        clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
        {paste0("rgb(255,", ., ",", ., ")")}  
        
        # Select var names to apply color gradient
        var_names <- tidy_grp %>% select(school:thinking) %>% colnames()
        
      } else print(paste0("Error.  Unrecognized input."))
      
      
      # Make datatable
      tidy_grp %>%
        datatable(rownames = FALSE,
                  colnames = c('Cluster','People',colnames(cluster_km()$centers)),
                  extensions = c('Responsive','Buttons'),
                  options = list(dom = 't', buttons = c('colvis'))) %>%
        # Doesn't currently match levels of palette applied to need_km output
        # formatStyle('cluster', 
        #             backgroundColor = styleEqual(unique(tidy_k$cluster),
        #                                          soft_12[1:length(cluster_km()$centers[,1])])) %>%
        formatStyle(var_names, backgroundColor = styleInterval(brks, clrs)) %>%
        formatStyle('size',
                    background = styleColorBar(tidy_grp$size, 'gray'),
                    backgroundSize = '100% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
      
    })
    
    output$need_km <- renderPlotly({
      
      df <-
        cluster_km() %>% 
        augment(clusterInput()) 
      
      # Rename cols based on inputs
      df$xvar <- df[ ,which( colnames(df) == input$kmPlot_x )]
      df$yvar <- df[ ,which( colnames(df) == input$kmPlot_y )]
      df$zvar <- df[ ,which( colnames(df) == input$kmPlot_z )]
      df$sizevar <- df[ ,which( colnames(df) == input$kmPlot_size )]
      
      df %>%
        group_by(.cluster) %>%
        plot_ly(x = ~xvar, 
                y = ~yvar, 
                z = ~zvar,
                p = ~sizevar,
                hoverinfo = 'text',
                text = ~paste(input$kmPlot_x, '(x): ', round(xvar, digits = 2),
                              '</br>', input$kmPlot_y, '(y): ', round(yvar, digits = 2),
                              '</br>', input$kmPlot_z, '(z): ', round(zvar, digits = 2),
                              '</br>', input$kmPlot_size, '(size): ', round(sizevar, digits = 2)),
                color = ~.cluster,
                colors = soft_12) %>%
        add_markers(size = ~sizevar, 
                    sizes = c(10, 500)) %>%
        layout(scene = list(xaxis = list(title = input$kmPlot_x), 
                            yaxis = list(title = input$kmPlot_y), 
                            zaxis = list(title = input$kmPlot_z)))
      
    })
    
    output$need_heat <- renderD3heatmap({
      
      withProgress(message = 'Creating heatmap...',
                   detail = 'Clustering can take awhile...',
                   value = 0.1, 
                   {d3heatmap(clusterInput(), 
                              colors = "Blues",
                              dendrogram = "row",
                              k_row = input$need_rows, 
                              theme = "",
                              yaxis_font_size =  "0pt",
                              show_grid = F)
                   })
      
    })
    
    output$summary_episodes <- renderDataTable({
      
      old <-
        scrub_fas %>%
        select(fake_id,version,
               # Episode vars
               fake_episode_id,episode_num,episode_start,episode_end,assess_ord,
               episode_elapsed,episode_length) %>%
        group_by(fake_episode_id) %>%
        summarize(length = max(episode_length),
                  elapsed = max(episode_elapsed),
                  assess_per = n()) %>%
        ungroup() %>%
        mutate(grouping =  "System Default",
               episode_id = paste0("A",fake_episode_id))  %>%
        select(-fake_episode_id)
      
      new <-
        scrub_fas %>%
        select(fake_id,version,
               # Revised episode vars
               rev_fake_episode_id,rev_episode_num,rev_episode_start,rev_episode_end,rev_assess_ord,
               rev_episode_elapsed,rev_episode_length) %>%
        group_by(rev_fake_episode_id) %>%
        summarize(length = max(rev_episode_length),
                  elapsed = max(rev_episode_elapsed),
                  assess_per = n()) %>%
        ungroup() %>%
        mutate(grouping =  "Common Episodes",
               episode_id = paste0("B",rev_fake_episode_id)) %>%
        select(-rev_fake_episode_id)
      
      old %>%
        rbind(new) %>%
        group_by(grouping) %>%
        summarize(n_episodes = n_distinct(episode_id),
                  avg_length = round(mean(length),digits = 1),
                  avg_elapse = round(mean(elapsed),digits = 1),
                  avg_assess = round(mean(assess_per),digits = 1)) %>%
        datatable(caption = 'Comparison of Episode Grouping Methods',
                  rownames = FALSE,
                  colnames = c('Method',
                               'Episodes identified','Avg length',
                               'Avg elapsed','Avg assessmts per episode'),
                  options = list(dom = 't'))
      
    })
    
    }
    )
