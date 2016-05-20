## explore CAFAS server.R ##

shinyServer(
  function(input, output) {
    
    # Make Reactive Datasets
    
    cafasInput <- reactive({  
      
      tx_status <- if (input$radio_status == "Either") {c("Active","Inactive")
      } else input$radio_status
      
      if (input$agency == "All") {
        scrub_cafas %>% 
          filter(as.Date(assess_date) >= input$dateRange[1]
                 & as.Date(assess_date) <= input$dateRange[2]
                 & client_status %in% tx_status)
      } else if ( input$agency %in% levels(unique(scrub_cafas$cmh)) ) {
        scrub_cafas %>% 
          filter(as.Date(assess_date) >= input$dateRange[1]
                 & as.Date(assess_date) <= input$dateRange[2]
                 & client_status %in% tx_status
                 & cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
    })
    
    outcome_df <- reactive({
      
      cafasInput() %>%
        group_by(fake_episode_id,fake_id,cmh,episode_num) %>%
        mutate(init_CAFAS = assess_ord == min(assess_ord), # for included assessments
               last_CAFAS = assess_ord == max(assess_ord)) %>%
        filter(init_CAFAS == T | last_CAFAS == T) %>%
        mutate(cafas_total_diff = cafas_total - lag(cafas_total), #doesn't work if start of episode not in dataset
               meaning_improve = total_diff >= 20, #total_diff is calc by episode
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
               change_risk_den = lag(child_mgmt) == T 
                                  | lag(behavioral) == T 
                                  | lag(psychosis_risk) == T 
                                  | lag(severe_sud) == T 
                                  | lag(suicide_risk) == T
                                  | lag(suicide_ideation) == T
                                  | lag(aggressive) == T
                                  | lag(sexual) == T
                                  | lag(fire_setting) == T
                                  | lag(runaway) == T, 
               change_risk_num = risk_improve_num >= 1,
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
               severe_impair_num = (lag(subscale_school) == 30 & subscale_school_diff < 0)
                                   | (lag(subscale_home) == 30 & subscale_home_diff < 0)
                                   | (lag(subscale_community) == 30 & subscale_community_diff < 0)
                                   | (lag(subscale_behavior) == 30 & subscale_behavior_diff < 0)
                                   | (lag(subscale_mood) == 30 & subscale_mood_diff < 0)
                                   | (lag(subscale_selfharm) == 30 & subscale_selfharm_diff < 0)
                                   | (lag(subscale_substance) == 30 & subscale_substance_diff < 0)
                                   | (lag(subscale_thinking) == 30 & subscale_thinking_diff < 0),
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
               episode_elapsed,episode_length,assess_type,assess_date,
               subscale_school_diff:change_subscale_log,
               severe_impair_den,severe_impair_num,
               change_pbi_den_log, change_pbi_num_log,
               cafas_total,total_diff,cafas_total_diff,meaning_improve,improvement,
               child_mgmt_improve:change_risk_num,change_impair,change_behavior,
               improve_one_more) %>%
        filter(is.na(cafas_total_diff) == F) # rm init assessments & episodes w/ 1 rating
      
    })
    
    outcome_agg <- reactive ({
      
      # Aggregate data
      outcome_df() %>%
        group_by(cmh) %>%
        summarize(
          meaningimprove_d = n_distinct(fake_episode_id),
          meaningimprove_n = sum(meaning_improve, na.rm = T),
          changerisk_d = sum(change_risk_den, na.rm = T),
          changerisk_n = sum(change_risk_num, na.rm = T),
          changeimpair_d = sum(severe_impair_den, na.rm = T),
          changeimpair_n = sum(severe_impair_num, na.rm = T),
          changepbi_d = sum(change_pbi_den_log, na.rm = T),
          changepbi_n = sum(change_pbi_num_log, na.rm = T),
          changesubscale_d = n_distinct(fake_episode_id),
          changesubscale_n = sum(change_subscale_log, na.rm = T)
        ) %>%
        mutate(
          meaningimprove_p = round(meaningimprove_n / meaningimprove_d *100, digits = 1),
          changerisk_p = round(changerisk_n / changerisk_d *100, digits = 1),
          changeimpair_p = round(changeimpair_n / changeimpair_d *100, digits = 1),
          changepbi_p = round(changepbi_n / changepbi_d *100, digits = 1),
          changesubscale_p = round(changesubscale_n / changesubscale_d *100, digits = 1)
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
                                     'changerisk' = 'Reduction of one or more risk areas';
                                     'changesubscale' = 'Improvement on one or more subscale';
                                     'meaningimprove' = 'Meaningful overall improvement'"))
      
    })
    
    all_line <- reactive({
      
      cafasInput() %>%
        filter(fake_episode_id %in% sample(unique(fake_episode_id), 
                                           size = input$num_kids)) %>%
        select(fake_episode_id, episode_elapsed, cafas_total) %>%
        arrange(fake_episode_id, episode_elapsed) %>% 
        spread(fake_episode_id,cafas_total)
      
    })
    
    ## Build Reactive UI Elements
    
    output$select_measure <- renderUI({
      outcome_agg <- outcome_agg()
      selectInput("select_measure",
                  label = "Select a measure:",
                  choices = levels(unique(as.factor(outcome_agg$measure_desc))), 
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
    
    ## Build DataViz
    
    output$outcome_bar <- renderPlotly({
    
      outcome_agg() %>%
        filter(measure_desc == input$select_measure) %>%
        arrange(desc(percent)) %>%
        plot_ly(x = cmh, y = percent, type = "bar",
                color = cmh, colors = "Set3",
                text = paste("Numerator: ", numerator,
                             "<br>Denominator: ", denominator)) %>%
        layout(xaxis = list(title = "CMHSP", showticklabels = F),
               yaxis = list(title = "%", range = c(0, 100)),
               legend = list(font = list(size = 10)))
      
    })
    
    output$define <- renderText({
      
      if ( input$select_measure == "No longer severely impaired" ) {
        paste0("This measure shows the percentage of youth who had one or more 
               severe impairments at their initial assessment and did not have 
               any severe impairments at the most recent CAFAS assessment.  
               Severe impairments are defined as any CAFAS subscales with a 
               score of 30. This measure excludes youth who did not have severe 
               impairments at intake, or who have only received a single 
               assessment.")
      } else if ( input$select_measure == "No longer pervasively behaviorally impaired" ) {
        paste0("This measure looks at the percentage of youth who were 
               identified as being pervasively behaviorally impaired (PBI) at 
               their initial assessment and who no longer met PBI criteria at 
               their most recent assessment. PBI criteria is defined as having 
               scores indicating severe or moderate impairment (within the 
               20-30 range) on three CAFAS subscales: School, Home, and 
               Behavior Toward Others.")
      } else if ( input$select_measure == "Reduction of one or more risk areas" ) {
        paste0("This measure looks at the percentage of youth who had one or 
               positive risk findings at their initial assessment and who were 
               no longer found to be at risk in those areas on their most recent 
               assessment.  Risk areas assessed include the following: 
               Child management, Behavioral, Psychotic symptoms, Severe substance  
               use disorder, Suicide risk or suicidal ideation, Aggression, 
               Inappropriate sexual behavior, Fire setting, or Runaway 
               behavior. ")
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
        dyRangeSelector(dateWindow = c("0", "365"),height = 20, strokeColor = "")
      
    })
    
  }
)
