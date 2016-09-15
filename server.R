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
      
      # Define lists based on inputs for us in %in% statements belows
      
      tool_version <-
        if (input$select_version == "Both versions") {c("CAFAS","PECFAS")
        } else if (input$select_version == "CAFAS") { c("CAFAS")
        } else if (input$select_version == "PECFAS") { c("PECFAS")
        } else print(paste0("Error.  Unrecognized input."))
      
      tx_status <- 
        if (input$radio_status == "Either") {c("Active","Inactive","Inferred Inactive")
        } else if (input$radio_status == "Inactive") { c("Inactive","Inferred Inactive")
        } else input$radio_status
      
      # Filter for selected agency (or "All")
      
      if (input$agency == "All") {
        scrub_fas %>% 
          filter(as.Date(max_date) >= input$dateRange[1]
                 & as.Date(max_date) <= input$dateRange[2]
                 & client_status %in% tx_status
                 & version %in% tool_version)
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        scrub_fas %>% 
          filter(as.Date(max_date) >= input$dateRange[1]
                 & as.Date(max_date) <= input$dateRange[2]
                 & client_status %in% tx_status
                 & version %in% tool_version
                 & cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
    })
    
    outcome_df <- reactive({
      
      # Create dataframe for calculating measures
      
      cafasInput() %>%
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
               child_mgmt_improve:change_risk_num,change_impair,change_behavior,
               improve_one_more) 
      
    })
    
    outcome_agg <- reactive({
      
      tx_status <- 
        if (input$radio_status == "Either") {c("Active","Inactive","Inferred Inactive")
        } else if (input$radio_status == "Inactive") { c("Inactive","Inferred Inactive")
        } else input$radio_status

      outcome_df <- outcome_df()
      
      if (input$agency == "All") {
        outcome_df %<>% 
          filter(as.Date(max_date) >= input$dateRange[1]
                 & as.Date(max_date) <= input$dateRange[2]
                 & client_status %in% tx_status)
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        outcome_df %<>% 
          filter(as.Date(max_date) >= input$dateRange[1]
                 & as.Date(max_date) <= input$dateRange[2]
                 & client_status %in% tx_status
                 & cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Aggregate data
      outcome_df %>%
        filter(is.na(score_total_diff) == F) %>% # rm init assessments & episodes w/ 1 rating
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
    
    outcome_avg <- reactive({
      
      tx_status <- 
        if (input$radio_status == "Either") {c("Active","Inactive","Inferred Inactive")
        } else if (input$radio_status == "Inactive") { c("Inactive","Inferred Inactive")
        } else input$radio_status
      
      if (input$agency == "All") {
        outcome_df %<>% 
          filter(as.Date(assess_date) >= input$dateRange[1]
                 & as.Date(assess_date) <= input$dateRange[2]
                 & client_status %in% tx_status)
      } else if ( input$agency %in% levels(unique(scrub_fas$cmh)) ) {
        outcome_df %<>% 
          filter(as.Date(assess_date) >= input$dateRange[1]
                 & as.Date(assess_date) <= input$dateRange[2]
                 & client_status %in% tx_status
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
    
    all_line <- reactive({
      
      cafasInput() %>%
        filter(fake_episode_id %in% sample(unique(fake_episode_id), 
                                           size = input$num_kids)) %>%
        select(fake_episode_id, episode_elapsed, score_total) %>%
        arrange(fake_episode_id, episode_elapsed) %>% 
        spread(fake_episode_id,score_total)
      
    })
    
    ## Build Reactive UI Elements
    
    output$select_episode <- renderUI({
      selectInput("select_episode",
                  label = "Select an episode grouper:",
                  choices = c("Common Episodes","System Default"), 
                  selected = "Common Episodes")
    })
    
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
    
      withProgress(message = 'Calculating outcomes',
                   detail = 'based on selected filters',
                   value = 0.1, 
                   {outcome_agg() %>%
                       suppressMessages() %>%
                       filter(measure_desc == input$select_measure) %>%
                       arrange(desc(percent)) %>%
                       plot_ly(x = cmh, y = percent, type = "bar",
                               color = cmh, colors = "Set3",
                               text = paste("Numerator: ", numerator,
                                            "<br>Denominator: ", denominator)) %>%
                       layout(xaxis = list(title = "CMHSP", showticklabels = F,
                                           categoryarray = cmh, categoryorder = "array"),
                              yaxis = list(title = "%", range = c(0, 100)),
                              legend = list(font = list(size = 10)))
      })
    })
    
    # output$avg_scale <- renderPlotly({
    #   
    #   outcome_avg <- outcome_avg()
    #   
    #   gg <-
    #     ggplot(data=outcome_avg, aes(x=cmh, y=avg, fill=interval)) + 
    #     geom_bar(stat="identity", position=position_dodge()) + 
    #     facet_grid(subscale ~ .) +
    #     labs(title = "Average Score by Subscale",
    #          x = "CMHSP",
    #          y = "Average Score") + 
    #     theme_minimal() +
    #     theme(legend.position = "top",
    #           legend.title = element_blank(),
    #           axis.text.x = element_text(angle=50),
    #           axis.title.x = element_blank(),
    #           strip.text.y = element_text(angle=0),
    #           strip.background = element_rect(colour="white", fill="#FFFFFF")) 
    #   
    #   ggplotly(gg)
    #   
    # })
    
    output$eligible_bar <- renderPlotly({
      
      df <-
      cafasInput() %>%
        filter(is.na(n_crit) == F
               & most_recent == T) %>%
        mutate(interval = recode(assess_type, 
                                 recodes = "'Exit CAFAS' = 'Discharge';
                                 'Initial CAFAS' = 'Intake';
                                 'Revised Initial' = 'Intake';
                                 else = 'In treatment'"),
               meet_crit = ifelse(n_crit == 0, 
                                  yes = "Ineligible", no = "Eligible")) %>%
        group_by(cmh, interval, meet_crit) %>%
        summarize(n = n()) %>%
        spread(meet_crit, n) %>%
        mutate(Total = Eligible + Ineligible,
               Pct_Elig = round(Eligible/Total*100, digits = 1),
               Pct_Inel = round(Ineligible/Total*100, digits = 1)) %>%
        ungroup() %>%
        droplevels()
      
      
      if (input$radio_elig_pct == "Number") {
        
        df %>%
          arrange(desc(Ineligible)) %>%
          plot_ly(x = cmh, y = Ineligible, type = "bar",
                  color = interval, colors = "Set3",
                  text = paste("% ineligible at interval: ", Pct_Inel,
                               "<br>Total assessed: ", Total)) %>%
          layout(title = "Youth not meeting eligibility criteria, by interval",
                 xaxis = list(title = "CMHSP", showticklabels = F,
                              categoryarray = cmh, categoryorder = "category ascending"),
                 yaxis = list(title = "# of youth assessed"), #, range = c(0, 100)
                 legend = list(font = list(size = 10)),
                 barmode = "stack")
        
      } else if (input$radio_elig_pct == "Percent")  {
        
        df %>%
          arrange(desc(Pct_Inel)) %>%
          plot_ly(x = cmh, y = Pct_Inel, type = "bar",
                  color = interval, colors = "Set3",
                  text = paste("# ineligible at interval: ", Ineligible,
                               "<br>Total assessed: ", Total)) %>%
          layout(title = "Youth not meeting eligibility criteria, by interval",
                 xaxis = list(title = "CMHSP", showticklabels = F,
                              categoryarray = cmh, categoryorder = "category ascending"),
                 yaxis = list(title = "% of youth assessed", range = c(0, 100)), 
                 legend = list(font = list(size = 10)))
        
      } else paste0("Uh-oh.  Input is neither number or percent.")
      
      
      
    })
    
    output$inel_days_hist <- renderPlotly({
      
      inel_hist <-
      cafasInput() %>% 
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
        filter(last_inel == T)  # just the last per episode
      
      max_hist <- max(hist(inel_hist$since_inel)$counts,na.rm=TRUE)
      
      inel_hist %>%
      plot_ly(x = since_inel,
                opacity = 0.6, 
                type = "histogram",
                #color = cmh, colors = "Set3",
                hoverinfo = "all", 
                name = "kids",
                showlegend = F,
                mode = "markers") %>%
        layout(title = "Days in service following ineligibility (Distribution)",
               xaxis = list(title = "Days in service following ineligibility", 
                            tickmode = "array"),
               yaxis = list(title = "# of youth", showgrid = F)) %>% 
        add_trace(x = rep(mean(since_inel), 
                          each = 2), 
                  y = c(0,max_hist),
                  type = "line",
                  line = list(dash = 5),
                  marker = list(color = "#DA824F"),
                  name = "Mean",
                  hoverinfo = "x",
                  xaxis = "x")
        
      
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
               at intake, or who have only received a single assessment.")
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
        dyLimit(limit = 120, label = "High Impairment", 
                labelLoc = "right") %>%
        dyLimit(limit = 80, label = "Medium Impairment",
                labelLoc = "right") %>%
        dyRangeSelector(dateWindow = c("0", "1460"),height = 20, strokeColor = "")
      
    })
    
    output$heatmap <- renderD3heatmap({
      
      withProgress(message = 'Creating heatmap...',
                   detail = 'Clustering can take awhile...',
                   value = 0.1, 
                   {cafasInput() %>%
                       filter(assess_ord == 1) %>%
                       select(score_total, subscale_school:subscale_thinking) %>%
                       rename(TOTAL = score_total, 
                              school = subscale_school, 
                              home = subscale_home, 
                              community = subscale_community,
                              behavior = subscale_behavior, 
                              mood = subscale_mood, 
                              selfharm = subscale_selfharm, 
                              substance = subscale_substance,
                              thinking = subscale_thinking) %>%
                       filter(complete.cases(.)) %>%
                       scale() %>%
                       d3heatmap(colors = "Blues",
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
