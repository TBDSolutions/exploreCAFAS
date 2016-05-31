## explore CAFAS server.R ##

shinyServer(
  function(input, output) {
    
    # Make Reactive Datasets
    
    cafasInput <- reactive({  
      
      tx_status <- 
      if (input$radio_status == "Either") {c("Active","Inactive","Inferred Inactive")
      } else if (input$radio_status == "Inactive") { c("Inactive","Inferred Inactive")
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
    
    outcome_agg <- reactive ({
      
      tx_status <- 
        if (input$radio_status == "Either") {c("Active","Inactive","Inferred Inactive")
        } else if (input$radio_status == "Inactive") { c("Inactive","Inferred Inactive")
        } else input$radio_status
      
      if (input$agency == "All") {
        outcome_df %<>% 
          filter(as.Date(assess_date) >= input$dateRange[1]
                 & as.Date(assess_date) <= input$dateRange[2]
                 & client_status %in% tx_status)
      } else if ( input$agency %in% levels(unique(scrub_cafas$cmh)) ) {
        outcome_df %<>% 
          filter(as.Date(assess_date) >= input$dateRange[1]
                 & as.Date(assess_date) <= input$dateRange[2]
                 & client_status %in% tx_status
                 & cmh %in% input$agency)
      } else print(paste0("Error.  Unrecognized input."))
      
      # Aggregate data
      outcome_df %>%
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
        layout(xaxis = list(title = "CMHSP", showticklabels = F,
                            categoryarray = cmh, categoryorder = "array"),
               yaxis = list(title = "%", range = c(0, 100)),
               legend = list(font = list(size = 10)))
      
    })
    
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
               Pct_Inel = round(Ineligible/Total*100, digits = 1))
      
      
      if (input$radio_elig_pct == "Number") {
        
        df %>%
          arrange(desc(Ineligible)) %>%
          plot_ly(x = cmh, y = Ineligible, type = "bar",
                  color = interval, colors = "Set3",
                  text = paste("% ineligible at interval: ", Pct_Inel,
                               "<br>Total assessed: ", Total)) %>%
          layout(title = "Youth not meeting eligibility criteria, by interval",
                 xaxis = list(title = "CMHSP", showticklabels = F,
                              categoryarray = cmh, categoryorder = "array"),
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
                              categoryarray = cmh, categoryorder = "array"),
                 yaxis = list(title = "% of youth assessed", range = c(0, 100)), 
                 legend = list(font = list(size = 10)))
        
      } else paste0("Uh-oh.  Input is neither number or percent.")
      
      
      
    })
    
    output$inel_days_hist <- renderPlotly({
      
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
        filter(last_inel == T) %>% # just the last per episode
        plot_ly(x = since_inel,
                opacity = 0.6, 
                type = "histogram",
                #color = cmh, colors = "Set3",
                hoverinfo = "all", 
                name = "days",
                showlegend = F,
                mode = "markers") %>%
        layout(title = "Days in service following ineligibility (Distribution)",
               xaxis = list(title = "Days in service following ineligibility", 
                            tickmode = "array"),
               yaxis = list(title = "# of youth", showgrid = F)) # barmode = "stack"
      
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
                       select(cafas_total, subscale_school:subscale_thinking) %>%
                       rename(TOTAL = cafas_total, 
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
    
  }
)
