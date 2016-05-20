## explore CAFAS ui.R ##

dashboardPage(skin = "yellow",
  dashboardHeader(
    title = "explore CAFAS"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Outcomes", 
        tabName = "outcomes", 
        icon = icon("sun-o")
      ),
      menuItem(
        "Eligibility", 
        tabName = "eligible", 
        icon = icon("sign-in")
      ),
      menuItem(
        "Miscellany", 
        tabName = "misc", 
        icon = icon("paper-plane")
      ),
      selectInput(
        "agency",
        label = "Pick an agency:",
        choices = c("All", levels(unique(scrub_cafas$cmh))), 
        selected = "All"
      ),
      radioButtons(
        "radio_status",
        label = "Treatment Status:",
        choices = c("Active", "Inactive", "Either"), 
        selected = "Either",
        inline = T
      ),
      dateRangeInput(
        'dateRange',
        label = 'Date range:',
        start = min(as.Date(scrub_cafas$assess_date)[as.Date(scrub_cafas$assess_date) <= Sys.Date()]), 
        end = max(as.Date(scrub_cafas$assess_date)[as.Date(scrub_cafas$assess_date) <= Sys.Date()])
      ),
      textOutput("valid_date"),
      br(),
      em(
        paste0("   Data updated ",max(as.Date(scrub_cafas$assess_date)[as.Date(scrub_cafas$assess_date) <= Sys.Date()]))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "outcomes",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Outcome Measures", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              uiOutput("select_measure"),
              tabBox(
                width = 6,
                tabPanel(
                  "Compare",
                  plotlyOutput("outcome_bar")
                ),
                tabPanel(
                  "About", 
                  br(),
                  strong("...the measure you've selected..."),
                  br(),
                  h5(textOutput("define"))
                )
              ),
              tabBox(
                width = 6,
                tabPanel(
                  "Trend",
                  "etc."
                  #plotlyOutput("outcome_line")
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "eligible",
        fluidRow(
          column(
            width = 12,
            "what"
          )
        )
      ),
      tabItem(
        tabName = "misc",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Journeys", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Chart",
                  p(
                    "On the chart below you can see the paths taken by children
                    across",uiOutput("num_kids"),"treatment episodes.  Note that 
                    any time you change a filter, new episodes will be selected 
                    at random and displayed."
                  ),
                  dygraphOutput("all_linechart")
                )
              )
            )
          )
        )
      )
    )
  )
)
