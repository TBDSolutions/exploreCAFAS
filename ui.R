## explore CAFAS ui.R ##

dashboardPage(skin = "yellow",
  dashboardHeader(
    title = "explore CAFAS"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Eligibility", 
        tabName = "eligible", 
        icon = icon("medkit")
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
      dateRangeInput(
        'dateRange',
        label = 'Date range:',
        start = begin, 
        end = max(as.Date(scrub_cafas$assess_date)[as.Date(scrub_cafas$assess_date) <= Sys.Date()])
      ),
      textOutput("valid_date"),
      br(),
      em(
        paste0("   Data updated ",max(as.Date(scrub_sis$sis_date)[as.Date(scrub_sis$sis_date) <= Sys.Date()]))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "eligible",
        fluidRow(
          column(
            width = 6,
            "what"
          )
        )
      ),
      tabItem(
        tabName = "misc",
        fluidRow(
          column(
            width = 6,
            "what"
          )
        )
      )
    )
  )
)
