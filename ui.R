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
            box(
              title = "Eligibility", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = 6,
                tabPanel(
                  "Compare",
                  radioButtons(
                    "radio_elig_pct",
                    label = "Show as:",
                    choices = c("Number", "Percent"), 
                    selected = "Percent",
                    inline = T
                  ),
                  plotlyOutput("eligible_bar")
                ),
                tabPanel(
                  "How long?",
                  plotlyOutput("inel_days_hist")
                ),
                tabPanel(
                  "About",
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "Criteria",
                      h4("Functional Impairment Criteria for SED"),
                      p(
                        "The Michigan Department of Health and Human Services 
                        (MDHHS) defines criteria for determining when a child 7 
                        through 17 years is considered to have a serious emotional 
                        disturbance (SED), which supports eligibility for specialty 
                        supports and services.  In addition to a supporting 
                        diagnosis, eligibility is based on", 
                        em("functional impairment that substantially interferes 
                           with or limits the minor’s role or results in impaired 
                           functioning in family, school, or community 
                           activities."), 
                        "See: ",
                        a(href = "http://www.michigan.gov/documents/mdch/FY09-10_CMHSP_Contract_Boilerplatewithattachments_312216_7.pdf",
                          "MDHHS/CMHSP Mental Health Supports and Services Contract: Attachment C4.7.4")
                      ),
                      p(
                        "For the SED population, this functional impairment is 
                        determined by:",
                        br(),
                        "a. A total score of 50 (using the eight subscale scores on 
                        the Child and Adolescent Functional Assessment Scale 
                        (CAFAS), or", 
                        br(),
                        "b. Two 20s on any of the first eight subscales of the 
                        CAFAS, or", 
                        br(),
                        "c. One 30 on any subscale of the CAFAS, except for 
                        substance abuse only."
                      )
                    ),
                    tabPanel(
                      "Charts",
                      h4("Compare"),
                      p(
                        "The chart in the ", em("Compare"), "panel shows the number 
                        of youth whose most recent assessment did ", strong("not"), 
                        " meet SED criteria, broken down by the interval of 
                        assessment (i.e. intake, discharge).  The ", 
                        em("In treatment"), " interval includes all assessments 
                        which were not intake or discharge scores."
                      ),
                      h4("How long?"),
                      p(
                        "After a youth is assessed to not meet functional 
                        impairment, how long does s/he continue to receive 
                        services?  This chart looks at the number of days that 
                        elapse between the time that a youth is initially found 
                        to be ineligible for service and the time that s/he 
                        receives the final assessment within that episode." 
                      ),
                      p(
                        "The graph shown here is called a histogram. A histogram 
                        groups numeric data into bins, displaying the bins as 
                        columns. They are used to show the distribution of a 
                        dataset, i.e. how often values fall into ranges."
                      ),
                      p(
                        "Receipt of additional services is assumed here based on 
                        the existence of ongoing CAFAS assessments completed for 
                        the individual within the same treatment episode.  It is 
                        worth noting, however, that the services received may 
                        not have been Medicaid services."
                      )
                    ),
                    tabPanel(
                      "Notes",
                      p(
                        "Assessment findings may require different interpretations 
                        when they occur at differnt intervals.  For instance, if 
                        assessments indicate ineligibility at discharge, this 
                        could be an indication of successful treatment outcome."
                      ),
                      p(
                        "Individuals whose assessments do not meet SED criteria 
                        at the 3 and 6-month intervals may be appropriate 
                        candidates for utilization review to determine if they 
                        continue to require the level of service they are 
                        currently receiving."
                      )
                    )
                  )
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
        tabName = "misc",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Paths", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = TRUE,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Chart",
                  p(
                    "On the chart below you can see the paths taken by children
                    across multiple treatment episodes.  Note that 
                    any time you change a filter, new episodes will be selected 
                    at random and displayed. You can change the number of 
                    episodes that are displayed here:",uiOutput("num_kids")
                  ),
                  dygraphOutput("all_linechart")
                )
              )
            ),
            box(
              title = "Patterns of Need", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = TRUE,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Defining Patterns",
                  p(
                    "If we look at the patterns of need that kids have in 
                    various domains (measured by the CAFAS subscales), it may 
                    be possible to distinguish patterns that may call for 
                    different types of treatment."
                  ),
                  h4("How many groups of people? (Rows)"),
                  p(
                    "Depending on your particular situation, you may want to 
                    focus on greater or fewer clusters of kids' intake scores. Each 
                    intake score is depicted as a row in the heatmap.  You can 
                    select the number of clusters below.  This will 
                    color the clusters of intake scores whose patterns of need 
                    are most distinct, based on the CAFAS subscales:"
                  ),
                  numericInput(
                    inputId = "need_rows",
                    label = NULL, 
                    value = 5,
                    min = 2, 
                    max = 10,
                    width = '100px'
                  ),
                  p(
                    "Then, click on the ", em("Heatmap"), " tab to explore 
                    the groups in your population."
                  )
                ),
                tabPanel(
                  "Heatmap",
                  p(
                    "The heatmap below shows scores at the start of each 
                    treatment episode for each child.  Since the values are 
                    scaled, you can't connect them back to a specific score 
                    on a CAFAS subscale.  Instead, this helps us to look at 
                    broader patterns across life domains among youth at intake. 
                    Here, a darker blue means a higher score."
                  ),
                  d3heatmapOutput("heatmap")
                )
              )
            )
          )
        )
      )
    )
  )
)
