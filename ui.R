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
      uiOutput("select_episode"),
      uiOutput("select_version"),
      menuItem(
        "Other filters",
        icon = icon("filter"),
        menuSubItem(
          icon = NULL,
          selectInput(
            "agency",
            label = "Pick an agency:",
            choices = c("All", levels(unique(scrub_fas$cmh))), 
            selected = "All"
          )
        ),
        menuSubItem(
          icon = NULL,
          radioButtons(
            "radio_status",
            label = "Treatment Status:",
            choices = c("Active", "Inactive", "Either"), 
            selected = "Either",
            inline = T
          )
        ),
        menuSubItem(
          icon = NULL,
          dateRangeInput(
            'dateRange',
            label = 'Date range:',
            # Start date defaults to earliest date in dataset which is the max
            # date for a given client (i.e. id)
            start = min(as.Date(scrub_fas$max_date)[as.Date(scrub_fas$max_date) <= Sys.Date()]), 
            # End date defaults to date of most recent data in dataset
            # to avoid user confusion. Filtering on end date should still filter 
            # value passed against 'max_date' variable in data.
            end = max(as.Date(scrub_fas$assess_date)[as.Date(scrub_fas$assess_date) <= Sys.Date()])
          )
        )
      ),
      menuItem(
        "About", 
        tabName = "about", 
        icon = icon("info-circle")
      ),
      textOutput("valid_date"),
      br(),
      em(
        paste0("   Data updated ",max(as.Date(scrub_fas$assess_date)[as.Date(scrub_fas$assess_date) <= Sys.Date()]))
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
              tabBox(
                width = 6,
                tabPanel(
                  "Compare",
                  uiOutput("select_measure"),
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
                  "Breakdown",
                  plotlyOutput("avg_scale")
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
                      ),
                      p(
                        "Please note that, while these criteria have frequently 
                        been interpreted as applying at the point of access 
                        (i.e. as entrance criteria), they have not consistently 
                        been used as ongoing criteria for continued eligibility.  
                        Since CAFAS is intended to complement the goals of the 
                        plan of care, however, scores indicating a decrease in 
                        need should correspond to a change in treatment 
                        provision."
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
                  h4("How many clusters of kids?"),
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
      ),
      tabItem(
        tabName = "about",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Episode Groupings", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = F,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Rationale",
                  p(
                    "This application offers two options for grouping episodes: ",
                    em("System Default"), "and ", em("Common Episodes"),".  ",
                    "The groupings shown by the ",em("System Default"),
                    " option use the episode labels from the source dataset.  
                    The application also includes an option for ", 
                    em("Common Episodes"),", which implement a new, consistent 
                    grouping logic across all organizations and assessors."
                  ),
                  br(),
                  strong("Why use a new grouping logic?"),
                  p(
                    "The CAFAS® and PECFAS® datasets include an episode number,  
                    which is generated when an employee activates a case in the 
                    FAS® system.  New episodes may be generated in the system 
                    for multiple reasons, including life changes that cue a 
                    significant alteration in treatment plan.  While these may 
                    be valid reasons for thinking of treatment as having entered 
                    a new phase, the logic used is subjective and not applied 
                    consistently across all assessors."
                  ),
                  p(
                    "In addition, the default method uses an episode start date 
                    related to the creation of the episode, while the initial 
                    assessment may occur later for multiple reasons (", 
                    em("e.g. no-show, rescheduling, etc."),".  ", 
                    "It is therefore more accurate to use the initial assessment 
                    date as the start date for an episode, since the assessment 
                    serves as the first step of service provision and a baseline 
                    for the measurement of improvement."
                  ),
                  p(
                    "Another inconsistency occurs regarding episodes during 
                    which children transition, or 'age out' of the PECFAS® tool 
                    and workers begin to use the CAFAS® tool instead.  When this 
                    occurs, some workers complete an 'initial CAFAS®', rather 
                    than continuing the same episode begun with the PECFAS®, and 
                    thereby create a new episode identifier. Since this practice 
                    is inconsistent, it leads to episodes built on different 
                    definitions.  There is also inconsistent practice around 
                    combining episodes that occur within 90 days."
                  ),
                  p(
                    "In order to consistently define treatment episodes for 
                    measuring outcomes and other features of clinical treatment, 
                    it is important to use a consistent definition of a 
                    treatment episode. This is what the ", em("Common Episodes"), 
                    " option provides."
                  )
                ),
                tabPanel(
                  "Methodology",
                  strong("Spanning Tools"),
                  p(
                    "The new method allows for consistent episode groupings that 
                    span PECFAS® and CAFAS®.  For this reason, a combined 
                    dataset with assessment data from both tools is processed 
                    using the grouping logic.  Users can still select a single 
                    tool as well using the filters provided. ", 
                    em("Caveat: Comparisons between baseline and update score 
                       using mixed episodes are somewhat less likely to show 
                       improvement, since PECFAS® has a max score of 210, 
                       while CAFAS® has a max score of 240.")
                  ),
                  strong("Episode Scope"),
                  p(
                    "All assessments are grouped by the Client ID field and the 
                    name of the Organization.  Using this definition, an episode 
                    never bridges multiple organizations, but may bridge service 
                    areas or programs within a given organization.  Records 
                    where either of these grouping variables are filtered out of 
                    the dataset."
                  ),
                  p(
                    "While some might wish to create episodes that span 
                    transitions between multiple organizations, this is not 
                    possible since each organization determines its own logic 
                    for assigning Client IDs, which could lead to instances 
                    where the same ID would mistakenly be applied to different 
                    persons."
                  ),
                  p(
                    em("Note:  "), 
                    "Persons returning for services after previously receiving 
                    treatment are merged with an existing ID in the FAS Online 
                    system.  Unfortunately, there may be some instances where a 
                    returning individual would not be matched due to the lookup 
                    process in that system."
                  ),
                  strong("Ordering Assessments within an Episode"),
                  p(
                    "Within the groupings detailed above, assessments are 
                    arranged by the date on which they occurred to determine 
                    sequence.  The first assessment for any Client ID in a given 
                    organization is automatically marked as episode 1, then 
                    increments from there when conditions are met, as detailed 
                    below."
                  ),
                  strong("Boundaries of Episodes"),
                  p(
                    "The episode counter increments by one (", 
                    em("a.k.a. a new episode is created"),") when the previous 
                    assessment has been tagged as the 'last' in an episode.  
                    Records are tagged if ", 
                    em("BOTH"), " of the following conditions are met:",
                    tags$ul(
                      tags$li("the next assessment is", em("Initial"), " or ", 
                              em("Revised Initial"), 
                              ".  (Note: Revised Initial assessments are serve 
                              to trigger the beginning of episodes, since there 
                              are instances when the initial is incompletely 
                              filled out by access staff and are therefore 
                              excluded from this data, due to incompleteness.)"),
                      tags$li(">= 120 days elapse before next assessment (Note: 
                              By this point all 'initial' assessments which are 
                              followed by a 'revised initial' have been removed.")
                    ),
                    "unless the following occurs:",
                    tags$ul(
                      tags$li(em("Exception"),": A new episode is not triggered 
                              if the first assessment in the episode has been 
                              cued by assess_type of 'Initial PECFAS' and then 
                              'Initial CAFAS' occurs during the course of that 
                              episode.")
                    )
                  ),
                  strong("Apply Episode Dates"),
                  p(
                    "Within the episodes defined by the logic above, episode 
                    'start' and 'end' dates are defined, using the initial 
                    assessment date as the start and the most recent assessment 
                    date as the end date.  If the child's status is 'Active', 
                    the episode end date for the most recent episode is set to 
                    be blank."
                  ),
                  p(
                    em("Note: The end date of episode is not necessarily marked 
                       as an 'Exit CAFAS', since this label is inconsistently 
                       used in practice (e.g. in the case that someone 
                       discontinues AMA following the assessment). If an 
                       assessment is not marked as an 'Exit CAFAS' and the child 
                       is not closed, then the end date of the episode will 
                       remain empty.  The episode will continue to be marked as 
                       active and calculations of the duration of the episode 
                       will continue to be calculated between the start date and 
                       the most recent system update (rather than an episode end 
                       date) which will lead to longer episodes.")
                  )
                ),
                tabPanel(
                  "Comparison",
                  p(
                    "The table below compares basic summaries of the different 
                    episode grouping methods outlined here:"
                  ),
                  dataTableOutput("summary_episodes")
                )
              )
            )
          )
        )
      )
    )
  )
)
