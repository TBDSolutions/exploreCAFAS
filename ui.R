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
        "Population Needs", 
        tabName = "pop_need", 
        icon = icon("life-ring")
      ),
      menuItem(
        "Eligibility", 
        tabName = "eligible", 
        icon = icon("sign-in")
      ),
      menuItem(
        "Paths", 
        tabName = "paths", 
        icon = icon("paper-plane")
      ),
      menuItem(
        "Patterns", 
        tabName = "patterns", 
        icon = icon("random")
      ),
      menuItem(
        "Filters",
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
          uiOutput("select_prog")
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
        ),
        menuSubItem(
          icon = NULL,
          uiOutput("select_version")
        ),
        menuSubItem(
          icon = NULL,
          uiOutput("select_episode")
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
                width = 12,
                tabPanel(
                  "Compare",
                  box(
                    title = "Select a measure...", 
                    color = "black",
                    collapsible = TRUE, 
                    collapsed = TRUE,
                    width = NULL,
                    uiOutput("select_measure")
                  ),
                  plotlyOutput("outcome_bar"),
                  br(),
                  box(
                    title = "About", 
                    color = "black",
                    collapsible = TRUE,
                    collapsed = T,
                    width = NULL,
                    strong("...the measure you've selected..."),
                    br(),
                    h5(textOutput("define"))
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "pop_need",
        fluidRow(
          column(
            width = 12,
            box(
              title = "CAFAS Scores", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Level of Care",
                  plotlyOutput("hist_fas"),
                  br(),
                  box(
                    title = "Chart settings", 
                    color = "black",
                    collapsible = TRUE,
                    collapsed = F,
                    width = NULL,
                    radioButtons(
                      "split",
                      label = "Display:",
                      choices = c("Stacked", "Facetted"), 
                      selected = "Stacked",
                      inline = T
                    ),
                    radioButtons(
                      "central",
                      label = "Summarize:",
                      choices = c("Mean", "Median"), 
                      selected = "Mean",
                      inline = T
                    ),
                    sliderInput(
                      "sni_bins", 
                      "Number of bins:", 
                      min = 1, 
                      max = 30, 
                      value = 24
                    ),
                    uiOutput("assess_num"),
                    em("Use this slider if you want to see the distribution of 
                       scores at the beginning, middle, or end of treatment.")
                  ),
                  box(
                    title = "Considerations", 
                    color = "black",
                    collapsible = TRUE,
                    collapsed = T,
                    width = NULL,
                    p(
                      "The following considerations may help to inform your 
                      interpretation of the data shown here:",
                      tags$ul(
                        tags$li(strong("Higher LOC prior to completion: "),
                                "In the case that a youth is nearing the 
                                completion of services in a higher level of care, 
                                it may in fact be advisable for continuation of 
                                the existing service even for clients who are 
                                below a given threshold in order to ensure 
                                continuity of the established treatment 
                                relationship."),
                        tags$li(strong("Assessment immediately prior to LOC: "),
                                "Please recall that the assessment score shown 
                                on the x-axis occurs prior to the level-of-care 
                                described.  The LOC grouping might therefore not 
                                have been provided prior to the score.  Thus, no 
                                conclusions regarding the impact of LOC on 
                                assessment scores can be drawn from this view of 
                                the data.")
                      )
                    )
                  )
                ),
                tabPanel(
                  "By CMH",
                  plotlyOutput("hist_cmh"),
                  box(
                    title = "Chart settings", 
                    color = "black",
                    collapsible = TRUE,
                    collapsed = F,
                    width = NULL,
                    uiOutput("assess_num_again"),
                    em("Use this slider if you want to see the distribution of 
                       scores at the beginning, middle, or end of treatment.")
                  )
                ),
                tabPanel(
                  "Boxplots",
                  plotlyOutput("box_fas")
                ),
                tabPanel(
                  "Relating Services to Needs",
                  fluidRow(
                    column(
                      width = 4, 
                      box(
                        title = "Settings", 
                        color = "black",
                        collapsible = TRUE,
                        collapsed = F,
                        width = NULL,
                        selectInput(
                          "predictor",
                          label = "Predictor variable (x-axis):",
                          choices = c("Length of episode",
                                      "Total hours of service per month",
                                      "Hours of home-based services per month",
                                      "Hours of case management per month",
                                      "Hours of wraparound services per month"),
                          selected = "Length of episode"
                        ),
                        em("Since service data are only available since 10/1/2014,
                           selecting service use variables will result in 
                           assessments prior to that date being filtered out."),
                        br(),
                        selectInput(
                          "response",
                          label = "Response variable (Y-axis):",
                          choices = c("Initial CAFAS Score in Episode",
                                      "Average CAFAS Score during Episode", 
                                      "Highest CAFAS Score during Episode", 
                                      "Change in CAFAS Score during Episode"),
                          selected = "Initial CAFAS Score in Episode"
                        ),
                        checkboxInput(
                          "remove_out", 
                          "Remove outliers? ", 
                          value = TRUE, 
                          width = NULL
                        )
                      ),
                      box(
                        title = "Definitions", 
                        color = "black",
                        collapsible = TRUE, 
                        collapsed = TRUE,
                        width = NULL,
                        p(
                          uiOutput("def_model")
                        ),
                        p(
                          strong("Slope: "),
                          uiOutput("def_slope")
                        ),
                        p(
                          strong("Intercept: "),
                          uiOutput("def_intercept")
                        ),
                        p(
                          strong("Good fit?: "),
                          uiOutput("def_r2")
                        ),
                        p(
                          strong("Significance: "),
                          uiOutput("def_pval")
                        )
                      )
                    ),
                    column(
                      width = 8, 
                      box(
                        title = "Table", 
                        color = "black",
                        collapsible = TRUE,
                        collapsed = F,
                        width = NULL,
                        dataTableOutput("regress_dt")
                      ),
                      box(
                        title = "Plot", 
                        color = "black",
                        collapsible = TRUE,
                        collapsed = T,
                        width = NULL,
                        plotlyOutput("regress"),
                        br(),
                        checkboxInput(
                          "ribbons", 
                          "Include uncertainty ribbons", 
                          value = FALSE, 
                          width = NULL
                        ),
                        p(
                          em("The thickness of an uncertainty ribbon gives an 
                             indication of the relative uncertainty at that point 
                             on the line.  If uncertainty lines overlap, it is 
                             uncertain whether the actual values are distinct.")
                        ),
                        box(
                          title = "Considerations", 
                          color = "black",
                          collapsible = TRUE,
                          collapsed = T,
                          width = NULL,
                          p(
                            "The following considerations may help to inform your 
                            interpretation of the data shown here:",
                            tags$ul(
                              tags$li(strong("High scores with low or zero LOS: "),
                                      "If a youth's initial score is high and length 
                                      of episode is either very low or zero, it is 
                                      possible that the youth were admitted to the 
                                      hospital in certain cases.  If the 
                                      hospitalization resulted in the end of agency
                                      services or in the start of a new episode upon 
                                      return, the LOS metric might not be a suitable 
                                      proxy for resource use during a given episode.  
                                      In such instances, the number or cost of 
                                      services may be a better indicator."),
                              tags$li(strong("Outliers highlighted: "),
                                      "Please note that outliers are highlighted in 
                                      the plot when the ",em("Remove outliers?"),
                                      " checkbox is not selected.  Clicking on the 
                                      legend to hide the outliers in the plot does 
                                      not, however, remove them from the linear 
                                      model which produces the line.")
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                tabPanel(
                  "About", 
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "CAFAS Distribution",
                      p(
                        "The CAFAS/PECFAS score is a global summary of needs 
                        across each of the domains included in the tool: ",
                        em(
                          "At School, At Home, in the Community (delinquency), 
                          Behavior Toward Others, Moods/emotions, Self Harm, 
                          Substance Use, and Thinking (irrationality).  "
                        ), 
                        "The developers of the CAFAS recommend a total cutoff 
                        score of 80 as an indicator of functional impairment, 
                        warranting classification of a child as having SED (",
                        a(href="https://deepblue.lib.umich.edu/handle/2027.42/44651", 
                          "Hodges and Wong, 1996"),")."
                      ),
                      p(
                        "The graph shown in the 'Distribution' tab is called a 
                        histogram. A histogram groups numeric data into bins, 
                        displaying the bins as columns. They are used to show the 
                        distribution of a dataset, i.e. how often values fall 
                        into ranges."
                      ),
                      p(
                        "This histogram shows the distribution of CAFAS/PECFAS 
                        scores, grouped by the level of care which the child 
                        received following the assessment on which the score 
                        was given (",em("and going up until the next assessment"),")."
                      ),
                      p(
                        "If the level of care were related to the CAFAS score, 
                        one would expect to see more intensive levels of care (",
                        em("such as Home-Based"), 
                        ") closer to the right side of the distribution, and 
                        less intensive levels of care (",
                        em("such as Outpatient"),") closer to the left side of 
                        the distribution."
                      ),
                      p(
                        "For more information on how levels of care are defined 
                        for this analysis, see the ", em("About"), 
                        " section located in the left-hand toolbar and look for the ", 
                        em("Level of Care Groupings")," definition."
                      )
                    ),
                    tabPanel(
                      "Boxplots",
                      p(
                        "The boxplots here show a summary of scores for all CMHSPs.  
                        A boxplot shows key information about the 
                        distribution of a measure, i.e. how it is spread out.  It is 
                        made up of the following pieces:",
                        br(),
                        strong("Median: "), 
                        "The mid-point of the data is shown by the line that divides 
                        the box into two parts. Half the scores are greater than or 
                        equal to this value, half are less.",
                        br(),
                        strong("Interquartile range: "), 
                        "The middle 'box' represents the middle 50% of scores for 
                        the group. The range of scores from lower to upper quartile 
                        is referred to as the inter-quartile range.",
                        br(),
                        strong("Upper quartile: "), 
                        "75% of the scores fall below the upper quartile. This is 
                        the top of the box (or the right side if the boxplot is 
                        displayed horizontally",
                        br(),
                        strong("Lower quartile: "), 
                        "25% of scores fall below the lower quartile. This is the 
                        bottom (left side) of the box.",
                        br(),
                        strong("Whiskers: "), 
                        "The whiskers stretch to the greatest (top) and least 
                        (bottom) values in the data, except for outliers.",
                        br(),
                        strong("Outliers: "), 
                        "Outliers are defined as more than 1.5x the upper value or 
                        less than 1.5x the lower value shown by the whiskers.",
                        br(),
                        "For more information, here's a ",
                        a(href = "http://flowingdata.com/2008/02/15/how-to-read-and-use-a-box-and-whisker-plot/",
                          "diagram from FlowingData"),
                        "showing the parts of a boxplot."
                        ),
                      br(),
                      h4("Interpreting the boxplot..."),
                      p(
                        "Box plots that are comparatively short show that assessed 
                        childrens' scores fall within a restricted range. 
                        Comparatively tall box plots show a broader range of scores. 
                        If one box plot is much higher or lower than all the others, 
                        this may suggest either a difference between the populations 
                        being assessed or some variation in the practice of 
                        assessors scoring individuals."
                      )
                    )
                  )
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
                width = 12,
                tabPanel(
                  "Compare",
                  plotlyOutput("eligible_bar"),
                  br(),
                  box(
                    title = "Chart Options", 
                    color = "black",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = NULL,
                    radioButtons(
                      "radio_elig_pct",
                      label = "Show as:",
                      choices = c("Number", "Percent"), 
                      selected = "Percent",
                      inline = T
                    ),
                    p(
                      "There may be times when you only want to view the most 
                      recent assessments that have occurred for a child, or 
                      vice-versa.  For instance, if you are interested in seeing 
                      kids who didn't meet eligibility criteria at intake ",
                      em("only if"), " the intake was followed by another 
                      assessment, then you can select ", 
                      em("All but most recent"), "to focus on those instances."
                    ),
                    radioButtons(
                      "radio_elig_recent",
                      label = "Include only most recent assessment?:",
                      choices = c("Only most recent", 
                                  "All but most recent", 
                                  "Either"), 
                      selected = "Either",
                      inline = T
                    ),
                    p(
                      "Keep in mind that you can also filter for ", em("Active"),
                      " or ", em("Inactive"), " cases in the sidebar panel."
                    )
                  )
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
                        and percentage of youth whose most recent assessment did ", 
                        strong("not"), 
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
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "paths",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Paths", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = F,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Chart",
                  p(
                    "On the chart below you can see the paths taken by multiple children
                    during the course of their respective treatment episodes.  Note that 
                    any time you change a filter, new episodes will be selected 
                    at random and displayed. You can change the number of 
                    episodes that are displayed here:",uiOutput("num_kids")
                  ),
                  dygraphOutput("all_linechart")
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "patterns",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Patterns of Need", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = F,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Defining Patterns",
                  p(
                    "If we look at the patterns of need that kids have in 
                    various domains (measured by the PECFAS/CAFAS subscales), it may 
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
                    are most distinct, based on the PECFAS/CAFAS subscales:"
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
              title = "About the Assessment", 
              status = "warning",
              collapsible = T, 
              collapsed = F,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Contents",
                  h4("Domains"),
                  p(
                    "The Child and Adolescent Functional Assessment Scale 
                    (CAFAS) is multidimensional, consisting of multiple scales 
                    designed to identify impairment across eight different 
                    areas of child functioning:",
                    tags$ul(
                      tags$li(strong("School/Work: "),
                              "functions in a group educational 
                              environment"),
                      tags$li(strong("Home: "),
                              "observes reasonable rules and performs 
                              age-appropriate tasks"),
                      tags$li(strong("Community: "),
                              "respects the rights and property of others, 
                              acts lawfully"),
                      tags$li(strong("Behavior Toward Self and Others: "),
                              "appropriateness of daily behavior"),
                      tags$li(strong("Moods/Emotions: "),
                              "modulation of emotional life"),
                      tags$li(strong("Self-Harmful Behavior: "),
                              "extent to which the youth can cope without 
                              resorting to self-harmful behavior or 
                              verbalizations"),
                      tags$li(strong("Substance Use: "),
                              "substance use and the extent to which it is 
                              inappropriate and disruptive"),
                      tags$li(strong("Thinking: "),
                              "ability to use rational thought processes")
                    )
                  ),
                  br(),
                  h4("Scoring"),
                  p(
                    "The CAFAS is basically a menu of behavioral items, from 
                    which the rater selects those that describe the youth's 
                    most severe functioning in a specified time period.  
                    For each scale, the items are grouped by four levels of 
                    severity, with higher scores indicating more pronounced 
                    impairment:",
                    tags$ul(
                      tags$li(strong("Severe (30 pts.)"),
                              em(" severe disruption or incapacitation")),
                      tags$li(strong("Moderate (20 pts.)"),
                              em(" persistent disruption or major occasional 
                                 disruption of functioning")),
                      tags$li(strong("Mild (10 pts.)"),
                              em(" significant problems or distress")),
                      tags$li(strong("Minimal or no impairment (0 pts.)"),
                              em(" no disruption of functioning"))
                    )
                  )
                ),
                tabPanel(
                  "Interpretation",
                  h4("Overall Level of Dysfunction"),
                  p(
                    "While there are no official cutoff scores for the CAFAS, 
                    a general framework referred to as Overall Level of 
                    Dysfunction, has been proposed as a useful method for 
                    explaining results for laypersons. The levels proposed 
                    are as follows: ",
                    tags$ul(
                      tags$li(strong("Very minimal "),"(0 to 30)"),
                      tags$li(strong("Mild impairment "),"(40 to 70)"),
                      tags$li(strong("Moderate impairment "),"(80 to 100)"),
                      tags$li(strong("Severe impairment "),"(110 to 130)"),
                      tags$li(strong("Very severe impairment "),"(140 or higher)")
                    ),
                    em("Cf. Hodges K. CAFAS: Manual for Training Coordinators, 
                       Clinical Administrators, and Data Managers. Third ed. 
                       Kay Hodges; Ann Arbor, MI: 2005. ")
                  ),
                  br(),
                  h4("Related Services"),
                  p(
                    strong("Outpatient"), 
                    "A threshold of greater than 30 is often taken to indicate 
                    that children should be receiving outpatient or more intense 
                    mental health or behavioral services."
                  ),
                  p(
                    strong("Waiver for Children with Serious Emotional Disturbance (SEDW)"),
                    "The Michigan Department of Health and Human Services 
                    (MDHHS) SEDW waiver uses the CAFAS scoring as in indicator 
                    of functional impairment to determine eligibility.  The 
                    following criteria come from", 
                    tags$a(href = "http://www.michigan.gov/mdhhs/0,5885,7-339-71550_2941_4868_7145-168285--,00.html",
                           "the state's website"),
                    ", which states that ",
                    em("'as appropriate for age, functional limitation will be 
                       identified using the Child and Adolescent Functional 
                       Assessment Scale (CAFAS®) or the Preschool and Early 
                       Childhood Functional Assessment Scale (PECFAS®)'"),
                    "using the following thresholds:",
                    tags$ul(
                      tags$li("CAFAS® score of 90 or greater for children age 7 to 12; OR"),
                      tags$li("CAFAS® score of 120 for greater for children age 13 to 18; OR"),
                      tags$li("For children age 3 to 7: elevated PECFAS® subscale 
                              scores in at least one of these areas: self-harmful 
                              behaviors, mood/emotions, thinking/communicating or 
                              behavior towards others")
                    )
                  )
                ),
                tabPanel(
                  "Strengths",
                  p(
                    "The CAFAS tool has demonstrated the following strengths 
                    across multiple studies:",
                    tags$ul(
                      tags$li(strong("Consistent, valid and reliable. "),
                              "Data from two large-scale evaluation studies 
                              indicate that the CAFAS demonstrates good internal 
                              consistency (ranging from .63 to .78) and high 
                              inter-rater reliability (above .92 for the total 
                              CAFAS score, above .83 for the individual scales; 
                              Hodges & Wong, 1996). It has also been found to 
                              perform similarly across males and females with 
                              respect to measuring overall functioning (e.g. 
                              Ezpeleta et al., 2006; Goldston et al., 2007). 
                              The content, concurrent, and predictive validity 
                              of the CAFAS suggest that it correlates 
                              significantly and positively with other indicators 
                              of impairment, including severity of psychiatric 
                              diagnosis and subsequent service  utilization 
                              (Hodges & Wong, 1996)."),
                      tags$li(strong("Predicts service use. "),
                              "CAFAS total score at intake has been found to 
                              predict subsequent service use at both 6 and 12 
                              months post-intake (Hodges & Wong, 1997). Scores 
                              on the CAFAS also distinguish between children 
                              who do and do not receive services in education/
                              mental health programs (Rosenblatt & Rosenblatt, 
                              1999). "),
                      tags$li(strong("Identifies psychopathology. "),
                              "Total CAFAS scores differentiate between the 
                              presence and absence of psychopathology among 
                              children and adolescents (Ezpeleta et al. 2006)")
                    )
                  )
                ),
                tabPanel(
                  "Bibliography",
                  tags$ul(
                    tags$li(
                      tags$a(
                        href="https://deepblue.lib.umich.edu/bitstream/handle/2027.42/45765/11414_2005_Article_BF02287471.pdf?sequence=1", 
                        "Hodges, K., Wong, M. M., & Latessa, M. (1998). Use of 
                        the Child and Adolescent Functional Assessment Scale 
                        (CAFAS) as an outcome measure in clinical settings. 
                        The journal of behavioral health services & research, 
                        25(3), 325-336."
                      )
                    ),
                    tags$li(
                      tags$a(
                        href="https://www.ncbi.nlm.nih.gov/pubmed/9230570", 
                        "Hodges, K., & Wong, M. M. (1997). Use of the Child and 
                        Adolescent Functional Assessment Scale to predict 
                        service utilization and cost. The Journal of Mental 
                        Health Administration, 24(3), 278-290."
                      )
                    ),
                    tags$li(
                      tags$a(
                        href="http://www.childfirst.ucla.edu/Francisetal(2012).pdf", 
                        "Francis, S. E., Ebesutani, C., & Chorpita, B. F. 
                        (2012). Differences in levels of functional impairment 
                        and rates of serious emotional disturbance between youth 
                        with internalizing and externalizing disorders when 
                        using the CAFAS or GAF to assess functional impairment. 
                        Journal of Emotional and Behavioral Disorders, 20(4), 
                        226-240."
                      )
                    ),
                    tags$li(
                      tags$a(
                        href="https://deepblue.lib.umich.edu/bitstream/handle/2027.42/45770/11414_2005_Article_BF02287803.pdf?sequence=1&isAllowed=y", 
                        "Hodges, K., Doucette-Gates, A., & Kim, C. S. (2000). 
                        Predicting service utilization with the child and 
                        adolescent functional assessment scale in a sample of 
                        youths with serious emotional disturbance served by 
                        center for mental health services-funded demonstrations. 
                        The Journal of Behavioral Health Services and Research, 
                        27(1), 47-59."
                      )
                    ),
                    tags$li(
                      tags$a(
                        href="http://psycnet.apa.org/psycinfo/1999-08239-008", 
                        "Hodges, K., Doucette-Gates, A., & Liao, Q. (1999). The 
                        relationship between the Child and Adolescent Functional 
                        Assessment Scale (CAFAS) and indicators of functioning. 
                        Journal of Child and Family Studies, 8(1), 109-122."
                      )
                    ),
                    tags$li(
                      tags$a(
                        href="https://www.ncbi.nlm.nih.gov/pubmed/10932440", 
                        "Hodges, K., & Wotring, J. (2000). Client typology 
                        based on functioning across domains using the CAFAS: 
                        Implications for service planning. The journal of 
                        behavioral health services & research, 27(3), 257-270."
                      )
                    ),
                    tags$li(
                      tags$a(
                        href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3878718/", 
                        "Kurowski, B. G., Wade, S. L., Kirkwood, M. W., Brown, 
                        T. M., Stancin, T., & Taylor, H. G. (2013). Behavioral 
                        predictors of outpatient mental health service 
                        utilization within 6 months after traumatic brain injury 
                        in adolescents. PM&R, 5(12), 1026-1034."
                      )
                    )
                  )
                )
              )
            ),
            box(
              title = "Episode Groupings", 
              status = "warning",
              collapsible = T, 
              collapsed = T,
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
                ),
                tabPanel(
                  "Suggestions",
                  p(
                    "You can review the technical details of the episode 
                    groupings (",em("and anything else that's part of this app!"),
                    ") in the source code, available",
                    a(href="https://github.com/j-hagedorn/exploreCAFAS/blob/master/prep/episode_grouper_FAS.R", 
                      "here on GitHub"),"."
                  ),
                  p(
                    "If you have suggestions on how to improve this process, 
                    please feel free to submit a pull request to the ",
                    a(href = "https://github.com/j-hagedorn/exploreCAFAS",
                      "exploreCAFAS repository"),
                    "or (if you don't speak code) describe the proposed changes 
                    in detail by ",
                    a(href = "https://github.com/j-hagedorn/exploreCAFAS/issues/new",
                      "filing an issue here")
                  )
                )
              )
            ),
            box(
              title = "Level of Care", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = T,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Methodology",
                  p(
                    "Each assessment is tagged with a given service type (a.k.a. 
                    level of care) based on the services that the individual 
                    received in the period subsequent to the assessment.  The 
                    services are derived from encounter data."
                  ),
                  p(
                    "Being placed in a given service category means that the 
                    individual received a service from a given group during the 
                    time period between the assignment of the CAFAS/PECFAS score 
                    to which the category is connected and the next CAFAS/PECFAS 
                    score for that specific client ID within a given episode."
                  )
                ),
                tabPanel(
                  "Groups",
                  p(
                    "Each assessment is tagged with one of the group labels 
                    below. Groups are listed from most intensive to least 
                    intensive. An assessment is tagged based on the most 
                    intensive group from which a service was received:",
                    tags$ul(
                      tags$li(strong("Home Based: "),
                              "Received service code(s) H0036 and/or H2033.  
                              May also include Wraparound (H2021) or 
                              other services"),
                      tags$li(strong("Case Management: "),
                              "Received service code(s) T1016 and/or T1017.  
                              May also include Wraparound (H2021) or 
                              other services"),
                      tags$li(strong("Outpatient: "),
                              "Received service code(s) 
                              from the 90xxx series.  May also include 
                              Wraparound (H2021) or other services"),
                      tags$li(strong("Wraparound: "),
                              "Received Wraparound services (H2021) but none of 
                              the other service codes associated with 
                              outpatient, case management, or home-based"),
                      tags$li(strong("Other Services: "),
                              "Received some services during the period, but 
                              none associated with the groups listed above"),
                      tags$li(strong("No Services: "),
                              "Received no services during the period")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
