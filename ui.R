#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => R-SHINY USER INTERFACE
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________
# # FYI: To run SOCRATES via Github (latest version!)
# library('shiny')
# runGitHub('socrates_rshiny','lwillem')

# # load all functions and packages
# this is done automatically when running the App

# Define UI for social contact application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(ui_title,
              windowTitle = paste0('SOCRATES (',version_id,')')),
  
  # Sidebar with controls
  sidebarPanel(
 
    if(bool_is_comix_ui){
      uiOutput("socrates_website_data")
    },
    
    if(bool_is_comix_ui){
      hr()
    },


    selectInput(inputId = "country", 
                label = "Country",
                choices = opt_country,
                selectize = ),
    
    # # waves (dynamic, only if wave info is present)
    uiOutput(outputId = 'dynamicWaveInput'),
    
    textInput(inputId="age_breaks_text",
              label="Age breaks (comma delimited)",
              value=opt_age_breaks),
   
    #by default 'all contacts' to prevent warnings/errors, can be extended in "server" script. 
    selectInput("daytype", "Type of day",
                opt_day_type[1]),
    
    conditionalPanel(condition = bool_selectInput_duration,
    selectInput("duration", "Contact duration",
                opt_duration)
    ),
    
    selectInput("touch", "Contact intensity",
                opt_touch),
    
    selectInput("gender", "Gender",
                opt_gender),
    
    tabsetPanel(type = "tabs", id="distancing_transmission",
                tabPanel("General",
                         checkboxInput("bool_reciprocal", "Reciprocity",value = TRUE),
                         checkboxInput("bool_weigh_age", "Weigh by age",value = TRUE),
                         checkboxInput("bool_weigh_week", "Weigh by week/weekend",value = TRUE),

                         # SPC (optional)
                         conditionalPanel(
                           condition = 'output.panelStatus',
                           checkboxInput("bool_spc", "Include supplemental professional contacts (SPC)",value = TRUE)),
                         
                         # HH members (optional)
                         conditionalPanel(
                           condition = 'output.panelStatusHome',
                           checkboxInput("bool_hhmember_selection", "Set contacts at 'Home' with non-household members as 'leisure contacts'",value = FALSE)),
                         
                         checkboxInput("bool_location", "Include all locations",value = TRUE),
                         conditionalPanel(
                           condition = "input.bool_location == false",
                           checkboxGroupInput('cnt_location',
                                              label = 'Included locations',
                                              choices = opt_location,
                                              selected = opt_location))
                        ),
                tabPanel("Options", 
                         checkboxInput("bool_age_range", "Age range: sample at random",value = TRUE),
                         checkboxInput("bool_age_missing", "Missing contact age: remove participant",value = FALSE),
                         checkboxInput("bool_matrix_limit", "Specify the color scale of the social contact matrix?",value = FALSE),
                         conditionalPanel(condition = "input.bool_matrix_limit == true",
                                          numericInput(inputId="ui_scale_max",
                                                       label = "Color scale upper limit (≥1)",
                                                       value = NA,
                                                       min   = 1))
                ),
                tabPanel("Distancing", checkboxInput("bool_physical_distancing","Include physical distancing"),
                                       conditionalPanel(
                                         condition = "input.bool_physical_distancing == true",
                                         sliderInput("cnt_reduction_home","Reduce 'home' contacts (%)",min=0,max=100,value=0),
                                         sliderInput("cnt_reduction_work","Reduce 'work' contacts (%)",min=0,max=100,value=0),
                                         sliderInput("cnt_reduction_school","Reduce 'school' contacts (%)",min=0,max=100,value=0),
                                         sliderInput("cnt_reduction_transport","Reduce 'transport' contacts (%)",min=0,max=100,value=0),
                                         sliderInput("cnt_reduction_leisure","Reduce 'lesiure' contacts (%)",min=0,max=100,value=0),
                                         sliderInput("cnt_reduction_otherplace","Reduce 'otherplace' contacts (%)",min=0,max=100,value=0))
                         ),
                tabPanel("Transmission",checkboxInput("bool_transmission_param", "Age-specific transmission",value = FALSE),
                                        conditionalPanel(
                                          condition = "input.bool_transmission_param == true",
                                          uiOutput("sliders_susceptibility"),
                                          uiOutput("sliders_infectiousness"))
                        )
                ),
    
    hr(),
    helpText('DOWNLOAD'),
    downloadButton('download_matrix',"Download matrix (.csv)", style = "width:99%;"),
    downloadButton('download_all',"Download results (.RData)",style = "width:99%;"),
    
    # add version and link to project website
    headerPanel(""),
    uiOutput("project_website"),
    if(!bool_is_comix_ui){
      uiOutput("socrates_website_comix")
    },
    helpText('SOCRATES',version_id)
    
  ),
  
  mainPanel(
    
    # allways show matrix with contact rates
    plotOutput('plot_cnt_matrix',width = "80%", height = "300px"),
    
    # use tabs
    tabsetPanel(type = "tabs",
                id='tabs_results',
                tabPanel("All results", 
                         verbatimTextOutput("social_contact_analysis"),
                         #helpText('Please note that contacts whose ages are given as a range but not exactly will have their age set to the mid-point of the range.')
                         #helpText('Please note that contacts whose ages are not given exactly will have by default their age sampled at random from the given range. If you want to use the midpoint, deselect "Age range: sample at random" [update 2020-10-05].')
                         helpText('The SOCRATES platform has been updated since the publication in BMC Research Notes. See the "Updates" tab for more info.')
                ),
                tabPanel("Matrix per capita", 
                         helpText('This per capita matrix presents the daily contact rate for every individual of an age group with all other individuals in the population.'),
                         plotOutput('plot_cnt_matrix_per_capita',width = "80%", height = "300px")),
                tabPanel("Contact rates", 
                         plotOutput('plot_mean_number_contacts',width = "80%", height = "300px")),
                tabPanel("Participants", 
                         helpText('Brief summary of participant data:'),
                         dataTableOutput('table_participants')),
                tabPanel("Weights",     
                         uiOutput("project_website_weights"),
                         dataTableOutput('table_weights')),
                tabPanel("Data sets",
                         uiOutput("project_website_data"),
                         dataTableOutput("social_contact_data")),
                tabPanel("About CoMix",
                        includeMarkdown("doc/doc_comix.md")),
                tabPanel("Updates",
                         includeMarkdown("doc/doc_updates.md"))
        )
  )
))
