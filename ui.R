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

# clear workspace
rm(list=ls(all=TRUE))

# load all functions and packages
library('shiny')
source('R/socrates_main.R')

# Define UI for social contact application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Social Contact Rates (SOCRATES) Data Tool",
              windowTitle = paste0('SOCRATES (',version_id,')')),
  
  # Sidebar with controls
  sidebarPanel(
 
    selectInput("country", "Country",
                opt_country),
    
    textInput(inputId="age_breaks_text",
              label="Age breaks (comma delimited)",
              value=opt_age_breaks),
   
    #by default 'all contacts' to prevent warnings/errors, can be extended in "server" script. 
    selectInput("daytype", "Type of day",
                opt_day_type[1]),
    
    selectInput("duration", "Contact duration",
                opt_duration),
    
    selectInput("touch", "Contact intensity",
                opt_touch),
    
    selectInput("gender", "Gender",
                opt_gender),
    
    checkboxGroupInput('cnt_matrix_features',
                       label = 'Contact features',
                       choices = opt_matrix_features,
                       selected = opt_matrix_features),
    
    helpText("Transmission features"),
    checkboxInput("bool_transmission_param", "Age-specific transmission?",value = FALSE),
    
    conditionalPanel(
      condition = "input.bool_transmission_param == true",
      uiOutput("sliders_susceptibility"),
      uiOutput("sliders_infectiousness")
    ),
    
    checkboxInput("bool_location", "Include all locations",value = TRUE),
    
    conditionalPanel(
      condition = "input.bool_location == false",
      checkboxGroupInput('cnt_location',
                         label = 'Included locations',
                         choices = opt_location,
                         selected = opt_location)
    ),
    
    
    downloadButton('download_matrix',"Download matrix (.csv)", style = "width:99%;"),
    downloadButton('download_all',"Download all results (.RData)",style = "width:99%;"),

    # add version and link to project website
    headerPanel(""),
    uiOutput("project_website"),
    helpText('SOCRATES',version_id)
    
  ),
  
  mainPanel(
    
    # allways show matrix with contact rates
    plotOutput('plot_cnt_matrix',width = "80%", height = "300px"),
    
    # use tabs
    tabsetPanel(type = "tabs",
                tabPanel("Results", verbatimTextOutput("social_contact_analysis")),
                tabPanel("Per Capita",   plotOutput('plot_cnt_matrix_per_capita',width = "80%", height = "300px")),
                tabPanel("Data sets",    dataTableOutput("social_contact_data"),
                                         helpText('More info on the data will follow soon...')),
                tabPanel("Distancing",  helpText("Social distancing (beta version)"),
                                        verbatimTextOutput("social_distancing"),

                                        checkboxInput("bool_telework","Telework"),
                                        conditionalPanel(
                                           condition = "input.bool_telework == true",
                                           sliderInput("telework_reference","Observed % telework",min=0,max=99,value=5),
                                           sliderInput("telework_target","Target % telework",min=0,max=99,value=5)
                                         ),
                                        checkboxInput("bool_social_distancing","Social distancing"),
                                        conditionalPanel(
                                           condition = "input.bool_social_distancing == true",
                                           sliderInput("cnt_reduction_school","Reduce 'school' contacts (%)",min=0,max=100,value=0),
                                           sliderInput("cnt_reduction_transport","Reduce 'transport' contacts (%)",min=0,max=100,value=0),
                                           sliderInput("cnt_reduction_leisure","Reduce 'lesiure' contacts (%)",min=0,max=100,value=0),
                                           sliderInput("cnt_reduction_otherplace","Reduce 'otherplace' contacts (%)",min=0,max=100,value=0)
                                       )
                         )
                
    )
    
  )
))
