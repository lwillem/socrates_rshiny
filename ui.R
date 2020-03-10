#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => MAIN SCRIPT FOR THE R-SHINY USER INTERFACE
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# clear workspace
rm(list=ls(all=TRUE))

# load all functions and packages
library('shiny')
source('R/socrates_main.R')

# Define UI for social contact application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Social Contact Rates (Socrates) Data Tool"),
  
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
                       label = 'Matrix options',
                       choices = opt_matrix_features,
                       selected = opt_matrix_features),
    
    checkboxInput("bool_location", "Include all locations",value = TRUE),
    
    conditionalPanel(
      condition = "input.bool_location == false",
      checkboxGroupInput('cnt_location',
                         label = 'Included locations',
                         choices = opt_location,
                         selected = opt_location)
    ),
    
    
    helpText("Reactive strategies"),
    checkboxInput("bool_schools_closed","Close all schools"),
    
    checkboxInput("bool_telework","Telework"),
    conditionalPanel(
      condition = "input.bool_telework == true",
      sliderInput("telework_reference","Observed % telework",min=0,max=99,value=5),
      sliderInput("telework_target","Target % telework",min=0,max=99,value=5)
    ),
    
    helpText("Transmission parameters"),
    checkboxInput("bool_transmission_param", "Include transmission parameters",value = FALSE),
    
    conditionalPanel(
      condition = "input.bool_transmission_param == true",
      textInput(inputId="age_susceptibility_text",
                label="Age specific susceptibility (comma delimited)",
                value=opt_age_susceptibility),
      textInput(inputId="age_infectivity_text",
                label="Age specific infectivity (comma delimited)",
                value=opt_age_infectivity)
    ),
    
    
    
    downloadButton('download_matrix',"Download matrix (.csv)", style = "width:99%;"),
    downloadButton('download_all',"Download all results (.RData)",style = "width:99%;")

  ),
  
  mainPanel(
    
    plotOutput('plot_cnt_matrix',width = "80%", height = "300px"),
 
    verbatimTextOutput("social_contact_analysis")
  )
))
