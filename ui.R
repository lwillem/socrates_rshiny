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
   
    selectInput("country", "Country?",
                opt_country),
    
    textInput(inputId="age_breaks_text",
              label="Age breaks (comma delimited)",
              value="0,18,45,65"),
   
    selectInput("daytype", "Type of day?",
                opt_day_type),

    selectInput("duration", "Contact duration?",
                opt_duration),
    
    selectInput("touch", "Skin-to-skin touching?",
                opt_touch),
    
    selectInput("gender", "Gender?",
                opt_gender),
    
    checkboxInput("symmetric", "Reciprocal contacts?", TRUE),
    
    checkboxGroupInput('cnt_location',
                       label = 'Included locations',
                       choices = opt_location,
                       selected = opt_location),
    
    helpText("Reactive strategies"),
    checkboxInput("bool_schools_closed","Close all schools"),
    
    sliderInput("telework_reference","Observed % telework",min=0,max=99,value=16),
    sliderInput("telework_target","Target % telework",min=0,max=99,value=16),
    
    downloadButton('download_matrix',"Download matrix (.csv)", style = "width:99%;"),
    downloadButton('download_all',"Download all results (.RData)",style = "width:99%;")

  ),
  
  mainPanel(
    
    plotOutput('plot_cnt_matrix',width = "80%", height = "300px"),
 
    verbatimTextOutput("social_contact_analysis")
    
  )
))
