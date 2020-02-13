#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => MAIN SCRIPT FOR THE R-SHINY USER INTERFACE
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# clear workspace
rm(list=ls(all=TRUE))
source('R/social_mixr_main.R')

# ######Automatically set working directory###
# if(require(rstudioapi) && isAvailable()){
#   current_path <- getActiveDocumentContext()$path 
#   #setwd(dirname(current_path ))
# }

# load shiny libraary
library(shiny)

# load social contact functions
source('R/sidebarPanel_config.R')
source('R/social_mixr_main.R')


# Define UI for social contact application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Social contact matrix"),
  
  # Sidebar with controls
  sidebarPanel(
    
    selectInput("country", "Country?",
                opt_country),
    
    textInput(inputId="age_breaks_text",
              label="Age breaks (comma delimited)",
              value="0,18,45,65"),
    
    selectInput("daytype", "Weekdays/weekends?",
                opt_day_type),
    
    selectInput("period", "Period? (regular/holiday)",
                opt_period),
    
    selectInput("duration", "Contact duration?",
                opt_duration),
    
    selectInput("touch", "Skin-to-skin touching?",
                opt_touch),
    
    checkboxInput("cnt_home",   "Contacts at HOME?", TRUE),
    checkboxInput("cnt_school", "Contacts at SCHOOL?", TRUE),
    checkboxInput("cnt_work",   "Contacts at WORK?", TRUE),
    checkboxInput("cnt_other",  "Contacts at OTHER places?", TRUE),
    checkboxInput("cnt_unknown",  "Contact location UNKNOWN?", TRUE),
    
    checkboxInput("symmetric", "Reciprocal contacts?", FALSE),
  
    helpText("Reactive strategies"),
    checkboxInput("bool_schools_closed","Close all schools"),
    
    sliderInput("telework_reference","Observed % telework",min=0,max=99,value=16),
    sliderInput("telework_target","Target % telework",min=0,max=99,value=16)
    
    #submit button
    #submitButton("Update View")

  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    
    plotOutput('plot_cnt_matrix',width = "80%", height = "300px"),
    
    verbatimTextOutput("print_cnt_matrix_comparison")
    
    #verbatimTextOutput("print_cnt_matrix_telework")
    
  )
))
