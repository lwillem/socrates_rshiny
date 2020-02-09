
# clear workspace
rm(list=ls(all=TRUE))

######Automatically set working directory###
if(require(rstudioapi) && isAvailable()){
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
}

# load shiny libraary
library(shiny)

# load social contact functions
source('R/social_mixr_main.R')

# Define UI for social contact application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Contact matrices"),
  
  # Sidebar with controls
  sidebarPanel(
    
    selectInput("country", "Country?",
                opt_country),
    
    textInput(inputId="age_breaks_text",
              label="age breaks (comma delimited)",
              value="0,18,25,45,65"),
    
    selectInput("daytype", "weekdays/weekends?",
                opt_day_type),
    
    # selectInput("period", "regular/holiday period?",
    #             opt_period),
    
    selectInput("duration", "Contact duration?",
                opt_duration),
    
    selectInput("touch", "Skin-to-skin touching?",
                opt_touch),
    
    # selectInput("location", "Location?",
    #             opt_location),

    checkboxInput("cnt_home",   "Contacts at HOME?", TRUE),
    checkboxInput("cnt_school", "Contacts at SCHOOL?", TRUE),
    checkboxInput("cnt_work",   "Contacts at WORK?", TRUE),
    checkboxInput("cnt_other",  "Contacts at OTHER places?", TRUE),
    checkboxInput("cnt_unknown",  "Contact location UNKNOWN?", TRUE),
    
    checkboxInput("symmetric", "Symmetric", FALSE),
    
    #submit button
    submitButton("Update View")
    
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    verbatimTextOutput("cnt_matrix")
  )
))
