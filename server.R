#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => MAIN SCRIPT FOR THE R-SHINY SERVER
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

library(shiny)
source('R/social_mixr_main.R')

# Define server logic required to plot various output
shinyServer(function(input, output) {
  
  # calculate social contact matrix
  cnt_matrix_out<- reactive({
    
    # set age intervals
    age_breaks_num <- as.numeric(unlist(strsplit(input$age_breaks_text,",")))
    
    # make sure the ages are increasing 
    age_breaks_num <- sort(age_breaks_num)
    
    # get specific social_mixr survey object
    survey_object <- get_survey_object(country      = input$country,
                                       daytype      = input$daytype,
                                       period       = input$period,
                                       touch        = input$touch,
                                       duration     = input$duration,
                                       cnt_home     = input$cnt_home,
                                       cnt_school   = input$cnt_school,
                                       cnt_work     = input$cnt_work,
                                       cnt_other    = input$cnt_other,
                                       cnt_unknown  = input$cnt_unknown)
    
    # symmetric?
    symmetric <- input$symmetric

    # run social_mixr function
    matrix_out <- contact_matrix(survey     = survey_object, 
                                 age.limits = age_breaks_num,
                                 symmetric  = symmetric,
                                 quiet      = TRUE)
    # return
    matrix_out
  })
  
  # print social contact matrix
  output$cnt_matrix <- renderPrint({
    cnt_matrix_out()
  })
  
  # plot social contact matrix
  output$plot_matrix_raster <- renderPlot({
    plot_cnt_matrix_raster(cnt_matrix_out()$matrix)
  })
  
  
  
})