#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => MAIN SCRIPT FOR THE R-SHINY SERVER
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

library(shiny)
library('socialmixr')
source('R/social_mixr_main.R')

# controls the number of digits to print when printing numeric values
options(digits = 3)

# Define server logic required to plot various output
shinyServer(function(input, output, session) {
  
  output$social_contact_analysis <- renderPrint({
    
    out <- run_social_contact_analysis(country      = input$country,
                                 daytype      = input$daytype,
                                 period       = input$period,
                                 touch        = input$touch,
                                 duration     = input$duration,
                                 cnt_home     = input$cnt_home,
                                 cnt_school   = input$cnt_school,
                                 cnt_work     = input$cnt_work,
                                 cnt_other    = input$cnt_other,
                                 cnt_unknown  = input$cnt_unknown,
                                 symmetric    = input$symmetric,
                                 age_breaks_text     = input$age_breaks_text,
                                 bool_schools_closed = input$bool_schools_closed,
                                 telework_reference  = input$telework_reference,
                                 telework_target     = input$telework_target)
    
    print(out)
  })
  
  cnt_matrix_ui<- reactive({
    
    get_contact_matrix(country      = input$country,
                       daytype      = input$daytype,
                       period       = input$period,
                       touch        = input$touch,
                       duration     = input$duration,
                       cnt_home     = input$cnt_home,
                       cnt_school   = input$cnt_school,
                       cnt_work     = input$cnt_work,
                       cnt_other    = input$cnt_other,
                       cnt_unknown  = input$cnt_unknown,
                       symmetric    = input$symmetric,
                       age_breaks_text = input$age_breaks_text,
                       bool_schools_closed = input$bool_schools_closed,
                       bool_exclusive = FALSE)
  })
  
  # plot social contact matrix
  output$plot_cnt_matrix <- renderPlot({
    plot_cnt_matrix(cnt_matrix_ui()$matrix)
  })
  
  # Update the minimum "telework target" (at least the observed value)
  observe({
      updateSliderInput(session, "telework_target", min = input$telework_reference)
  })
  
})