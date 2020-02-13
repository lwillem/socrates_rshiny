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
  cnt_matrix_reference<- reactive({

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
                           bool_schools_closed = FALSE)
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
                       bool_schools_closed = input$bool_schools_closed)
  })
  
  # plot social contact matrix
  output$plot_cnt_matrix <- renderPlot({
    plot_cnt_matrix(cnt_matrix_control()$matrix)
  })
  
  # print social contact matrix
  output$print_cnt_matrix_comparison <- renderPrint({
    
    cnt_matrix_ui <- cnt_matrix_ui()
    
    # CLI
    print(cnt_matrix_ui)

    if(input$bool_schools_closed){
      print("schools closed")
      cnt_matrix_ref <- cnt_matrix_reference()
      
      print("ratio = with control / without control")
      compare_contact_matrices(cnt_matrix_ui$matrix,cnt_matrix_ref$matrix)
    }
  })
  
})