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
                           bool_schools_closed = FALSE,
                           bool_exclusive = FALSE)
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
  
  cnt_matrix_work_excl<- reactive({
    
    get_contact_matrix(country      = input$country,
                       daytype      = input$daytype,
                       period       = input$period,
                       touch        = input$touch,
                       duration     = input$duration,
                       cnt_home     = input$cnt_home,
                       cnt_school   = input$cnt_school,
                       cnt_work     = FALSE,
                       cnt_other    = input$cnt_other,
                       cnt_unknown  = input$cnt_unknown,
                       symmetric    = input$symmetric,
                       age_breaks_text = input$age_breaks_text,
                       bool_schools_closed = input$bool_schools_closed,
                       bool_exclusive = TRUE)
  })
  
  # plot social contact matrix
  output$plot_cnt_matrix <- renderPlot({
    plot_cnt_matrix(cnt_matrix_ui()$matrix)
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
  
  # print social contact matrix
  output$print_cnt_matrix_telework <- renderPrint({
    
    cnt_matrix_ui <- cnt_matrix_ui()
    
    # CLI
    print(cnt_matrix_ui)
    
    if(input$telework_reference != input$telework_target){
      print("telework increased")
      
      # get contact matrix with work-contacts (exclusive)
      if(input$cnt_work){
        cnt_matrix_work_excl <- cnt_matrix_work_excl()$matrix
      } else{
        cnt_matrix_work_excl <- cnt_matrix_ui$matrix * 0
      }
      
      # apply reduction
      telework_increase  <- input$telework_target - input$telework_reference
      telework_reduction <- telework_increase / (1-input$telework_reference)
      cnt_matrix_work_reduction <- cnt_matrix_work_excl * telework_reduction
      print(telework_reduction)
      print(cnt_matrix_work_reduction)
      
      # calculate final matrix
      cnt_matrix_telework <- cnt_matrix_ui$matrix - cnt_matrix_work_reduction
      
      print("ratio = with control / without control")
      compare_contact_matrices(cnt_matrix_telework,cnt_matrix_ui$matrix)
    }
  })
  
})