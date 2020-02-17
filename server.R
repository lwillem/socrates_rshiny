#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => MAIN SCRIPT FOR THE R-SHINY SERVER
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# load all functions and packages
library('shiny')
source('R/socrates_main.R')

# controls the number of digits to print when printing numeric values
options(digits = 3)

# Define server logic required to plot various output
shinyServer(function(input, output, session) {
  
  app_social_contact_analysis <- reactive({
    
    out <- run_social_contact_analysis(country      = input$country,
                                       daytype      = input$daytype,
                                       touch        = input$touch,
                                       duration     = input$duration,
                                       gender       = input$gender,
                                       cnt_home     = input$cnt_home,
                                       cnt_school   = input$cnt_school,
                                       cnt_work     = input$cnt_work,
                                       cnt_other    = input$cnt_other,
                                       symmetric    = input$symmetric,
                                       age_breaks_text     = input$age_breaks_text,
                                       bool_schools_closed = input$bool_schools_closed,
                                       telework_reference  = input$telework_reference,
                                       telework_target     = input$telework_target)
    
    out
  })
  
  output$social_contact_analysis <- renderPrint({
    app_social_contact_analysis()
  })
  
  cnt_matrix_ui<- reactive({
    
    get_contact_matrix(country      = input$country,
                       daytype      = input$daytype,
                       touch        = input$touch,
                       duration     = input$duration,
                       gender       = input$gender,
                       cnt_home     = input$cnt_home,
                       cnt_school   = input$cnt_school,
                       cnt_work     = input$cnt_work,
                       cnt_other    = input$cnt_other,
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
  
  output$download_matrix <- downloadHandler(
    filename = function(file) {
      paste0(format(Sys.time(),'%Y%m%d%H%M%S'),"_social_contact_matrix.csv")
    },
    content = function(file) {
      
      out <- cnt_matrix_ui()
      
      cnt_matrix           <- unlist(out$matrix)
      colnames(cnt_matrix) <- paste0('contact_',colnames(cnt_matrix))
      cnt_matrix           <- cbind(age_group=row.names(out$matrix),cnt_matrix)
      
      #write.table(social_contact_analysis(), file,sep=',')
      write.table(cnt_matrix, file,sep=',',row.names=F)
    }
  )
  
  output$download_all <- downloadHandler(
    filename = function(file) {
      paste0(format(Sys.time(),'%Y%m%d%H%M%S'),"_social_contact_analysis.RData")
    },
    content = function(file) {
      out <- app_social_contact_analysis()
      saveRDS(object = out, file)
    }
  )

})