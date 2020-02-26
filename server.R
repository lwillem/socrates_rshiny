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
  
  # Update UI panel(s) ####
  observe({
    
    #Update the minimum "telework target" (at least the observed value)
    updateSliderInput(session, "telework_target", min = input$telework_reference)
    
    # Update whether the location-specific checkboxes are displayed
    if(input$bool_location)
      updateCheckboxGroupInput(session, "cnt_location", selected = opt_location)
    
    # Update whether the telework sliders are displayed
    if(!input$bool_telework)
      updateSliderInput(session, "telework_target", value = input$telework_reference)
    
    # Update 'daytype' input, by default 'all contacts' to prevent warnings/errors  
    # Options can be extended based on data availability
    flag_country <- input$country == opt_country_admin$name
    if(opt_country_admin$has_holiday_data[flag_country]){
      updateSelectInput(session,"daytype", choices = opt_day_type, selected = input$daytype)
    } else if(opt_country_admin$has_dayofweek_data[flag_country]) {
      if(input$daytype %in% opt_day_type[1:3]){
        updateSelectInput(session,"daytype", choices = opt_day_type[1:3], selected = input$daytype)
      } else{
        updateSelectInput(session,"daytype", choices = opt_day_type[1:3])
      }
    } else {
      updateSelectInput(session,"daytype", choices = opt_day_type[1])
    }
    
  })
 
  ## UPDATE CONTENT ####
  observe({
    
    out <- run_social_contact_analysis(country      = input$country,
                                       daytype      = input$daytype,
                                       touch        = input$touch,
                                       duration     = input$duration,
                                       gender       = input$gender,
                                       cnt_location = input$cnt_location,
                                       cnt_matrix_features = input$cnt_matrix_features,
                                       age_breaks_text     = input$age_breaks_text,
                                       bool_schools_closed = input$bool_schools_closed,
                                       telework_reference  = input$telework_reference,
                                       telework_target     = input$telework_target,
                                       max_part_weight     = max_part_weight)
    
    # plot social contact matrix
    output$plot_cnt_matrix <- renderPlot({
      plot_cnt_matrix(out$matrix)
    })
    
    # print results
    output$social_contact_analysis <- renderPrint({
      out
    })
    
    # download matrix
    output$download_matrix <- downloadHandler(
      filename = function(file) {
        paste0(format(Sys.time(),'%Y%m%d%H%M%S'),"_social_contact_matrix.csv")
      },
      content = function(file) {
        
        cnt_matrix           <- unlist(out$matrix)
        colnames(cnt_matrix) <- paste0('contact_',colnames(cnt_matrix))
        cnt_matrix           <- cbind(age_group=row.names(out$matrix),cnt_matrix)
        
        write.table(cnt_matrix, file,sep=',',row.names=F)
      }
    )
    
    # download all
    output$download_all <- downloadHandler(
      filename = function(file) {
        paste0(format(Sys.time(),'%Y%m%d%H%M%S'),"_social_contact_analysis.RData")
      },
      content = function(file) {
        saveRDS(object = out, file)
      }
    )
  })
  
  
  
})