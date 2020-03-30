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

# Define server logic required to plot various output
shinyServer(function(input, output, session) {
  
  # create memory variable for the transmission param sliders
  bool_update <- reactiveValues(age_breaks_text = '')
  
  # Update UI panel(s) ####
  observe({
    
    # Update the "supplementary professional contacts" option
    if(opt_country_admin$has_suppl_professional_cnt_data[opt_country_admin$name == as.character(input$country)]){
      
      updateCheckboxGroupInput(session,'cnt_matrix_features',
                                selected = input$cnt_matrix_features,
                                choices  = opt_matrix_features)
    } else{
      updateCheckboxGroupInput(session,'cnt_matrix_features',
                               selected = input$cnt_matrix_features[-4],
                               choices  = opt_matrix_features[-4])
    }
      
    #Update the minimum "telework target" (at least the observed value)
    updateSliderInput(session, "telework_target", min = input$telework_reference)
    
    # Update whether the location-specific checkboxes are displayed
    if(input$bool_location)
      updateCheckboxGroupInput(session, "cnt_location", selected = opt_location)
    
    # Update whether the telework sliders are displayed
    if(!input$bool_telework)
      updateSliderInput(session, "telework_target", value = input$telework_reference)
    
    # if social distancing slider are not displayed => set all to 0
    if(!input$bool_social_distancing){
      updateSliderInput(session, "cnt_reduction_transport", value = 0)
      updateSliderInput(session, "cnt_reduction_leisure", value = 0)
      updateSliderInput(session, "cnt_reduction_otherplace", value = 0)
    }
      
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
    
    #update transmission sliders, if the age groups have changed
    if(bool_update$age_breaks_text != input$age_breaks_text){
      
      # adjust memory variable
      bool_update$age_breaks_text <- input$age_breaks_text
      
      # get age groups
      age_groups <- parse_age_values(input$age_breaks_text)
      num_age_groups <- length(age_groups)
      age_groups_label <- paste0('[',age_groups,'-',c(age_groups[-1],'+'),')')
      age_groups_label[num_age_groups] <- paste0(age_groups[num_age_groups],'+')
      
      # update sliders: susceptibility
      output$sliders_susceptibility <- renderUI({
        
        lapply(seq(age_groups), function(i) {
          sliderInput(inputId = paste0("s_susceptibility",i),
                      label = paste('Susceptibility:',age_groups_label[i]),
                      min = 0, max = 2, value = 1,step=0.1)
        })
      })
      
      # update sliders: infectiousness
      output$sliders_infectiousness <- renderUI({
        
        lapply(seq(age_groups), function(i) {
          sliderInput(inputId = paste0("s_infectiousness",i),
                      label = paste('infectiousness:',age_groups_label[i]),
                      min = 0, max = 2, value = 1,step=0.1)
        })
      })
      
    }
    
  })
 
  
  
  ## UPDATE CONTENT ####
  observe({
    
    # parse transmission parameters
    age_susceptibility_text    <- parse_input_list(input,'s_susceptibility')
    age_infectiousness_text    <- parse_input_list(input,'s_infectiousness')
    
    print(input$cnt_reduction_transport)
  
    # combine contact reductions
    # TODO: use notation from opt_location (capitals etc.)
    cnt_reduction <- data.frame(Transport  = input$cnt_reduction_transport/100,
                                Leisure    = input$cnt_reduction_leisure/100,
                                Otherplace = input$cnt_reduction_otherplace/100)

    
    # run social contact analysis
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
                                       max_part_weight     = max_part_weight,
                                       bool_transmission_param = input$bool_transmission_param,
                                       age_susceptibility_text = age_susceptibility_text,
                                       age_infectiousness_text = age_infectiousness_text,
                                       cnt_reduction           = cnt_reduction)
    
    # plot social contact matrix
    output$plot_cnt_matrix <- renderPlot({
      plot_cnt_matrix(out$matrix)
    })
    
    # plot social contact matrix per capita
    output$plot_cnt_matrix_per_capita <- renderPlot({
      if('matrix_per_capita' %in% names(out)){
        plot_cnt_matrix(out$matrix_per_capita, 'per capita')
      }
    })
    
    # print results
    output$social_contact_analysis <- renderPrint({
      out
    })
    
    # print results
    output$social_distancing <- renderPrint({
      out[names(out) %in% c('matrix','mij_ratio','notes')]
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
        download_contact_matrices(  country      = input$country,
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
                                    max_part_weight     = max_part_weight,
                                    bool_transmission_param = input$bool_transmission_param,
                                    age_susceptibility_text = age_susceptibility_text,
                                    age_infectiousness_text = age_infectiousness_text,
                                    filename = file)
      }
    )
    
    # create url link
    output$project_website <- renderUI({
      tagList("More info:", url)
    })
    
    # add social contact data info
    output$social_contact_data <- renderDataTable({
    data_table_description
    },
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '170px', targets = 0))
    ))
    
  })
  
  
  
  
  
})