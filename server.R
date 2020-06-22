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
  
  # create bool to show the SPC checkbox
  output$panelStatus <- reactive({
    opt_country_admin$has_suppl_professional_cnt_data[opt_country_admin$name == as.character(input$country)]
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
  # Update UI panel(s) ####
  observe({
  
    # if the SCP checkbox is not shown (nor used), set as "TRUE" 
    # MESSAGE ==>> "SCP are never excluded if the checkbox is not shown"
    show_spc_panel <- opt_country_admin$has_suppl_professional_cnt_data[opt_country_admin$name == as.character(input$country)]
    if(!show_spc_panel){
      updateCheckboxInput(session,"bool_spc", value = TRUE)
    }
    
    # Update whether the location-specific checkboxes are displayed
    if(input$bool_location)
      updateCheckboxGroupInput(session, "cnt_location", selected = opt_location)
    
    # if physical distancing slider are not displayed => set all to 0
    if(!input$bool_physical_distancing){
      updateSliderInput(session, "cnt_reduction_home", value = 0)
      updateSliderInput(session, "cnt_reduction_work", value = 0)
      updateSliderInput(session, "cnt_reduction_school", value = 0)
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
    
    # update contact duration options
    print(opt_country_admin[flag_country,])
    if(opt_country_admin$has_cnt_duration_data[flag_country]){
      updateSelectInput(session,"duration", choices = opt_duration, selected = input$duration)
    } else{
      updateSelectInput(session,"duration", choices = opt_duration[1], selected = opt_duration[1])
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
        }) # end: lapply
      }) # end= renderUI
      
    } # end if-clause: update transmission sliders
    
  }) # end: observe
 
  
  
  ## UPDATE CONTENT ####
  observe({

    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'In progress...',
                 detail = 'Please wait.',
                )
    
    # combine general options
    features_select <- c(input$bool_reciprocal,
                        input$bool_weigh_age,
                        input$bool_weigh_week,
                        input$bool_spc)

    # parse transmission parameters
    age_susceptibility_text    <- parse_input_list(input,'s_susceptibility')
    age_infectiousness_text    <- parse_input_list(input,'s_infectiousness')
    
    # combine contact reductions
    # TODO: use notation from opt_location (capitals etc.)
    cnt_reduction <- data.frame(Home       = input$cnt_reduction_home/100,
                                Work       = input$cnt_reduction_work/100,
                                School     = input$cnt_reduction_school/100,
                                Transport  = input$cnt_reduction_transport/100,
                                Leisure    = input$cnt_reduction_leisure/100,
                                Otherplace = input$cnt_reduction_otherplace/100)

    # run social contact analysis
    out <- run_social_contact_analysis(country      = input$country,
                                       daytype      = input$daytype,
                                       touch        = input$touch,
                                       duration     = input$duration,
                                       gender       = input$gender,
                                       cnt_location = input$cnt_location,
                                       cnt_matrix_features = opt_matrix_features[features_select],
                                       age_breaks_text     = input$age_breaks_text,
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
      # exclude results with separate tab
      list_exclude <- c('weights','participants','meta_data')
      out[!names(out) %in% list_exclude]
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
                                    cnt_matrix_features = cnt_matrix_features[features_select],
                                    age_breaks_text     = input$age_breaks_text,
                                    max_part_weight     = max_part_weight,
                                    bool_transmission_param = input$bool_transmission_param,
                                    age_susceptibility_text = age_susceptibility_text,
                                    age_infectiousness_text = age_infectiousness_text,
                                    cnt_reduction           = cnt_reduction,
                                    filename = file)
      }
    )
    
    # create url link
    output$project_website <- renderUI({
      tagList("More info:", url)
    })
    
    # create url link
    output$project_website_data <- renderUI({
      tagList("More info on the social contact data initiative 
              and links to the ZENODO repositories are provided at", url, " and data sets marked with an * contain supplementary professional contacts (SPC) imputed from aggregated statistics.")
    })
    # add social contact data info
    output$social_contact_data <- renderDataTable({
    data_table_description
    },
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '170px', targets = 0))
    ))
    
    # add weights table
    output$table_weights <- renderDataTable({
      if(any(is.null(out$weights))){
        data.table('No weights selected' = '')
      } else {
        out$weights
      }
    },
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '170px', targets = 0))
     ))
    
    # add weights table
    output$table_participants <- renderDataTable({
        out$participants
    },
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '170px', targets = 0))
    ))
    
  })
  
  
  
  
  
})