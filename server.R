#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => MAIN SCRIPT FOR THE R-SHINY SERVER
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# load all functions and packages
# this is done automatically when running the App

# Define server logic required to plot various output
shinyServer(function(input, output, session) {
  
  ## if not CoMix: hide tab(s)
  if(!bool_is_comix_ui){
    hideTab(inputId = "tabs_results", target = "About CoMix")    
  }

  ## Wave input ----
  ## list to store reactive values
  values <- reactiveValues()

  # The dynamic input definition
  output$dynamicWaveInput <- renderUI({
    
    # This input exists if the `country` survey contains wave info
    if (opt_country_admin$has_waves[opt_country_admin$name == input$country]) {
      selectInput(inputId = 'wave_dynamic',
                  label   = 'Wave: start [panel]',
                  choices = opt_waves)
    } else {
      return(NULL)
    }
  })
  
  ## this bit fixes the issue
  ## force the dynamic dynamicWaveInput to reset if the country survey has no waves
  observe({
    if(opt_country_admin$has_waves[opt_country_admin$name == input$country]) {
      values$w_dynamic <- input$wave_dynamic
    } else {
      values$w_dynamic <- opt_waves[[1]]
    }
  })
  
  
  # Setup ####
  # create memory variable for the transmission param sliders
  bool_update <- reactiveValues(age_breaks_text = '',
                                sel_transmission = '')
  
  # create bool to show the SPC checkbox
  output$panelStatus <- reactive({
    opt_country_admin$has_suppl_professional_cnt_data[opt_country_admin$name == as.character(input$country)]
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
  # create bool to show the 'home member' checkbox
  output$panelStatusHome <- reactive({
    opt_country_admin$has_hhmember_cnt_data[opt_country_admin$name == as.character(input$country)]
  })
  outputOptions(output, "panelStatusHome", suspendWhenHidden = FALSE)

  # Update UI panel(s) ####
  observe({
  
    # if the SCP check box is not shown (nor used), set as "TRUE" 
    # MESSAGE ==>> "SCP are never excluded if the check box is not shown"
    show_spc_panel <- opt_country_admin$has_suppl_professional_cnt_data[opt_country_admin$name == as.character(input$country)]
    if(!show_spc_panel){
      updateCheckboxInput(session,"bool_spc", value = TRUE)
    }
    
    # if the HH-member checkbox is not shown (nor used), set as "FALSE"
    # MESSAGE ==>> "selection is never excluded if the check box is not shown"
    show_hhmember_panel <- opt_country_admin$has_hhmember_cnt_data[opt_country_admin$name == as.character(input$country)]
    if(!show_hhmember_panel){
      updateCheckboxInput(session,"bool_hhmember_selection", value = FALSE)
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
    if(opt_country_admin$has_cnt_duration_data[flag_country]){
      updateSelectInput(session,"duration", choices = opt_duration, selected = input$duration)
    } else{
      updateSelectInput(session,"duration", choices = opt_duration[1], selected = opt_duration[1])
    }
    
    # update contact intensity options
    if(opt_country_admin$has_cnt_touch_data[flag_country]){
      updateSelectInput(session,"touch", choices = opt_touch, selected = input$touch)
    } else{
      updateSelectInput(session,"touch", choices = opt_touch[1], selected = opt_touch[1])
    }
    
    # update wave  options
    opt_waves <- unlist(opt_country_admin$opt_wave[flag_country])
    if(opt_country_admin$has_waves[flag_country]){
      updateSelectInput(session,"wave_dynamic", choices = opt_waves, selected = input$wave_dynamic)
    } else {
      updateSelectInput(session,"wave_dynamic", choices = opt_waves[1], selected = opt_waves[1])
    }
    
    #update transmission sliders, if the age groups have changed
    if(bool_update$age_breaks_text  != input$age_breaks_text ||
       bool_update$sel_transmission != input$sel_transmission){
      
      # adjust memory variable
      bool_update$age_breaks_text  <- input$age_breaks_text
      bool_update$sel_transmission <- input$sel_transmission
      
      # range
      q_default_max  <- c(2,1)
      if(input$sel_transmission == 'sensitivity'){
        q_default_max  <- c(1,0.5,0.1)
      }
      
      # get age groups
      age_groups                       <- parse_age_values(input$age_breaks_text)
      num_age_groups                   <- length(age_groups)
      age_groups_label                 <- paste0('[',age_groups,'-',c(age_groups[-1],'+'),')')
      age_groups_label[num_age_groups] <- paste0(age_groups[num_age_groups],'+')
      
      # update sliders: susceptibility
      output$sliders_susceptibility <- renderUI({
        lapply(seq(age_groups), function(i) {
          sliderInput(inputId = paste0("s_susceptibility",i),
                      label = paste('Susceptibility:',age_groups_label[i]),
                      min = 0, max = q_default_max[1], value = q_default_max[2], step=0.1)
        })
      })
      
      
      # update sliders: infectiousness
      output$sliders_infectiousness <- renderUI({
        lapply(seq(age_groups), function(i) {
          sliderInput(inputId = paste0("s_infectiousness",i),
                      label = paste('infectiousness:',age_groups_label[i]),
                      min = 0, max = q_default_max[1], value = q_default_max[2], step=0.1)
        }) # end: lapply
      }) # end= renderUI
      
      # update slider: Proportionality factor
      output$sliders_q <- renderUI({
          sliderInput(inputId = paste0("s_q"),
                      label = paste('Proportionality factor (q):'),
                      min = 0, max = 5, value = 1,step=0.1)
      })
      
      # update slider: proportional perturbation
      output$sliders_delta_p <- renderUI({
        sliderInput(inputId = paste0("s_p"),
                    label = paste('Proportional perturbation (p):'),
                    min = -1, max = 1, value = 0.1,step=0.1)
      })
      
      # update slider: Projection time
      output$sliders_nrgen <- renderUI({
        sliderInput(inputId = paste0("s_nrgen"),
                    label = paste('Projection time (in generations):'),
                    min = 2, max = 20, value = 3,step=1)
      })
    } # end if-clause: update transmission sliders
  }) # end: observe
 
  ## Update results ####
  observe({
    disable("inputPanel_country")  # Disable survey input
    disable("inputPanel_general")  # Disable general input
    # note: excluded the age breaks to improve user experience
    
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'In progress...',
                 detail = 'Please wait.',
                )
    
    # combine general options
    features_select <- c(ifelse(is.null(input$bool_reciprocal),FALSE,input$bool_reciprocal),
                        input$bool_weigh_age,
                        input$bool_weigh_week,
                        input$bool_age_range,
                        input$bool_age_missing,
                        input$bool_spc,
                        input$bool_hhmember_selection)

    # parse transmission parameters
    bool_transmission_param <- input$sel_transmission == 'relative'
    bool_NGA_analysis       <- input$sel_transmission == 'sensitivity'
    
    # note: if the number of age groups is reduced, the previous "s_susceptibility" values remain, while not visible.
    # since they are hard to remove in the current code, we select the relevant age groups with max_dept.
    num_age_groups             <- length(parse_age_values(input$age_breaks_text))
    age_susceptibility_text    <- parse_input_list(input,'s_susceptibility',max_dept=num_age_groups)
    age_infectiousness_text    <- parse_input_list(input,'s_infectiousness',max_dept=num_age_groups)

    q_text <- parse_input_list(input,'s_q')
    delta_p_text <- parse_input_list(input,'s_p')
    nrgen_text <- parse_input_list(input,'s_nrgen')
    
    # combine contact reductions
    cnt_reduction <- data.frame(Home       = input$cnt_reduction_home/100,
                                Work       = input$cnt_reduction_work/100,
                                School     = input$cnt_reduction_school/100,
                                Transport  = input$cnt_reduction_transport/100,
                                Leisure    = input$cnt_reduction_leisure/100,
                                Otherplace = input$cnt_reduction_otherplace/100)

    # fix for wave
    if(is.null(values$w_dynamic)) values$w_dynamic <- opt_waves[[1]]
    
    # run social contact analysis
    out <- run_social_contact_analysis(country      = input$country,
                                       daytype      = input$daytype,
                                       touch        = input$touch,
                                       duration     = input$duration,
                                       gender       = input$gender,
                                       cnt_location = input$cnt_location,
                                       cnt_matrix_features  = opt_matrix_features[features_select],
                                       age_breaks_text      = input$age_breaks_text,
                                       weight_threshold     = weight_threshold,
                                       cnt_reduction           = cnt_reduction,
                                       wave                    = values$w_dynamic,
                                       age_susceptibility_text = age_susceptibility_text,
                                       age_infectiousness_text = age_infectiousness_text,
                                       bool_NGA_analysis       = bool_NGA_analysis,
                                       q_text                  = q_text,
                                       delta_p_text            = delta_p_text,
                                       nrgen_text              = nrgen_text)
    
    # plot social contact matrix
    output$plot_cnt_matrix <- renderPlot({
      scale_max <- ifelse(input$bool_matrix_limit == TRUE ,input$ui_scale_max,NA) 
      plot_cnt_matrix(out$matrix,scale_max=scale_max)
    })
    
    # plot social contact matrix per capita
    output$plot_cnt_matrix_per_capita <- renderPlot({
      if('matrix_per_capita' %in% names(out)){
        plot_cnt_matrix(mij = out$matrix_per_capita, 'per capita')
      } else{
        get_dummy_plot_for_ui("MISSING DATA ISSUE...\nUNABLE TO GENERATE THIS MATRIX \nConsider adjusting the age limits.")
      }
    })
    
    # plot mean number of social contacts
    output$plot_mean_number_contacts <- renderPlot({
      plot_mean_number_contacts(mij = out$matrix)
    })
    
    # plot relative incidence
    output$plot_relative_incidence <- renderPlot({
      if("relative_incidence" %in% names(out)){
        bplt <- barplot(out$relative_incidence,
                        xlab="Age group",
                        ylab="Relative incidence",
                        ylim=c(0,1),
                        cex.names =  0.8)
        text(x = bplt,
             y = out$relative_incidence,
             labels = round(out$relative_incidence,digits=2),
             pos=3)
      } else {
        get_dummy_plot_for_ui("Relative incidence results not available, see $notes")
      }
      
    })
   
    # add NGA input parameter table
    output$table_NGA_parameters <- renderDataTable({
      sel_NGA_param <- out$meta_data$parameter %in% c('age groups','age specific susceptibility','age specific infectiousness',
                                                      'q factor','delta p', 'nr generations')
      out_NGA_param <- data.table(out$meta_data[sel_NGA_param,])
      
      out_NGA_param$parameter <- gsub('susceptibility','susceptibility (~A)',out_NGA_param$parameter)
      out_NGA_param$parameter <- gsub('infectiousness','infectiousness (~H)',out_NGA_param$parameter)
      out_NGA_param$parameter <- gsub('q factor','proportionality factor (q)',out_NGA_param$parameter)
      out_NGA_param$parameter <- gsub('delta p','proportional perturbation (p)',out_NGA_param$parameter)
      out_NGA_param$parameter <- gsub('nr generations','number of generations (m)',out_NGA_param$parameter)
      out_NGA_param$value[-1] <- gsub(',',', ',out_NGA_param$value[-1])
      
      out_NGA_param
    },
    options = list(
      autoWidth = TRUE,
      dom = 't',   # 't' specifies only the table should be shown
      paging = FALSE,     # Disable pagination
      info = FALSE,       # Disable the info text (e.g., "Showing 1 to 10 of X entries")
      columnDefs = list(list(width = '50%', targets = 0))
    ))
    
    # plot next_gen_matrix
    output$plot_next_gen_matrix <- renderPlot({
      if("NGA" %in% names(out)){
        plot_next_gen_matrix(next_gen_matrix = out$NGA$next_gen_matrix)
      } else {
        get_dummy_plot_for_ui("NGA results not available")
      }
    })
    
    # plot elas
    output$plot_ELAS <- renderPlot({
      if("NGA" %in% names(out)){
        plot_NGA_elas(R_t = out$NGA$R_t,
                      elasticity_tbl = out$NGA$elasticity_tbl)
      } else {
          get_dummy_plot_for_ui("NGA results not available")
        }
    })
    
    # plot RI w.r.t a
    output$plot_RI_a <- renderPlot({
      if("NGA" %in% names(out)){
        plot_NGA_RI(NGA = out$NGA,
                    delta_p = out$meta_data$value[grepl('delta p',out$meta_data$parameter)], 
                    rn_gen = out$meta_data$value[grepl('nr generations',out$meta_data$parameter)],
                    bool_susceptibility = TRUE)
      } else {
          get_dummy_plot_for_ui("NGA results not available")
        }
    })
    
    # plot RI w.r.t h
    output$plot_RI_h <- renderPlot({
      if("NGA" %in% names(out)){
        plot_NGA_RI(NGA = out$NGA,
                    delta_p = out$meta_data$value[grepl('delta p',out$meta_data$parameter)], 
                    rn_gen = out$meta_data$value[grepl('nr generations',out$meta_data$parameter)],
                    bool_susceptibility = FALSE)
      } else {
          get_dummy_plot_for_ui("NGA results not available")
        }
    })

    # print results
    output$social_contact_analysis <- renderPrint({
      
      if("notes" %in% names(out)){
        print_notes(notes_vector = out$notes, txt_width = 70)
      }
      
      # exclude results with separate tab
      list_exclude <- c('weights','participants','participants.weights','meta_data','notes')
      print(out[!names(out) %in% list_exclude])
      
    })
    
    # print notes
    output$print_notes <- renderPrint({
      if("notes" %in% names(out)){
        print_notes(notes_vector = out$notes, txt_width = 70)
       }
    })
    
    # Re-enable the inputs after all results are (re)generated and displayed
    enable("inputPanel_survey")
    enable("inputPanel_general")
    
    # download matrix
    output$download_matrix <- downloadHandler(
      filename = function(file) {
        paste0(format(Sys.time(),'%Y%m%d%H%M%S'),"_social_contact_matrix.csv")
      },
      content = function(file) {
        
        # Disable inputs while the matrix is being generated for download
        disable("inputPanel")

        # Assuming `out$matrix` is available in your server logic
        cnt_matrix           <- unlist(out$matrix)
        colnames(cnt_matrix) <- paste0('contact_',colnames(cnt_matrix))
        cnt_matrix           <- cbind(age_group=row.names(out$matrix),cnt_matrix)
        
        # Write the matrix to a CSV file
        write.table(cnt_matrix, file,sep=',',row.names=F)
        
        # Re-enable inputs after the download is ready
        enable("inputPanel")
      }
    )
    
    # download all
    output$download_all <- downloadHandler(
      filename = function(file) {
        paste0(format(Sys.time(),'%Y%m%d%H%M%S'),"_social_contact_analysis.RData")
      },
      content = function(file) {
        # Disable inputs while the matrix is being generated for download
        disable("inputPanel")
        
          download_contact_matrices(country      = input$country,
                                    daytype      = input$daytype,
                                    touch        = input$touch,
                                    duration     = input$duration,
                                    gender       = input$gender,
                                    cnt_location = input$cnt_location,
                                    cnt_matrix_features = opt_matrix_features[features_select],
                                    age_breaks_text     = input$age_breaks_text,
                                    weight_threshold     = weight_threshold,
                                    cnt_reduction           = cnt_reduction,
                                    wave                    = values$w_dynamic,
                                    age_susceptibility_text = age_susceptibility_text,
                                    age_infectiousness_text = age_infectiousness_text,
                                    bool_NGA_analysis       = bool_NGA_analysis,
                                    q_text                  = q_text,
                                    delta_p_text            = delta_p_text,
                                    nrgen_text              = nrgen_text,
                                    filename                = file)
          
          # Re-enable inputs after the download is ready
          enable("inputPanel")
          
      }
    )
    
    # create url link
    output$project_website <- renderUI({
      tagList("More info:", url)
    })
    
    # create url link
    output$project_website_data <- renderUI({
      tagList("More info on the social contact data initiative 
              is provided at ", url," with links to the ZENODO repositories. Info about the Supplementary Professional Contacts 
              (SPC) for the French dataset ",url_doc_spc)
    })
    # add social contact data info
    output$social_contact_data <- renderDataTable({
    data_table_description
    },
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '170px', targets = 0))
    ))
    
    # create url link
    output$project_website_weights <- renderUI({
      tagList('Based on the selected options, we calculate participant weights to account for age and the number of observations during week (5/7) and weekend (2/7) days. 
                         The United Nation’s World Population Prospects are used as reference. Weights are constrained to a maximum of 3 to limit the influence of single participants.
                         ',url_doc_weights)
    })
    # add weights table
    output$table_weights <- renderDataTable({
      if(any(is.null(out$participants.weights))){
        data.table('No weights selected' = '')
      } else {
        out$participants.weights
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
  
  # create url link
  output$socrates_website <- renderUI({
    tagList(url_socrates)
  })
  
  # create url link
  output$socrates_website_data <- renderUI({
    tagList("The goal of the CoMix project has been to measure social distancing during the COVID-19 pandemic. This tool is part of the", url_socrates)
  })
  
  # create url link
  output$socrates_website_comix <- renderUI({
    tagList("Also have a look at", url_socrates_comix)
  })
  
})
