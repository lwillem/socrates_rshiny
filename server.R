library(shiny)

# Define server logic required to plot various output
shinyServer(function(input, output) {
  
  # calculate social contact matrix
  cnt_matrix_out<- reactive({
    
    # set age intervals
    age_breaks_num <- as.numeric(unlist(strsplit(input$age_breaks_text,",")))
    
    # check if increasing... 
    age_breaks_num <- sort(age_breaks_num)
    
    # get specific social_mixr survey object
    survey_object <- get_survey_object(country      = input$country,
                                       sel_weekday  = input$daytype,
                                       sel_touch    = input$touch,
                                       sel_duration = input$duration,
                                       cnt_home     = input$cnt_home,
                                       cnt_school   = input$cnt_school,
                                       cnt_work     = input$cnt_work,
                                       cnt_other    = input$cnt_other,
                                       cnt_unknown  = input$cnt_unknown)
    
    # symmetric?
    #bool_symmetric <- input$symmetric
    bool_symmetric <- FALSE
    
    # run social_mixr function
    contact_matrix(survey_object, 
                   age.limits = age_breaks_num,
                   symmetric  = bool_symmetric,
                   quiet      = TRUE)
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