library(shiny)

# Define server logic required to plot various output
shinyServer(function(input, output) {
  
  # get social contact matrix
  output$cnt_matrix <- renderPrint({
    
    age_breaks_num <- as.numeric(unlist(strsplit(input$age_breaks_text,",")))

    # print(contact_matrix(polymod, 
    #                countries  = input$country, 
    #                age.limits = age_breaks_num,
    #                symmetric  = input$symmetric))
    
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
    
    # run social_mixr function
    print(contact_matrix(survey_object, 
                   age.limits = age_breaks_num,
                   symmetric  = input$symmetric))
    
  })
})