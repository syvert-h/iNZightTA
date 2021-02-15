
#####################################################
#################### Original Server #################### 
#####################################################

# This file containts mostly the server code from the original inzightta shiny app. 
# Added a button to download the data behind the visualization. Ongoing addition:
# data table below vis plot


prepped <- eventReactive(input$prep_button, {
  
  from_sw_file <- reactive({
    if (is.null(input$sw_file)) {""}
    else {readLines(input$sw_file$datapath)}
  })
  
  addl_stopwords <- reactive({
    input_sw <- unlist(strsplit(input$addl_stopwords, split = ","))
    c(input_sw, from_sw_file())
  })
  
  
  imported() %>%
    inzightta::format_data(input$lemmatise, input$stopwords, input$sw_lexicon, addl_stopwords())
})

sectioned <- reactive({
  data <- prepped()
  if (isTruthy(input$section_by)){
    data <- data %>%
      section(input$section_by)}
  data})

filtered <- reactive({
  data <- sectioned()
  if (isTruthy(input$filter_var) &
      isTruthy(input$filter_pred)){
    # data <- data %>%
    #   dplyr::filter(!! dplyr::sym(input$filter_var) == input$filter_pred)}
    
    ###########################
    data <- data %>%
      dplyr::filter(stringr::str_detect(!! dplyr::sym(input$filter_var), input$filter_pred))
    ###########################
  }
  data
})

# grouped <- reactive({
#    data <- filtered()
#    if (isTruthy(input$group_var)){
#      data <- data %>%
#        dplyr::group_by(!! dplyr::sym(input$group_var))}
#    data
# })

# # ############################################
# # Attempt at conditioning on features
# # ############################################
#
observe({
  if (input$subset_data == input$restore_data) {
    shinyjs::disable("restore_data")
    shinyjs::enable("subset_data")
  } else {
    shinyjs::enable("restore_data")
    shinyjs::disable("subset_data")
  }
})

# observe({
#   if (input$restore_data == 1) {
#     shinyjs::disable("restore_data")
#   } 
# })
# observe({
#    if (input$subset_data == 0) {
#      shinyjs::disable("restore_data")
#    } else {
#      shinyjs::enable("restore_data")
#      shinyjs::disable("subset_data")
#    }
#  })
#  
#  observe({
#    if (input$restore_data == 1) {
#      shinyjs::disable("restore_data")
#    } 
#  })

  values <- reactiveValues(data=NULL)
   
  observeEvent(input$subset_data, {
    filtered_rows <- input$insighted_table_rows_all
    values$data <- filtered_rows
  })
   
  insighted_filtered <- eventReactive(input$subset_data, {
     full_data <- filtered()
     full_data[values$data, ]
  })

  grouped <- reactive({
    if (input$restore_data >= input$subset_data){
      data <- filtered()
      if (isTruthy(input$group_var)){
        data <- data %>%
         dplyr::group_by(!! dplyr::sym(input$group_var))}
        data
      }
    else{
      insighted_filtered()
    }
  })

# ############################################
###########################################

output$table <- renderTable({
  filtered() %>% utils::head(300)})

output$sw_lexicon <- renderUI(selectInput("sw_lexicon", "Select the Stopword Lexicon",
                                          stopwords::stopwords_getsources(), selectize = F))
output$vars_to_filter <- renderUI(selectInput("filter_var",
                                              "select which column to apply filtering to",
                                              c("", names(sectioned())) %||% c(""), selectize = F))

output$group_by <- renderUI({
                            input$what_vis
                            if (input$what_vis == "Term Frequency-Inverse Document Frequency"){
                              selectInput("group_var",
                                          "select which columns to group on (grouping is combined with faceting)",
                                          c("", names(filtered())) %||% c(""), selectize = F)
                            }
                            else {
                              selectInput("group_var",
                                          "select which columns to group on",
                                          c("", names(filtered())) %||% c(""), selectize = F)
                            }
})

output$insight_options <- renderUI({
  switch(input$what_vis,
         "Term Frequency" = selectInput("vis_type",
                                        "Select how to Visualise it",
                                        list("Bar",
                                             "Word Cloud",
                                             "Page View",
                                             "Time Series",
                                             "Density",
                                             "Histogram"), selectize = F),
         "Term Frequency-Inverse Document Frequency" = selectInput("vis_type",
                                                                   "Select how to Visualise it",
                                                                   list("Bar",
                                                                        "Word Cloud",
                                                                        "Page View",
                                                                        "Time Series",
                                                                        "Density",
                                                                        "Histogram"), selectize = F),
         "n-gram Frequency" = tagList(selectInput("vis_type",
                                                  "Select how to Visualise it",
                                                  list("Bar",
                                                       "Word Cloud",
                                                       "Page View",
                                                       "Time Series",
                                                       "Density",
                                                       "Histogram"), selectize = F),
                                      sliderInput("n_gram",
                                                  "n-gram count",
                                                  2, 8, 2)),
         ##-------------------------------------------------------------------
         "Syllable Frequency" = selectInput("vis_type",
                                            "Select how to Visualise it",
                                            list("Bar",
                                                 # "Time Series",
                                                 # "Density",
                                                 # "Histogram",
                                                 "Stem Plot"), selectize = F),
         ##-------------------------------------------------------------------
         "Key Words" = tagList(selectInput("vis_type",
                                           "Select how to Visualise it",
                                           list("Bar",
                                                "Word Cloud",
                                                "Page View",
                                                "Time Series",
                                                "Density",
                                                "Histogram"), selectize = F),
                               selectInput("summ_method",
                                           "Method of summary generation",
                                           list("TextRank")
                                           #list("TextRank", "LexRank")
                                           ), selectize = F),
         ##########################
         
         "Word Tree" = tagList(
           textInput(
             inputId = "firstword",
             label = "Input the root word."),
           
           selectInput("tree_type", "Tree Type",
                          c("suffix", "prefix", "double"),
                          selected = "suffix", selectize = F)
         ),

         ##########################
         "Term Sentiment" = tagList(selectInput("vis_type",
                                                "Select how to Visualise it",
                                                list("Page View",
                                                     "Word Cloud",
                                                     "Time Series",
                                                     "Bar",
                                                     "Density",
                                                     "Histogram"), selectize = F)#,
                                    # selectInput("sent_lex",
                                    #             "Lexicon for Sentiment Dictionary",
                                    #             list("afinn", "bing",
                                    #                  "loughran", "nrc",
                                    #                  "nrc emotions", "loughran - all sentiments"))
                                    ),
         "Moving Average Term Sentiment" = tagList(selectInput("vis_type",
                                                               "Select how to Visualise it",
                                                               list("Time Series",
                                                                    "Word Cloud",
                                                                    "Page View",
                                                                    "Bar",
                                                                    "Density",
                                                                    "Histogram"), selectize = F),
                                                   sliderInput("term_sent_lag",
                                                               "Lag Length for Calculation of Moving Average",
                                                               3,10000,250) #,
                                                   # selectInput("sent_lex",
                                                   #             "Lexicon for Sentiment Dictionary",
                                                   #             list("afinn", "bing",
                                                   #                  "loughran", "nrc"))
                                                   ),
         "Aggregated Term Count" = tagList(selectInput("vis_type",
                                                       "Select how to Visualise it",
                                                       list("Bar",
                                                            "Word Cloud",
                                                            "Page View",
                                                            "Time Series",
                                                            "Density",
                                                            "Histogram"), selectize = F),
                                           selectInput("agg_var",
                                                       "Select which variable to aggregate on",
                                                       c("", names(grouped())) %||% c(""), selectize = F)),
         "Key Sections" = tagList(selectInput("vis_type",
                                              "Select how to Visualise it",
                                              list("Bar",
                                                   "Word Cloud",
                                                   "Page View",
                                                   "Time Series",
                                                   "Density",
                                                   "Histogram"), selectize = F),
                                  selectInput("summ_method",
                                              "Method of summary generation",
                                              list("LexRank"),
                                              #list("TextRank", "LexRank")
                                              selectize = F),
                                  selectInput("agg_var",
                                              "Select which variable to aggregate on",
                                              c("", names(grouped())) %||% c(""), selectize = F)),
         "Aggregated Sentiment" = tagList(selectInput("vis_type",
                                                      "Select how to Visualise it",
                                                      list("Page View",
                                                           "Word Cloud",
                                                           "Time Series",
                                                           "Bar",
                                                           "Density",
                                                           "Histogram"), selectize = F),
                                          # selectInput("sent_lex",
                                          #             "Lexicon for Sentiment Dictionary",
                                          #             list("afinn", "bing",
                                          #                  "loughran", "nrc")),
                                          selectInput("agg_var",
                                                      "Select which variable to aggregate on",
                                                      c("", names(grouped())) %||% c(""), selectize = F)),
         )})


################
output$senti_choices <- renderUI({
  if (input$what_vis == "Term Sentiment" && input$vis_type %in% c("Page View", "Bar")){
    selectInput("sent_lex",
                "Lexicon for Sentiment Dictionary",
                list("afinn", "bing",
                     "loughran", "nrc",
                     "nrc - emotions", "loughran - emotions"), selectize = F)
  }
  else {
    if(input$what_vis %in% c("Term Sentiment", "Moving Average Term Sentiment", "Aggregated Sentiment"))
      selectInput("sent_lex",
                  "Lexicon for Sentiment Dictionary",
                  list("afinn", "bing",
                       "loughran", "nrc"), selectize = F)
  }
})

output$senti_choices2 <- renderUI({
  if (input$what_vis == "Term Sentiment" && input$vis_type %in% c("Page View", "Bar")){
    if (input$sent_lex == "nrc - emotions"){
      selectInput("spec_senti",
                  "Highlight words with a sentiment of",
                  list("anger", "anticipation", "disgust", "fear", 
                       "joy", "sadness", "surprise", "trust"), selectize = F)
    }
    else if (input$sent_lex == "loughran - emotions"){
      selectInput("spec_senti",
                  "Highlight words with a sentiment of",
                  list("litigious", "uncertainty", 
                       "constraining","superfluous"), selectize = F)
    }
  }
})

insighted <- reactive({
  switch(input$what_vis,
         "Term Frequency" = get_term_insight(grouped(),
                                             input$what_vis),
         ##------------------------------------------------------
         "Syllable Frequency" = get_term_insight(grouped(), 
                                                 input$what_vis),
         ##------------------------------------------------------
         "n-gram Frequency" = get_term_insight(grouped(),
                                               c("n-grams", "n-gram Frequency"),
                                               input$n_gram),
         "Key Words" = get_term_insight(grouped(),
                                        input$what_vis,
                                        input$summ_method),
         
         ########################################
         "Term Frequency-Inverse Document Frequency" = get_tf_idf(grouped(), input$group_var),
         
         ########################################
         "Term Sentiment" = get_term_insight(grouped(),
                                             input$what_vis,
                                             input$sent_lex,
                                             ##############
                                             input$spec_senti
                                             ),
         "Moving Average Term Sentiment" = get_term_insight(grouped(),
                                                            input$what_vis,
                                                            input$sent_lex,
                                                            input$term_sent_lag))})

insighted_agg <- reactive({
  shiny::validate(
    need(input$agg_var != "", "Please select which variable to aggregate on")
  )
  
  switch(input$what_vis,
    "Aggregated Term Count" = get_aggregate_insight(grouped(),
                                                    c("Bound Aggregates", input$what_vis),
                                                    input$agg_var),
    "Key Sections" = get_aggregate_insight(grouped(),
                                           c("Bound Aggregates", input$what_vis),
                                           input$agg_var,
                                           input$summ_method),
    "Aggregated Sentiment" = get_aggregate_insight(grouped(),
                                                   c("Bound Aggregates", input$what_vis),
                                                   input$agg_var,
                                                   input$sent_lex, 
                                                   to_scale = input$scale_senti))
  })


output$vis_options <- renderUI({
  switch(input$vis_type,
         "Word Cloud" = tagList(sliderInput("num_terms",
                                            "Select the number of terms to visualise",
                                            3, 50, 5),
                                selectInput("wordcloud_shape",
                                            "Select the shape of the wordcloud",
                                            list("circle",
                                                 "cardioid",
                                                 "diamond",
                                                 "square",
                                                 "triangle-forward",
                                                 "triangle-upright",
                                                 "pentagon",
                                                 "star"), selectize = F)),
         "Page View" = switch(input$what_vis,
                                "Aggregated Term Count" =,
                                "Key Sections" =,
                                "Aggregated Sentiment" = tagList(sliderInput("num_terms",
                                           "Select the number of terms to visualise",
                                           3, 400, 100),
                               sliderInput("term_index",
                                           "Select the point to begin visualisation from",
                                           1, nrow(insighted_agg()), 1),
                               selectInput("palette",
                                           "Select the colour palette type",
                                           list("Sequential", "Diverging"), selectize = F)),
                              
                              tagList(sliderInput("num_terms",
                                                  "Select the number of terms to visualise",
                                                  3, 400, 100),
                                      sliderInput("term_index",
                                                  "Select the point to begin visualisation from",
                                                  1, nrow(insighted()), 1),
                                      selectInput("palette",
                                                  "Select the colour palette type",
                                                  list("Sequential", "Diverging"), selectize = F))), 
         
         "Bar" = tagList(sliderInput("num_terms", "Select the number of terms to visualise",
                                     2,50,5),
                         checkboxInput("desc", "Sort descending")),
         ##---------------------------------------------------------------------------------
         "Stem Plot" = tagList(
           sliderInput("num_stem_plots", "Select which term(s) to visualise", 1, 50, c(1,4))
           )
         ##-----------------------------------------------   -------------------------------      
         )})


output$vis_facet_by <- renderUI({input$what_vis
                                if (input$what_vis == "Term Frequency-Inverse Document Frequency"){
                                    tagList(selectInput("vis_facet",
                                                    "select which variable to facet on (Faceting plot by more than 50 levels may take a few minutes.)",
                                                    c("", names(grouped())) %||% c(""), selected = input$group_var, selectize = F), 
                                        checkboxInput("scale_fixed", "Scale Fixed", value=FALSE))
                                }
                      else{
                        tagList(selectInput("vis_facet",
                                            "select which variable to facet on (Faceting plot by more than 50 levels may take a few minutes.)",
                                            c("", names(grouped())) %||% c(""), selected = NULL, selectize = F),
                                checkboxInput("scale_fixed", "Scale Fixed", value=FALSE))
                      }
  })
 

visualisation <- reactive({
  switch(input$vis_type,
         "Word Cloud" = switch(input$what_vis,
                               "n-gram Frequency" = get_vis(
                                 insighted(),
                                 input$vis_type,
                                 input$what_vis,
                                 input$vis_facet,
                                 input$scale_fixed,
                                 input$num_terms,
                                 x = `n-grams`,
                                 shape = input$wordcloud_shape, 
                                 ncol = input$n_col_facet),
                               "Aggregated Term Count" =,
                               "Key Sections" =,
                               "Aggregated Sentiment" = get_vis(
                                 insighted_agg(),
                                 input$vis_type,
                                 input$what_vis,
                                 input$vis_facet,
                                 input$scale_fixed,
                                 input$num_terms,
                                 x = `Bound Aggregates`,
                                 shape = input$wordcloud_shape, 
                                 ncol = input$n_col_facet),
                               get_vis(insighted(),
                                       input$vis_type,
                                       input$what_vis,
                                       input$vis_facet,
                                       input$scale_fixed,
                                       input$num_terms,
                                       shape = input$wordcloud_shape, 
                                       ncol = input$n_col_facet)),
         "Page View" = switch(input$what_vis,
                              "Aggregated Term Count" =,
                              "Key Sections" =,
                              "Aggregated Sentiment" = get_vis(insighted_agg(), input$vis_type,
                                                               input$what_vis,
                                                               input$vis_facet,
                                                               input$scale_fixed,
                                                               input$num_terms,
                                                               input$term_index,
                                                               palette = input$palette, 
                                                               ncol = input$n_col_facet, 
                                                               word_size = input$page_text_size),
                             get_vis(insighted(), input$vis_type,
                                                 input$what_vis,
                                                 input$vis_facet,
                                                 input$scale_fixed,
                                                 input$num_terms,
                                                 input$term_index,
                                                 palette = input$palette, 
                                                 ncol = input$n_col_facet, 
                                                 word_size = input$page_text_size)),
         "Time Series" = switch(input$what_vis,
                                "Aggregated Term Count" =,
                                "Key Sections" =,
                                "Aggregated Sentiment" = get_vis(insighted_agg(), input$vis_type,
                                                                 input$what_vis,
                                                                 input$vis_facet,
                                                                 input$scale_fixed, 
                                                                 ncol = input$n_col_facet), 
                                   get_vis(insighted(), input$vis_type,
                                                         input$what_vis,
                                                         input$vis_facet,
                                                         input$scale_fixed, 
                                                         ncol = input$n_col_facet)),
         ##---------------------------------------------------------------------------------------
         "Stem Plot" = switch(input$what_vis,
                              "Syllable Frequency" = get_vis(insighted(),
                                                         input$vis_type,
                                                         input$what_vis,
                                                         input$vis_facet,
                                                         input$scale_fixed,
                                                         input$num_terms, #ncol
                                                         input$num_stem_plots #number of stemplots
                                                         )
                              ),
         ##---------------------------------------------------------------------------------------
         "Bar" = switch(input$what_vis,
                        "n-gram Frequency" = get_vis(insighted(),
                                                     input$vis_type,
                                                     input$what_vis,
                                                     input$vis_facet,
                                                     input$scale_fixed,
                                                     input$num_terms,
                                                     x = `n-grams`,
                                                     desc = input$desc, 
                                                     ncol = input$n_col_facet),
                        "Aggregated Term Count" =,
                        "Key Sections" =,
                        "Aggregated Sentiment" = get_vis(insighted_agg(),
                                                         input$vis_type,
                                                         input$what_vis,
                                                         input$vis_facet,
                                                         input$scale_fixed,
                                                         input$num_terms,
                                                         x = `Bound Aggregates`,
                                                         desc = input$desc, 
                                                         ncol = input$n_col_facet),
                        
                        get_vis(insighted(), input$vis_type,
                                input$what_vis,
                                facet_by = input$vis_facet,
                                input$scale_fixed,
                                input$num_terms,
                                desc = input$desc, 
                                ncol = input$n_col_facet)),
         "Density" = switch(input$what_vis,
                            "Aggregated Term Count" =,
                            "Key Sections" =,
                            "Aggregated Sentiment" = get_vis(insighted_agg(), input$vis_type,
                                                             input$what_vis,
                                                             input$vis_facet,
                                                             input$scale_fixed, 
                                                             ncol = input$n_col_facet),
                     get_vis(insighted(), input$vis_type,
                                       input$what_vis, input$vis_facet,
                                       input$scale_fixed, 
                                       ncol = input$n_col_facet)),
         "Histogram" = switch(input$what_vis,
                              "Aggregated Term Count" =,
                              "Key Sections" =,
                              "Aggregated Sentiment" = get_vis(insighted_agg(), input$vis_type,
                                                               input$what_vis,
                                                               input$vis_facet,
                                                               input$scale_fixed, 
                                                               ncol = input$n_col_facet),
                               get_vis(insighted(), input$vis_type,
                                                   input$what_vis,
                                                   input$vis_facet,
                                                   input$scale_fixed, 
                                                   ncol = input$n_col_facet))
)})

output$plot <- renderPlot({
  visualisation() + ggplot2::theme(axis.text = element_text(size = input$text_size))
})

# to allow users to dynamically alter the plot
output$plot.ui <- renderUI({
  plotOutput("plot", height = input$plot_height)
})

output$insighted_table <- DT::renderDT({
  if (input$what_vis %in% c("Aggregated Term Count","Key Sections","Aggregated Sentiment")){
    insighted_agg()
  }
  else{
    insighted()
  }
  }, 
filter = "bottom")

  
output$downloadData <- downloadHandler(
  filename = function() {
    paste(input$what_vis, ".csv", sep = "")
  },
  content = function(file) {
    if (input$what_vis %in% c("Aggregated Term Count","Key Sections","Aggregated Sentiment")){
      utils::write.csv(insighted_agg(), file, row.names = FALSE)
    }
    else{
      utils::write.csv(insighted(), file, row.names = FALSE)
    }
  }
)


output$downloadprocessed <- downloadHandler(
  filename = function() {
    paste("processed", "RDS", sep = ".") 
  },
  content = function(file) {
    saveRDS(filtered(), file)
  }
)