#####################################################
#################### Sidebar Panel #################### 
#####################################################

# This file containts the server code that changes the input options in the 
# side bar panel depending on where the user wants to import text data from. 


##### Stopwords file upload
shinyjs::onclick("toggleAdvanced4",
                 shinyjs::toggle(id = "advanced4", anim = TRUE))

##### Spotify Authentication tab
shinyjs::onclick("toggleAdvanced2",
                 shinyjs::toggle(id = "advanced2", anim = TRUE))

##### Twitter Authentication tab
shinyjs::onclick("toggleAdvanced",
                 shinyjs::toggle(id = "advanced", anim = TRUE))

##### The Guardian Authentication tab
shinyjs::onclick("toggleAdvanced3",
                 shinyjs::toggle(id = "advanced3", anim = TRUE))

output$side <- renderUI({
  ##### The Guardian choices for categories
  guardian_sec <- c("all","entertainment","sport","world","opinion","economy",
                    "national","artanddesign","politics","environment",
                    "lifestyle","jobs","education", "technology", "science",
                    "fashion", "culture", "society", "tourism", "health", "cars",
                    "uk-news", "business", "football", "stage", "money")
  
  
  switch(input$import_from,
         "Project Gutenberg" = tagList(actionButton("gather_data", "Import text"), 
                                       # tags$hr(),
                                       # tags$h4("Pre-process"),
                                       # checkboxInput("expand_contractions", "Expand contractions"), 
                                       #actionButton("pre_process_text", "Pre-process text"), 
                                       tags$hr() 
                                       ),

         
         "Upload .txt, .csv, .xlsx, or .xls file" = tagList(tags$h4("Import"),
                                                            fileInput("file1", "Choose File(s)",
                                                                      multiple = TRUE,
                                                                      accept = c("text/csv",
                                                                                 "text/comma-separated-values,text/plain",
                                                                                 ".csv", ".xlsx", ".xls")),
                                                            actionButton("gather_data", "Import text"), 
                                                            # tags$hr(),
                                                            # tags$h4("Pre-process"),
                                                            # checkboxInput("expand_contractions", "Expand contractions"), 
                                                            #actionButton("pre_process_text", "Pre-process text"),
                                                            tags$hr()),
         "Spotify/Genius" = tagList(
             a(id = "toggleAdvanced2", "***Create Spotify and Genius authorization tokens***", href = "#"),
             shinyjs::hidden(
               div(id = "advanced2",
                   textInput(inputId = "spotify_id",
                             label = "Spotify client ID",
                             value = ""),
                   textInput(inputId = "spotify_secret",
                             label = "Spotify client secret",
                             value = ""),
                   textInput(inputId = "genius_id",
                             label = "Genius client ID",
                             value = ""),
                   textInput(inputId = "genius_secret",
                             label = "Genius client secret",
                             value = ""),
                   textInput(inputId = "genius_api_token",
                             label = "Genius API token",
                             value = ""),
                   actionButton("get_spot_token", "Create tokens")
               )
             ),
             
             tags$hr(),
           
           radioButtons("type_spotify", "Retrieve lyrics for",
                        c("songs" = "songs",
                          "album" = "album",
                          "spotify playlist ID" = "playlist")),
           
           output$more_spot <- renderUI({switch(input$type_spotify,
                                                "songs" = tagList(textInput(inputId = "artist", label = "Artist(s)", placeholder = "Taylor Swift%Adele%Lorde"),
                                                                  textInput(inputId = "song_title", label = "Song Title(s)", placeholder = "Love Story%Rolling in the Deep%Royals")),
                                                "album" = tagList(textInput(inputId = "artist", label = "Artist", placeholder = "Ariana Grande"),
                                                                  textInput(inputId = "album_title", label = "Album Title", placeholder = "Sweetener%Thank U, Next")),
                                                "playlist" = tagList(textInput(inputId = "spotify_username", label = "Spotify Username", placeholder = "spotify"),
                                                                     textInput(inputId = "playlist_id", label = "Spotify Playlist ID", placeholder = "4OIVU71yO7SzyGrh0ils2i"))
                                                
           )}),
           actionButton("gather_data", "Gather lyrics"),
           # tags$hr(),
           # tags$h4("Pre-process"),
           # checkboxInput("expand_contractions", "Expand contractions"), 
           #actionButton("pre_process_text", "Pre-process text"), 
           tags$hr()
         ),
         
         "The Guardian Articles" = tagList(
           
           a(id = "toggleAdvanced3", "***Input The Guardian API Key***", href = "#"),
           shinyjs::hidden(
             div(id = "advanced3",
                 textInput(inputId = "guardian_api",
                           label = "API Key",
                           value = "")
             )
           ),
           
           tags$hr(),
           
           textInput(inputId = "guardian_keywords", label = "Keywords", placeholder = "Sir+Peter+Jackson"),
           output$add_info <- renderUI({HTML(paste0("<small>", 'For exact phrases and matches,
                                                 please encapsulate the keywords in %22 (e.g "%22Death+of+Margaret+Thatcher%22")',
                                                    "</small>", "<br></br>"))}),
           
           dateRangeInput("guardian_dates", "Date range"),
           selectizeInput("guardian_sections", "Section", guardian_sec),
           actionButton("gather_data", "Gather articles"),
           # tags$hr(),
           # checkboxInput("expand_contractions", "Expand contractions"), 
           # tags$h4("Pre-process"),
           #actionButton("pre_process_text", "Pre-process text"), 
           tags$hr()
           
         ),
         
         "stuff.co.nz Comments" = tagList(
           
           textInput(inputId = "rss_link", label = "URL of RSS Feed", placeholder = ""),
           
           actionButton("gather_data", "Gather comments"),
           # tags$hr(),
           # checkboxInput("expand_contractions", "Expand contractions"), 
           # actionButton("pre_process_text", "Pre-process text"), 
           # tags$h4("Pre-process"),
           tags$hr()
         ),
         
         "Twitter" = tagList(
           a(id = "toggleAdvanced", "***Create Twitter authorization token***", href = "#"),
           shinyjs::hidden(
             div(id = "advanced",
                 textInput(inputId = "appname",
                           label = "Name of Twitter application",
                           value = ""),
                 textInput(inputId = "key",
                           label = "Application API key",
                           value = ""),
                 textInput(inputId = "secret",
                           label = "Application API secret",
                           value = ""),
                 textInput(inputId = "access_token",
                           label = "Access token as supplied by Twitter",
                           value = ""),
                 textInput(inputId = "access_secret",
                           label = "Access secret as supplied by Twitter",
                           value = ""),
                 actionButton("get_twitter_token", "Create token")
             )
           ),
           
           tags$hr(),
           
           radioButtons("tw_type", "Retrieve tweets from a",
                        c("user" = "user2",
                          "hashtag" = "hashtag")),
           
          output$more_twitter <- renderUI({switch(input$tw_type,
                                                  "user2" = tagList(
                                                    textInput(inputId = "given_user",
                                                              label = "Please provide the Twitter username(s).",
                                                              placeholder = "@user1%@user2%@user3")
                                                  ),
                                                  "hashtag" = tagList(
                                                    textInput(inputId = "given_hashtag",
                                                              label = "Please provide the Twitter hashtag(s).",
                                                              placeholder = "#hash1%#hash2%#hash3")))
                                          }),
           
           numericInput(inputId = "num_tweets",
                        label = "Number of Tweets to Retrieve per user/hashtag",
                        value = 500),
           output$tweet_info <- renderUI({HTML(paste0("<small>", '(up to 18000 total tweets every 15 mins)',
                                                    "</small>", "<br></br>"))}),
           checkboxInput("include_retweets", "Include retweets"),
           actionButton("gather_data", "Gather tweets"),
           tags$hr()#,
           # tags$h4("Pre-process"),
           # checkboxInput("remove_hash", "Remove hashtags"), 
           # checkboxInput("remove_user", "Remove user mentions"),
           # checkboxInput("expand_contractions", "Expand contractions"), 
           #actionButton("pre_process_text", "Pre-process text"), 
           
           #tags$hr()
         ),
         "Reddit" = tagList(
           
           radioButtons("type_reddit", "Retrieve",
                        c("comments of a particular post" = "comments_url",
                          "comments" = "comment",
                          "submissions" = "submission")),
           
           output$more_reddit <- renderUI({switch(input$type_reddit,
                                                  "comment" = tagList(
                                                    textInput(inputId = "q", label = "Search Query", placeholder = "brexit"),
                                                    textInput(inputId = "subreddit", label = "Subreddit", placeholder = "politics"),
                                                    textInput(inputId = "after", label = "After Date Time (in epoch time)"),
                                                    textInput(inputId = "before",label = "Before Date Time (in epoch time)"),
                                                    
                                                    output$epoch <- renderUI({HTML(paste0("<small>", 'Convert human-readable date to epoch time <a href = "https://www.epochconverter.com/" target="_blank">here</a>.',
                                                                                          "</small>", "<br></br>"))}),
                                                    checkboxInput(inputId = "nest_level", label = "Only top level comments")
                                                    
                                                  ),
                                                  "submission" = tagList(
                                                    textInput(inputId = "title", label = "String to search for in post titles", placeholder = "vegan"),
                                                    textInput(inputId = "subreddit", label = "Subreddit", placeholder = "Cooking"),
                                                    textInput(inputId = "after", label = "After Date Time (in epoch time)"),
                                                    textInput(inputId = "before", label = "Before Date Time (in epoch time)"),
                                                    output$epoch <- renderUI({HTML(paste0("<small>", 'Convert human-readable date to epoch time <a href = "https://www.epochconverter.com/" target="_blank">here</a>.',
                                                                                          "</small>", "<br></br>"))})
                                                  ),
                                                  "comments_url" = tagList(
                                                    textInput(inputId = "url", label = "URL of Post"),
                                                    checkboxInput(inputId = "nest_level", label = "Only top level comments")
                                                  )
           )}),
           actionButton("gather_data", "Gather data"),
           # tags$hr(),
           # tags$h4("Pre-process"),
           # checkboxInput("expand_contractions", "Expand contractions"), 
           #actionButton("pre_process_text", "Pre-process text"), 
           tags$hr()
         )
         
  )
})


###### Choices of books for Project Gutenberg
gut <- copy(gutenbergr::gutenberg_metadata)
gut <- data.table::setDT(gut)
gut <- gut[language == "en" & gutenberg_id != 0 & has_text]
gut <- gut[,choice := gsub("\\n|\\r", "",
                           ifelse(!is.na(author),
                                  paste(title, author, sep = " :by: "),
                                  title))]
choices <- gut[,choice]

updateSelectizeInput(session, "gutenberg_work", choices = choices, server = TRUE)
###########################################################