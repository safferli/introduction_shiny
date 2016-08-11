##############
# ANNO 6 EVENT SESSION DASHBOARD - server.R
##############

shinyServer(function(input, output, session) {
  
  ##############
  # REACTIVE EXPRESSIONS WRAP FUNCTIONS
  ##############
  rGetActionsByLevel <- reactive({
    getActionsByLevel(getEventData())
  })
  rGetEventTimeByEvent <- reactive({
    getEventTimeByEvent(getEventData())
  })
  rGetEventsByEvent <- reactive({
    getEventsByEvent(getEventData())
  })
  rGetMaterialsByEvent <- reactive({
    getMaterialsByEvent(getEventData())
  })
  rGetEventsByLevel <- reactive({
    getEventsByLevel(getEventData())
  })
  rGetMaterialsTotal <- reactive({
    getMaterialsTotal(getEventData())
  })
  rGetActionsTotal <- reactive({
    getActionsTotal(getEventData())
  })
  rGetActionsByLevel <- reactive({
    getActionsByLevel(getEventData())
  })
  rGetMilitaryLevelByLevel <- reactive({
    getMilitaryLevelByLevel(getEventData())
  })
  rGetAnalysisData <- reactive({
    getAnalysisData(getEventData())
  })
  rGetAvgEventSessions <- reactive({
    getAvgEventSessions(getEventData())
  })
  
  
  ##############
  # DATA QUERY
  ##############
  getEventData <- eventReactive(input$querybutton, {
    # Close any previous alerts
    closeAlert(session, "data_alert")
    
    ## PUT QUERY IN HERE
    # Get difficulty of corps
#     sql.corps <- sqlQuery(myconn,
#                           paste0("SELECT UserId AS user_id, contextName AS corp, ",
#                                  "difficulty, MIN(corpLevel) AS corp_level, MIN(CAST(ServerTimestamp AS date)) AS first_date ",
#                                  "FROM [DW_ANNO6_POSTLAUNCH].[event].[corporation_start] ",
#                                  "GROUP BY UserId, contextName, difficulty ",
#                                  "HAVING MIN(CAST(ServerTimestamp AS date)) >= '", input$dates[1], "'"),
#                           stringsAsFactors = FALSE)
#     # Get event sessions
#     sql.events <- sqlQuery(myconn,
#                            paste0("SELECT UserId AS user_id, Contextcorporation AS corp, ",
#                                   "SessionId AS session_id, EventId AS event_id, ",
#                                   "Contextsession AS event_session, ContextsessionId, optional_quests, ",
#                                   "corpLevel AS corp_level, result, ClientTimestamp AS time, ",
#                                   "earned_rare_base, earned_rare_earth, earned_rare_moon, ",
#                                   "earned_rare_polar, special_action_used_nuke, ",
#                                   "special_action_used_push, special_action_used_repair, ",
#                                   "special_action_used_shield, special_action_used_stun, special_action_used_support ",
#                                   "FROM [DW_ANNO6_POSTLAUNCH].[event].[STATS_EVENT_SESSION] ",
#                                   "WHERE CAST(ServerTimestamp AS date) >= '", input$dates[1], "' ",
#                                   "AND CAST(ServerTimestamp AS date) <= '", input$dates[2], "' "),
#                            stringsAsFactors = FALSE)
#     # Get start of context sessions
#     sql.sessions <- sqlQuery(myconn,
#                              paste0("SELECT UserId AS user_id, SessionId AS session_id, ",
#                                     "contextId AS context_id, contextName, ClientTimestamp AS session_start ",
#                                     "FROM [DW_ANNO6_POSTLAUNCH].[event].[session_start] ",
#                                     "WHERE CAST(ServerTimestamp AS date) >= '", input$dates[1]-1, "' ",
#                                     "AND CAST(ServerTimestamp AS date) <= '", input$dates[2], "' "),
#                              stringsAsFactors = FALSE) %>%
#       distinct(user_id, session_id, context_id, contextName)
#     # Get military level
#     sql.military <- sqlQuery(myconn,
#                              paste0("SELECT SessionId AS session_id, UserId AS user_id, ",
#                                     "Contextcorporation AS corp, ContextcorporationId, ",
#                                     "corpLevel AS corp_level, military_level, ClientTimestamp AS time ",
#                                     "FROM [DW_ANNO6_POSTLAUNCH].[event].[EVENT_MILITARY_LEVEL_UP] ",
#                                     "WHERE CAST(ServerTimestamp AS date) <= '", input$dates[2], "'"),
#                              stringsAsFactors = FALSE) %>%
#       group_by(user_id, corp) %>%
#       summarize(military_level = max(military_level))
    ## QUERY FINISHED HERE
    
    # Make sure we have data
    # If not throw up a little warning popup
#     if (length(sql.events[,1]) == 0) {
#       # Do some error or something, popup?
#       createAlert(session, "no_data_popup", alertId = "data_alert", style = "warning", title = "Data Error:",
#                   content = "No data found for given dates.")
#       return(invisible())
#     }
    
    # FORMATTING DATA AND MERGING WITH LOOKUPS
#     df.corps <- sql.corps %>%
#       distinct(user_id, corp, difficulty) %>%
#       select(-corp_level)
#     df.events <- inner_join(df.corps, sql.events, by=c("user_id","corp")) %>%
#       group_by(user_id, session_id, corp, ContextsessionId) %>%
#       arrange(desc(time)) %>%
#       slice(1)
#     df.events <- inner_join(df.events, sql.sessions,
#                             by=c("user_id","session_id",
#                                  "ContextsessionId"="context_id",
#                                  "event_session"="contextName"))
#     df.events <- left_join(df.events, event_lookup, by=c("event_session"))
#     # Add summary columns
#     df.events <- df.events %>%
#       mutate(completion_time = as.numeric(difftime(time, session_start, units = "mins"))) %>%
#       left_join(sql.military, by=c("user_id","corp")) %>%
#       replace_na(list(military_level = 1))
    
    # load("source/sql_data.rda")
    load(paste0(file.path, "sql_data.rda"))
    df.events <- df.events %>%
      filter(first_date >= as.Date(input$dates[1]) &
               as.Date(session_start) >= as.Date(input$dates[1]) & as.Date(session_start) <= as.Date(input$dates[2]))
    
    updateDateRangeInput(session, "dates",
                         start = as.Date(min(df.events$session_start)), min = as.Date(min(df.events$session_start)),
                         end = as.Date(max(df.events$session_start)), max = as.Date(max(df.events$session_start)))
    
    df.events <- df.events %>%
      filter(corp_level >= input$level_slider[1] & corp_level <= input$level_slider[2])
    
    # Change button colour to green
    updateButton(session, "querybutton", label = "Data Loaded", style = "success")
    
    return(df.events)
  })
  
  # Force query at dashboard page
  output$invisible <- renderText({
    getEventData()
    return(invisible())
  })
  
  ##############
  # FRONT PAGE STATS BOXES
  ##############
  output$usersBox <- renderValueBox({
    try({n_users <- length(unique(getEventData()$user_id))})
    if (!exists("n_users")) {
      n_users <- "-"
    }
    valueBox(paste0(n_users),
             "Unique Users",
             icon = icon("user", lib = "glyphicon"),
             color = "light-blue")
  })
  output$totalEventsBox <- renderValueBox({
    try({n_events <- length(getEventData()$user_id)})
    if (!exists("n_events")) {
      n_events <- "-"
    }
    valueBox(paste0(n_events),
             "Total Event Sessions",
             icon = icon("thumbs-up", lib = "glyphicon"),
             color = "light-blue")
  })
  output$successBox <- renderValueBox({
    try({n_success <- length((getEventData() %>% filter(result == "success"))$user_id)})
    if (!exists("n_success")) {
      n_success <- "-"
    }
    valueBox(paste0(n_success),
             "Successful Event Sessions",
             icon = icon("thumbs-up", lib = "glyphicon"),
             color = "light-blue")
  })
  output$timeBox <- renderValueBox({
    try({avg_time <- round(mean((getEventData() %>% filter(result == "success"))$completion_time), digits = 2)})
    if (!exists("avg_time")) {
      avg_time <- "-"
    }
    valueBox(paste0(avg_time),
             "Average Mins to Complete an Event",
             icon = icon("time", lib = "glyphicon"),
             color = "light-blue")
  })
  output$avgEventsBox <- renderValueBox({
    try({avg_time <- round(mean((getEventData() %>% group_by(user_id) %>% summarize(n_events = n()))$n_events), digits = 2)})
    if (!exists("avg_time")) {
      avg_time <- "-"
    }
    valueBox(paste0(avg_time),
             "Average Event Sessions per User",
             icon = icon("thumbs-up", lib = "glyphicon"),
             color = "light-blue")
  })
  output$avgEventsCorpBox <- renderValueBox({
    try({avg_time <- round(mean((getEventData() %>% group_by(user_id, corp) %>% summarize(n_events = n()))$n_events), digits = 2)})
    if (!exists("avg_time")) {
      avg_time <- "-"
    }
    valueBox(paste0(avg_time),
             "Average Event Sessions per Corporation",
             icon = icon("thumbs-up", lib = "glyphicon"),
             color = "light-blue")
  })
  
  
  ##############
  # PLOTS 1
  ##############
  # 1-1
  output$plot11 <- renderPlot({
    getPlotEventsByEvent(getEventData(), diff_filter = input$selectdiff11)
  })
  output$datatable11 <- renderDataTable({
    rGetEventsByEvent() %>% filter(difficulty == input$selectdiff11)
  }, options = list(pageLength = 10))
  #1-2
  output$plot12 <- renderPlot({
    getPlotEventTimeByEvent(getEventData(), diff_filter = input$selectdiff11)
  })
  output$datatable12 <- renderDataTable({
    rGetEventTimeByEvent() %>% filter(difficulty == input$selectdiff11)
  }, options = list(pageLength = 10))
  # 1-3
  output$plot13 <- renderPlot({
    getPlotEventsByLevel(getEventData(), diff_filter = input$selectdiff13, cumulative = input$checkbox13)
  })
  output$datatable13 <- renderDataTable({
    rGetEventsByLevel() %>% filter(difficulty == input$selectdiff13)
  }, options = list(pageLength = 10))
  # 1-4
  output$plot14 <- renderPlot({
    getPlotAvgEvents(rGetAvgEventSessions(), diff_filter = input$selectdiff14, plot_type = input$selectplot14)
  })
  output$datatable14 <- renderDataTable({
    rGetAvgEventSessions() %>%
      group_by(difficulty) %>%
      summarize(avg_event_sessions = mean(n_events),
                med_event_sessions = median(n_events),
                min_event_sessions = min(n_events),
                max_event_sessions = max(n_events))
  })
  
  ##############
  # PLOTS 2
  ##############
  # 2-1
  output$plot21 <- renderPlot({
    getPlotMaterialsTotal(getEventData(), material = input$selectmat21, diff_filter = input$selectdiff21)
  })
  output$datatable21 <- renderDataTable({
    rGetMaterialsTotal() %>% filter(difficulty == input$selectdiff21)
  }, options = list(pageLength = 10))
  # 2-2
  output$plot22 <- renderPlot({
    getPlotMaterialsByEvent(getEventData(), diff_filter = input$selectdiff21, material = tolower(input$selectmat21))
  })
  output$datatable22 <- renderDataTable({
    rGetMaterialsByEvent() %>% filter(difficulty == input$selectdiff21)
  }, options = list(pageLength = 10))
  
  ##############
  # PLOTS 3
  ##############
  # 3-1
  output$plot31 <- renderPlot({
    getPlotActionsTotal(rGetActionsTotal(), diff_filter = input$selectdiff31)
  })
  output$datatable31 <- renderDataTable({
    rGetActionsTotal() %>% filter(difficulty == input$selectdiff31)
  }, options = list(pageLength = 10))
  # 3-2
  output$plot32 <- renderPlot({
    getPlotActionsByLevel(rGetActionsByLevel(), diff_filter = input$selectdiff31)
  })
  output$datatable32 <- renderDataTable({
    rGetActionsByLevel() %>% filter(difficulty == input$selectdiff31)
  }, options = list(pageLength = 10))
  
  ##############
  # PLOTS 4
  ##############
  output$plot41 <- renderPlot({
    getPlotMilitaryLevelByLevel(rGetMilitaryLevelByLevel(), diff_filter = input$selectdiff41)
  })
  output$datatable41 <- renderDataTable({
    rGetMilitaryLevelByLevel() %>% filter(difficulty == input$selectdiff41)
  }, options = list(pageLength = 10))
  
  
  ##############
  # ANLAYSIS
  ##############
  output$histogram_plot <- renderPlot({
    getPlotHistogram(rGetAnalysisData())
  })
  output$survival_plot <- renderPlot({
    getPlotSurvival(rGetAnalysisData())
  })
  output$hazard_plot <- renderPlot({
    getPlotHazard(rGetAnalysisData())
  })
  output$curtate_plot <- renderPlot({
    getPlotCurtate(rGetAnalysisData())
  })
  output$histogram_text <- renderText({
    paste0("This histogram shows the overall frequency of users completing a certain amount of event sessions. Right away we can see that the largest dropoff of players ",
           "occurs after completing around 5-7 event sessions. There is also a long tail on high-end consisting of players that use this feature a lot. This is a fairly ",
           "common shape of histogram for this kind of in-game activity.")
  })
  output$survival_text <- renderText({
    survival_data <- rGetAnalysisData()
    paste0("The chart above shows the survival function for the number of event sessions completed by users. Even though it resembles the shape of the the histogram ",
           "it's important to note that these show different metrics. ",
           "Reading a survival function is fairly straightforward. As X (the number of events) increases, the propbably of a randomly selected player ",
           "having completed more event sessions than that decreases.\n",
           "Three points are marked on the chart showing the survival probabilties at those points. For example there is a ",
           round(survival_data$KMest[7]*100, digits = 2), "% probability that a randomly selected user will complete more than 7 event sessions (based on the ",
           "information currently available).")
  })
  output$hazard_text <- renderText({
    survival_data <- rGetAnalysisData()
    paste0("The above chart shows what's known as the hazard rate or force of mortality. This concept relates to the survival function in that it shows, ",
           "at what stages, players are most likely to stop playing event sessions. Initially the hazard rate increases and then decreases. This shows that early ",
           "on players are more likely to stop playing event sessions. Then the remaining players tend to continue to play event sessions with an almost constant ",
           "hazard rate.\n\n",
           "Only after ", (survival_data %>%
                             filter(n_events <= 50) %>%
                             filter(hazard == max(hazard, na.rm = TRUE)))$n_events[1], " event sessions does the hazard rate start to decrease. ",
           "At this point approximately ", round(survival_data$KMest[(survival_data %>%
                                                                        filter(n_events <= 50) %>%
                                                                        filter(hazard == max(hazard, na.rm = TRUE)))$n_events[1]]*100, digits = 2),
           "% of all players are actually still playing event sessions.")
  })
  output$curtate_text <- renderText({
    survival_data <- rGetAnalysisData()
    paste0("This chart shows what's known to actuaries as the 'Curtate Expectation of Life'. However, in this case it shows the remaining event sessions ",
           "instead of remaining years of life. The height of the line corresponds to the expected number of remaining event sessions GIVEN that a player has already ",
           "completed a certain amount. It's worth noting that the shape of this chart is roughly inverse that of the hazard function because, as the risk ",
           "of dropping out decreases, the likelihood of continuing to play events increases and thus the expected number also increases. ",
           "After completing 1 event session the expected number of remaining event sessions is ", round(survival_data$e_x[1], digits=2), " meaning that after any player ",
           "finishes the tutorial event they are expected to play that many more event sessions. ",
           "The expected number ",
           "of remaining event sessions top out at ", round(max(survival_data$e_x), digits=2), " after completing ",
           (survival_data %>% filter(e_x == max(e_x)))$n_events[1], " event sessions. So for a random player who ALREADY completed ",
           (survival_data %>% filter(e_x == max(e_x)))$n_events[1], " event sessions they will on average complete ", round(max(survival_data$e_x), digits=2),
           " more before giving up. However, in the current available data, out of the ", survival_data$cum_users[1], " users that completed the tutorial ",
           "only ", (survival_data %>% filter(e_x == max(e_x)))$cum_users[1], " remain after completing ", (survival_data %>% filter(e_x == max(e_x)))$n_events[1],
           " event sessions.")
  })
  
  
  
  # RAW DATA TABLE
  getRawData <- reactive({
    data <- getEventData() %>%
      select(user_id, session_id, corp, ContextsessionId, difficulty, quest_name, corp_level, session_start, time, completion_time, result,
             contains("special_action"), contains("earned_rare"))
    names(data) <- gsub("^.*special_action_used_", "action_", names(data))
    names(data) <- gsub("^.*earned_rare_", "earned_", names(data))
    return(data)
  })
  output$data_raw <- renderDataTable({
    getRawData()
  }, options = list(pageLength = 8))
  
  # SUMMARY DATA
  output$data_summary <- renderDataTable({
    table <- NULL
    if (input$selectdata == 1) {
      table <- rGetEventsByEvent() %>%
        select(-pos) %>% data.frame()
    } else if (input$selectdata == 2) {
      table <- rGetEventTimeByEvent() %>% data.frame()
    } else if (input$selectdata == 3) {
      table <- rGetMaterialsTotal() %>%
        select(-pos) %>% data.frame()
    } else if (input$selectdata == 4) {
      table <- rGetActionsByLevel() %>%
        select(-pos) %>% data.frame()
    } else if (input$selectdata == 5) {
      table <- rGetMilitaryLevelByLevel() %>%
                   select(difficulty, corp_level, avg_military, n_corps) %>%
                   distinct(difficulty, corp_level, avg_military, n_corps) %>%
                   group_by(difficulty) %>% arrange(corp_level) %>% data.frame()
    }
    
    if (input$selectdiff_data != "All") {
      table <- table %>% filter(difficulty == input$selectdiff_data)
    }
    return(table)
  })
  
  # PIVOT TABLE
#   output$data_pivot <- renderRpivotTable({
#     data <- getEventData() %>%
#       select(user_id, session_id, corp, ContextsessionId, difficulty, quest_name, corp_level, session_start, time, completion_time, result,
#              contains("special_action"), contains("earned_rare"))
#     names(data) <- gsub("^.*special_action_used_", "action_", names(data))
#     names(data) <- gsub("^.*earned_rare_", "earned_", names(data))
#     data <- melt(data, measure.vars = grep("earned_", names(data)), value.name = "material_amount", variable.name = "material")
#     #data <- melt(data, measure.vars = grep("action_", names(data)), value.name = "action_amount", variable.name = "action")
#     
#     rpivotTable(data)
#   })
  
  # DOWNLOAD HANDLER FOR RAW DATA
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("event_session_", gsub(" ", "_", Sys.time()), ".xlsx")
    },
    content = function(file) {    
      outwb <- createWorkbook()
      sheet <- createSheet(outwb, sheetName = "Title")
      addDataFrame(getTitle(), sheet, col.names=FALSE, row.names=FALSE, showNA=FALSE)
      autoSizeColumn(sheet,colIndex=1:2)
      sheet <- createSheet(outwb, sheetName = "RawData")
      addDataFrame(getRawData() %>% data.frame(), sheet, col.names=TRUE, row.names=FALSE, showNA=FALSE)
      sheet <- createSheet(outwb, sheetName = "WonSessions")
      addDataFrame(rGetEventsByEvent() %>%
                     select(-perc, -pos) %>% data.frame(), sheet, col.names=TRUE, row.names=FALSE, showNA=FALSE)
      sheet <- createSheet(outwb, sheetName = "SessionTime")
      addDataFrame(rGetEventTimeByEvent() %>% data.frame(), sheet, col.names=TRUE, row.names=FALSE, showNA=FALSE)
      sheet <- createSheet(outwb, sheetName = "RareMaterials")
      addDataFrame(rGetMaterialsTotal() %>%
                     select(-perc, -pos) %>% data.frame(), sheet, col.names=TRUE, row.names=FALSE, showNA=FALSE)
      sheet <- createSheet(outwb, sheetName = "SpecialActions")
      addDataFrame(rGetActionsByLevel() %>%
                     select(-perc, -pos) %>% data.frame(), sheet, col.names=TRUE, row.names=FALSE, showNA=FALSE)
      sheet <- createSheet(outwb, sheetName = "MilitaryLevel")
      addDataFrame(rGetMilitaryLevelByLevel() %>%
                     select(difficulty, corp_level, avg_military, n_corps) %>%
                     distinct(difficulty, corp_level, avg_military, n_corps) %>%
                     group_by(difficulty) %>% arrange(corp_level) %>%
                     select(difficulty, corp_level, military_level, n_corps, avg_military) %>% data.frame(),
                   sheet, col.names=TRUE, row.names=FALSE, showNA=FALSE)
      saveWorkbook(outwb, file)
    }
  )
  # Title page for download button
  getTitle <- reactive({
    title <- rbind(
      c("ANNO 6 Event Session Dashboard ","Summary Data"),
      c("",""),
      c("Created on: ",paste0(Sys.Date())),
      c("",""),
      c("Filtered By: ",""),
      c("Start Date: ",paste0(input$dates[1])),
      c("End Date: ",paste0(input$dates[2]))
    )
    title
  })
})