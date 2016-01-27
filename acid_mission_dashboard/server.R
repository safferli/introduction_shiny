shinyServer(function(input, output, session) {
  
  df_data <- reactive({
    df_raw %>% filter(date >= input$daterange[1] & date <= input$daterange[2])
  })
  
  ### PLOTS
  
  output$plot_mission_count_by_locobj <- renderPlot({
    tmp <- df_data() %>% 
      filter(mission_type == "Random") %>%
      filter(!is.na(mission_location)) %>%
      group_by(mission_status, mission_location, mission_objective) %>%
      summarize(N = n()) %>% 
      ungroup() %>%
      spread(mission_status, N) %>%
      mutate(Unfinished = Started - Won - Lost) %>%
      gather(mission_status, N, -mission_location, -mission_objective) %>%
      mutate(N = ifelse(is.na(N) | N < 0, 0, N))
    
    t <- max(tmp$N, na.rm = TRUE)
    q <- ggplot(tmp) +
      aes(x = mission_location, y = mission_objective) + 
      geom_tile(aes(fill = N)) +
      geom_text(aes(label = N), size = 3) +
#       scale_fill_gradient(low = "white", high = "steelblue") +
      scale_fill_gradientn(colours=list_colors, limits=c(0, t), breaks=round(seq(0, t, by=t/8)/5000)*5000, guide=guide_colourbar()) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "", y = "", title = "Missions by status/location/objective") +
      facet_grid(. ~ mission_status)
    print(q)
  })
  
  output$plot_win_rate_by_assassin_class <- renderPlot({
    tmp <- df_data() %>%
      filter(mission_type == "Random") %>%
      filter(mission_status != "Started") %>%
      group_by(mission_location, mission_objective, assassin_class, mission_status) %>%
      summarize(N = n()) %>%
      spread(mission_status, N) %>%
      mutate(win_ratio = Won / (Won + Lost), N = Won + Lost, N = ifelse(is.na(N), 0, N)) %>%
      select(mission_location, mission_objective, assassin_class, N, win_ratio) %>%
      filter(N != 0) %>%
      filter(N >= input$cutoff_heatmap)
    
    q <- ggplot(tmp) +
      aes(x = mission_location, y = mission_objective) + 
      geom_tile(aes(fill = win_ratio)) +
      geom_text(aes(label = paste0(round(win_ratio, 2)*100, "%")), size = 3) +
      scale_fill_gradient2(low = "#d73027", mid = "#f7f7f7", high = "#4575b4", midpoint = input$midpoint_heatmap) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "", y = "", title = paste0("Win rate by assassin class and mission location/objective",
                                          " (midpoint = ", input$midpoint_heatmap, 
                                          ", cutoff = ", input$cutoff_heatmap,")")) +
      facet_grid(. ~ assassin_class)
    print(q)
  })

  output$plot_win_rate_by_hireling_class <- renderPlot({
    tmp <- df_data() %>%
      filter(mission_status != "Started") %>%
      filter(mission_type == "Random") %>%
      filter(!is.na(hireling_class)) %>%
      group_by(mission_location, mission_objective, hireling_class, mission_status) %>%
      summarize(N = n()) %>%
      spread(mission_status, N) %>%
      mutate(win_ratio = Won / (Won + Lost), N = Won + Lost, N = ifelse(is.na(N), 0, N)) %>%
      select(mission_location, mission_objective, hireling_class, N, win_ratio) %>%
      filter(N != 0) %>%
      filter(N >= input$cutoff_heatmap)
    
    q <- ggplot(tmp) +
      aes(x = mission_location, y = mission_objective) + 
      geom_tile(aes(fill = win_ratio)) +
      geom_text(aes(label = paste0(round(win_ratio, 2)*100, "%")), size = 3) +
      scale_fill_gradient2(low = "#d73027", mid = "#f7f7f7", high = "#4575b4", midpoint = input$midpoint_heatmap) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "", y = "", title = paste0("Win rate by hireling class and mission location/objective",
                                          " (midpoint = ", input$midpoint_heatmap, 
                                          ", cutoff = ", input$cutoff_heatmap,")")) +
      facet_grid(. ~ hireling_class)
    print(q)
  })

  ### TABLES
  
  output$data_fail <- renderDataTable({
    if (input$ignore_failid) {
      tmp <- df_data() %>% filter(mission_status != "Started") %>% filter(mission_failid != as.integer64(0))
    } else {
      tmp <- df_data() %>% filter(mission_status != "Started")
    }
    
    tmp %>%
      group_by_(.dots = input$fail_factors) %>%
      summarize(N = n()) %>% 
      group_by_(.dots = setdiff(input$fail_factors, c("mission_failid"))) %>%
      mutate(ratio = N / sum(N)) %>%
      filter(N >= input$cutoff_failid_n) %>%
      filter(ratio >= input$cutoff_failid_r)
  }, options=list(pageLength=25))

  output$data_pivot <- renderRpivotTable({
    df_data() %>%
      select(user_level, assassin_class, hireling_type, hireling_class, 
             mission_status, mission_type, mission_location, mission_objective,
             mission_deaths, mission_failid, mission_hscount, mission_playtime, 
             date) %>%
      mutate(date = as.character(date)) %>%
      rpivotTable()
  })
  
  output$data_raw <- renderDataTable({
    df_data()
  }, options=list(pageLength=25))
  
})