##############
# FUNCTIONS TO GENERATE SUMMARY DATA
##############
getEventTimeByEvent <- function(df.events) {
  # DATAFRAME of amount of time to complete events
  df.summary <- df.events %>%
    group_by(difficulty, result, quest_name) %>%
    summarize(avg_comp_time = mean(completion_time),
              med_comp_time = median(completion_time),
              min_comp_time = min(completion_time),
              max_comp_time = max(completion_time))
}

getEventsByEvent <- function(df.events) {
  # DATAFRAME how many of each event is played split by result
  df.summary <- df.events %>%
    group_by(difficulty, quest_name, result) %>%
    summarize(count = n()) %>%
    arrange(desc(result)) %>%
    group_by(difficulty, quest_name) %>%
    mutate(perc = round((count / sum(count))*100, digits=2),
           pos = cumsum(count) -0.5*count)
}

getMaterialsByEvent <- function(df.events) {
  # DATAFRAME distribution of the amount of rare materials earned in each event
  df.summary <- df.events %>%
    select(user_id, corp, difficulty, session_id, event_id, quest_name, ContextsessionId,
           corp_level, result, time, contains("earned"))
  names(df.summary) <- gsub("^.*earned_rare_", "", names(df.summary))
  df.summary <- melt(df.summary, measure.vars = c("earth","moon","base","polar"), value.name = "amount", variable.name = "material_source") %>%
    group_by(difficulty, quest_name, material_source) %>%
    summarize(avg_earned = mean(amount),
              med_earned = median(amount),
              min_earned = min(amount),
              max_earned = max(amount))
}

getEventsByLevel <- function(df.events) {
  # DATAFRAME number of successful events completed by different corporation levels
  df.summary <- df.events %>%
    group_by(corp_level, difficulty, result) %>%
    summarize(count = n())%>%
    group_by(difficulty, corp_level) %>%
#     right_join(level_results, by=c("corp_level","difficulty","result")) %>%
#     replace_na(list(count = 0)) %>%
    group_by(difficulty, result) %>%
    arrange(corp_level) %>%
    mutate(cum_count = cumsum(count)) %>%
    group_by(corp_level) %>% arrange(desc(result))
}

getMaterialsTotal <- function(df.events) {
  # DATAFRAME get total amount of rare materials earned from different sessions
  df.summary <- df.events %>%
    select(user_id, corp, difficulty, session_id, event_id, quest_name, ContextsessionId,
           corp_level, result, time, contains("earned"))
  names(df.summary) <- gsub("^.*earned_rare_", "", names(df.summary))
  df.summary <- melt(df.summary, measure.vars = c("earth","moon","base","polar"), value.name = "amount", variable.name = "material_source")
  df.summary <- df.summary %>%
    group_by(difficulty, material_source, quest_name) %>%
    summarize(amount = sum(amount)) %>%
    group_by(difficulty, material_source) %>% arrange(amount) %>%
    mutate(perc = round((amount / sum(amount))*100, digits=2),
           pos = cumsum(amount) -0.5*amount)
}

getActionsTotal <- function(df.events) {
  # DATAFRAME get total amount of special actions in each event session
  df.summary <- df.events %>%
    select(user_id, corp, difficulty, session_id, event_id, event_session, ContextsessionId, quest_name,
           corp_level, result, time, contains("special_action"))
  names(df.summary) <- gsub("^.*special_action_used_", "", names(df.summary))
  df.summary <- melt(df.summary, measure.vars = c("nuke","push","repair","shield","stun","support"), value.name = "amount", variable.name = "special_action")
  df.summary <- df.summary %>%
    group_by(difficulty, special_action, quest_name) %>%
    summarize(amount = sum(amount)) %>%
    group_by(difficulty, special_action) %>% arrange(amount) %>%
    mutate(perc = round((amount / sum(amount))*100, digits=2),
           pos = cumsum(amount) -0.5*amount)
}

getActionsByLevel <- function(df.events) {
  # DATAFRAME get total amount of special actions in each event session
  df.summary <- df.events %>%
    select(user_id, corp, difficulty, session_id, event_id, event_session, ContextsessionId, quest_name,
           corp_level, result, time, contains("special_action"))
  names(df.summary) <- gsub("^.*special_action_used_", "", names(df.summary))
  df.summary <- melt(df.summary, measure.vars = c("nuke","push","repair","shield","stun","support"), value.name = "amount", variable.name = "special_action")
  df.summary <- df.summary %>%
    group_by(difficulty, special_action, corp_level) %>%
    summarize(amount = sum(amount)) %>%
    group_by(difficulty, corp_level) %>% arrange(amount) %>%
    mutate(perc = round((amount / sum(amount))*100, digits=2),
           pos = cumsum(amount) -0.5*amount)
}

getMilitaryLevelByLevel <- function(df.events) {
  # DATAFRAME get military level by user level
  df.summary <- df.events %>%
    group_by(difficulty, user_id, corp) %>%
    summarize(corp_level = max(corp_level),
              military_level = max(military_level)) %>%
    group_by(difficulty, corp_level) %>%
    mutate(avg_military = mean(military_level)) %>%
    group_by(difficulty, corp_level, military_level) %>%
    mutate(n_corps = n())
}

getAvgEventSessions <- function(df.events) {
  df.summary <- df.events %>%
    group_by(difficulty, corp) %>%
    summarize(n_events = n())
}

getAnalysisData <- function(df.events) {
  finished_tutorial <- unique((df.events %>%
                                 filter(event_session == 5100403))$user_id)
  ## K-M estimate for number of event sessions
  df.summary <- df.events %>%
    filter(user_id %in% finished_tutorial) %>%
    group_by(user_id) %>%
    summarize(n_events = n()) %>%
    group_by(n_events) %>%
    summarize(n_users = n_distinct(user_id)) %>%
    ungroup() %>%
    mutate(cum_users = rev(cumsum(rev(n_users)))) %>%
    # Kaplam-meier
    mutate(KM = (cum_users-n_users)/cum_users) %>%
    mutate(KMest = cumprod(KM)) %>%
    # Hazard rates
    mutate(cum_hazard = -log(KMest, base = exp(1))) %>%
    mutate(hazard = n_users / ((lead(n_events)-n_events)*(cum_users - 0.5*n_users)) ) %>%
    # q_x
    mutate(#q_x = 1 - lead(KMest) / KMest,
           q_x = n_users / cum_users)
  # Add e_x
  dddd <- data.frame(n_events = df.summary$n_events)
  for (i in 1:length(df.summary$n_events)) {
    temp_p <- c(NULL, rep(0, i-1))
    for (j in i:length(df.summary$n_events)) {
      tmp_p <- 1
      for (k in i:j) {
        tmp_p <- tmp_p * (1 - df.summary$q_x[k])
      }
      temp_p <- c(temp_p, tmp_p)
    }
    dddd <- cbind(dddd, temp_p)
  }
  dddd <- (setNames(dddd,
                    c(paste0("n_events"), paste0("t_p_", df.summary$n_events))) %>%
             ungroup() %>%
             summarise_each(funs(sum)) %>%
             melt(id.vars = "n_events"))$value
  df.summary <- df.summary %>%
    mutate(e_x = dddd)
  return(df.summary %>% data.frame())
}

##############
# FUNCTIONS TO GENERATE CHARTS
##############

getPlotEventTimeByEvent <- function(df.events, diff_filter = "Easy") {
  # BOXPLOT distribution of amount of time to complete events
  df.summary <- df.events %>%
    filter(difficulty == diff_filter)
  ggplot(df.summary, aes(x=factor(quest_name), y=completion_time, fill=result)) +
    geom_boxplot() +
    #geom_violin(scale = "width") +
    #geom_jitter() +
    coord_cartesian(ylim = c(0, 50)) +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(title = paste0("Completion Time of Event Sessions\n",
                        "Difficulty: ", diff_filter),
         x = "Event Session GUID",
         y = "Time (minutes)")
}

getPlotEventsByEvent <- function(df.events, diff_filter = "Easy") {
  # HISTOGRAM how many of each event is played split by result
  df.summary <- df.events %>%
    filter(difficulty == diff_filter) %>%
    group_by(quest_name, result) %>%
    summarize(count = n()) %>%
    arrange(desc(result)) %>%
    group_by(quest_name) %>%
    mutate(perc = paste0(round((count / sum(count))*100, digits=2), "%"),
           pos = cumsum(count) -0.5*count)
  ggplot(df.summary, aes(x=factor(quest_name), y=count, fill=result)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = perc, y = pos)) +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(title = "Number of Event Sessions Grouped by Event",
         x = "Event Context",
         y = "Count")
}

# getPlotEventsByEvent <- function(df.events, diff_filter = "Easy") {
#   df.summary <- df.events %>%
#     filter(difficulty == diff_filter)
#   ggvis(df.summary, x = ~quest_name, y = ~count, stroke = ~result) %>% layer_bars(stack = TRUE) %>%
#     bind_shiny("plot11")
# }

getPlotMaterialsByEvent <- function(df.events, diff_filter = "Easy", material = "base") {
  # BOXPLOT distribution of the amount of rare materials earned in each event
  df.summary <- df.events %>%
    select(user_id, corp, difficulty, session_id, event_id, quest_name, ContextsessionId,
           corp_level, result, time, contains("earned"))
  names(df.summary) <- gsub("^.*earned_rare_", "", names(df.summary))
  df.summary <- melt(df.summary, measure.vars = c("earth","moon","base","polar"), value.name = "amount", variable.name = "material_source") %>%
    filter(difficulty == diff_filter)
  if (material == "all") {
    ggplot(df.summary, aes(x=material_source, y=amount, fill=result)) +
      geom_boxplot() +
      labs(title = paste0("Amount of Rare Material Earned per Event Session\n",
                          "Difficulty: ", diff_filter),
           y = "Amount",
           x = "Rare Material") +
      coord_cartesian(ylim=c(0, 200))
  } else {
    
    df.summary <- df.summary %>%
      filter(material_source == material) %>%
      group_by(quest_name) %>%
      arrange(desc(result))
    lims <- df.summary %>%
      group_by(quest_name, result) %>%
      summarize(view_max = boxplot.stats(amount)$stats[c(5)])
    lims <- c(0, max(lims$view_max))
    ggplot(df.summary, aes(x=factor(quest_name), y=amount, fill=result)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
      coord_cartesian(ylim = lims) +
      labs(title = paste0("Amount of Rare Material Earned per Event Session\n",
                          "Difficulty: ", diff_filter),
           y = paste0(toupper(substr(material, 1, 1)), substr(material, 2, str_length(material))),
           x = "Event Session")
  }
}

getPlotEventsByLevel <- function(df.events, diff_filter = "Easy", cumulative = TRUE) {
  # BARCHART number of successful events completed by different corporation levels
  level_results <- expand.grid(difficulty = c("Easy","Normal","Hard","Classic"),
                               result = c("success","failed"),
                               corp_level = c(seq(min(df.events$corp_level), max(df.events$corp_level))))
  df.summary <- df.events %>%
    group_by(corp_level, difficulty, result) %>%
    summarize(count = n())%>%
    group_by(difficulty, corp_level) %>%
    right_join(level_results, by=c("corp_level","difficulty","result")) %>%
    filter(difficulty == diff_filter) %>%
    replace_na(list(count = 0)) %>%
    group_by(difficulty, result) %>%
    arrange(corp_level) %>%
    mutate(cum_count = cumsum(count)) %>%
    group_by(corp_level) %>% arrange(desc(result))
  if (cumulative) {
    plot <- ggplot(df.summary, aes(x=factor(corp_level), y=cum_count, fill=result))
    title <- "Cumulative Number of Successful Event Sessions Played"
  } else {
    plot <- ggplot(df.summary, aes(x=factor(corp_level), y=count, fill=result))
    title <- "Number of Successful Event Sessions Played"
  }
  plot +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = title,
         x = "Corporation Level",
         y = "Count") +
    scale_x_discrete(breaks=c(seq(0, 250, by=10)))
}

getPlotMaterialsTotal <- function(df.events, material = "base", diff_filter = "Easy") {
  # BARCHART get total amount of rare materials earned from different sessions
  df.summary <- df.events %>%
    select(user_id, corp, difficulty, session_id, event_id, quest_name, ContextsessionId,
           corp_level, result, time, contains("earned"))
  names(df.summary) <- gsub("^.*earned_rare_", "", names(df.summary))
  df.summary <- melt(df.summary, measure.vars = c("earth","moon","base","polar"), value.name = "amount", variable.name = "material_source") %>%
    filter(difficulty == diff_filter)
  df.summary <- df.summary %>%
    group_by(material_source, quest_name) %>%
    summarize(amount = sum(amount)) %>%
    ungroup()
  if (material != "All") {
    df.summary <- df.summary %>% filter(material_source == tolower(material))
  }
  df.summary <- df.summary %>%
    arrange(quest_name) %>%
    mutate(session = sub(" .*", "", quest_name))
  if (material == "All") {
  df.summary <- df.summary %>%
    group_by(material_source, session) %>%
    summarize(amount = sum(amount)) %>%
    group_by(material_source) %>% arrange(amount) %>%
    mutate(perc = as.character(ifelse((amount / sum(amount))*100 <= 4,
                                      paste0(""),
                                      paste0(round((amount / sum(amount))*100, digits=2), "%"))),
           pos = cumsum(amount) -0.5*amount)
  ggplot(df.summary, aes(x=material_source, y=amount, fill=session)) +
    geom_bar(aes(reorder(material_source, amount, function(x) sum(x)*-1)),
             stat = "identity", position="stack") +
    geom_text(aes(label = perc, y = pos)) +
    guides(fill=guide_legend(title="Event Session GUID")) +
    labs(title = paste0("Total Amount of Rare Material Earned\n",
                        "Difficulty: ", diff_filter),
         y = "Amount",
         x = "Rare Material")
  } else {
    ggplot(df.summary, aes(x=factor(quest_name), y=amount, fill = session)) +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
      labs(title = paste0("Total Amount of Rare Material Earned\n",
                          "Difficulty: ", diff_filter),
           y = paste0(toupper(substr(material, 1, 1)), substr(material, 2, str_length(material))),
           x = "Event Session")
  }
}

getPlotActionsTotal <- function(df.events, diff_filter = "Easy") {
  # BARCHART get total amount of special actions in each event session
  df.summary <- df.events %>%
    filter(difficulty == diff_filter) %>%
    mutate(perc_label = ifelse(perc <= 5, NA, paste0(perc, "%")))
#   ggplot(df.summary, aes(x=special_action, y=amount, fill=factor(quest_name))) +
#     geom_bar(aes(reorder(special_action, amount, function(x) sum(x)*-1)),
#              stat = "identity", position="stack") +
#     geom_text(aes(label = perc_label, y = pos)) +
#     guides(fill=guide_legend(title="Event Session")) +
#     labs(title = paste0("Number of Special Actions Used in Event Sessions\n",
#                         "Difficulty: ", diff_filter),
#          y = "Amount",
#          x = "Special Action")
  df.summary <- df.summary %>%
    arrange(quest_name) %>%
    mutate(session = sub(" .*", "", quest_name)) %>%
    group_by(quest_name, session, special_action) %>%
    summarize(amount = sum(amount)) %>%
    group_by(quest_name) %>%
    mutate(Percentage = (amount/sum(amount))*100)
  list_colors <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a", "#e29421", "#e29421", "#f05336", "#ce472e")
  t <- max(df.summary$Percentage, na.rm = TRUE)
  
  ggplot(df.summary, aes(x=factor(quest_name), y=special_action, fill=Percentage)) +
    geom_tile(colour = "white") +
    geom_text(aes(fill = Percentage, label = round(Percentage)), size = 3) +
    scale_fill_gradientn(colours=list_colors, limits=c(0, t), breaks=round(seq(0, t, by=t/8)), guide="colourbar", space="Lab") +
    labs(title = paste0("Special Actions used by Event Session\n",
                        "Difficulty: ", diff_filter),
         x = "Event Session",
         y = "Special Action") +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))
}

getPlotActionsByLevel <- function(df.events, count = FALSE, diff_filter = "Easy") {
  # BARCHART get total amount of special actions in each event session
  df.summary <- df.events %>%
    filter(difficulty == diff_filter) %>%
    mutate(Percentage = perc)
  
  list_colors <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a", "#e29421", "#e29421", "#f05336", "#ce472e")
  t <- df.summary[, "perc"] %>% max(na.rm = TRUE)
  
  ggplot(df.summary, aes(x=corp_level, y=special_action, fill=Percentage)) +
      geom_tile(colour = "white") +
      # scale_fill_gradient(low = "#eff3ff", high = "#08519c") +
      scale_fill_gradientn(colours=list_colors, limits=c(0, t), breaks=round(seq(0, t, by=t/8)), guide="colourbar", space="Lab") +
      #geom_text(aes(fill = perc, label = round(perc)), size = 3) +
      labs(title = paste0("Special Actions Used by Corporation Level\n",
                          "Difficulty: ", diff_filter),
           y = "",
           x = "Corporation Level")
}

getPlotMilitaryLevelByLevel <- function(df.events, diff_filter = "Easy") {
  # DATAFRAME get 
  ggplot(df.events %>% filter(difficulty == diff_filter), aes(x=corp_level, y=military_level)) +
    geom_point(aes(size = n_corps, colour = n_corps)) +
    geom_line(aes(x=corp_level, y=avg_military), size = 1, colour = "royalblue") +
    coord_cartesian(ylim = c(0, max((df.events %>% filter(difficulty == diff_filter))$military_level)),
                    xlim = c(0, max((df.events %>% filter(difficulty == diff_filter))$corp_level))) +
    scale_y_continuous(labels = function (x) floor(x)) +
    labs(title = paste0("Military Level by Corporation Level\n",
                        "with Average Line"),
         x = "Corporation Level",
         y = "Military Level")
}

getPlotSurvival <- function(df_data) {
  summary_data <- df_data %>% data.frame()
  event1 <- summary_data$n_events[3]
  event2 <- summary_data$n_events[7]
  event3 <- summary_data$n_events[12]
  km1 <- summary_data$KMest[3]
  km2 <- summary_data$KMest[7]
  km3 <- summary_data$KMest[12]
  ggplot(summary_data, aes(x=n_events, y=KMest)) +
    geom_line(colour = "steelblue", size = 1.2) +
    annotate("segment", x=0, xend=event1, y=km1, yend=km1, colour = "springgreen4") +
    annotate("segment", x=event1, xend=event1, y=0, yend=km1, colour = "springgreen4") +
    annotate("text", label = paste0("Pr(X > x) = ", round(km1, digits=2), "\nEvents = ", event1),
             x=event1, y=km1,
             hjust=0, vjust=0) +
    annotate("segment", x=0, xend=event2, y=km2, yend=km2, colour = "springgreen4") +
    annotate("segment", x=event2, xend=event2, y=0, yend=km2, colour = "springgreen4") +
    annotate("text", label = paste0("Pr(X > x) = ", round(km2, digits=2), "\nEvents = ", event2),
             x=event2, y=km2,
             hjust=0, vjust=0) +
    annotate("segment", x=0, xend=event3, y=km3, yend=km3, colour = "springgreen4") +
    annotate("segment", x=event3, xend=event3, y=0, yend=km3, colour = "springgreen4") +
    annotate("text", label = paste0("Pr(X > x) = ", round(km3, digits=2), "\nEvents = ", event3),
             x=event3, y=km3,
             hjust=0, vjust=0) +
    coord_cartesian(xlim = c(0, 150)) +
    labs(title = paste0("Survival Function for Number of Event Sessions"),
         x = "x - Number of Event Sessions",
         y = "Pr(X > x)")
}

getPlotHazard <- function(df_data) {
  max_hazard <- max((df_data %>%
                       filter(n_events <= 50))$hazard, na.rm = TRUE)
  max_hazard_events <- (df_data %>%
                          filter(n_events <= 50) %>%
                          filter(hazard == max(hazard, na.rm = TRUE)))$n_events[1]
  ggplot(df_data, aes(x=n_events, y=hazard)) +
    geom_line(colour = "steelblue", size = 1) +
    annotate("segment", x=0, xend=max_hazard_events, y=max_hazard, yend=max_hazard, colour = "springgreen4") +
    annotate("segment", x=max_hazard_events, xend=max_hazard_events, y=0, yend=max_hazard, colour = "springgreen4") +
    annotate("text", label = paste0("Max hazard rate = ", round(max_hazard, digits=2), "\nEvents = ", max_hazard_events),
             x=max_hazard_events, y=max_hazard,
             hjust=0, vjust=1) +
    coord_cartesian(xlim = c(0, 150), ylim = c(0, .2)) +
    labs(title = paste0("Hazard Rate for the Number of Completed Event Sessions"),
         x = "Number of Events",
         y = "Hazard Rate")
}

getPlotCurtate <- function(df_data) {
  ggplot(df_data, aes(x=n_events, y=e_x)) +
    geom_line(colour = "steelblue", size = 1) +
    coord_cartesian(xlim=c(0, 150)) +
    labs(title = paste0("Conditional Expectation of Remaining Event Sessions"),
         x = "Number of Event Sessions",
         y = "Expected Remaining Event Sessions")
}

getPlotHistogram <- function(df_data) {
  ggplot(df_data, aes(x=n_events, y=n_users)) +
    geom_bar(stat="identity", colour = "black", fill = "steelblue") +
    coord_cartesian(xlim = c(0, 150)) +
    labs(title = paste0("Histogram of Number of Events Completed"),
         x = "Number of Event Sessions",
         y = "Number of Players")
}

getPlotAvgEvents <- function(df_data, diff_filter = "All", plot_type = "Boxplot") {
  ll <- df_data
  if (diff_filter != "All") {
    ll <- ll %>%
      filter(difficulty == diff_filter)
  }
  if (plot_type == "Boxplot") {
    plot <- ggplot(ll, aes(x=difficulty, y=n_events, fill=difficulty)) +
      geom_boxplot() +
      labs(title = paste0("Number of Event Sessions Completed per Player\n",
                          "Min: ", min(ll$n_events), "\n",
                          "Max: ", max(ll$n_events), "\n"),
           x = "Difficulty",
           y = "Number of Events") +
      coord_cartesian(ylim = c(0,50))
  } else if (plot_type == "Density"){
    plot <- ggplot(ll, aes(x=n_events, fill=difficulty)) +
      geom_density(alpha = 0.3) +
      labs(title = paste0("Number of Event Sessions Completed per Player\n",
                          "Min: ", min(ll$n_events), "\n",
                          "Max: ", max(ll$n_events), "\n"),
           x = "Number of Events",
           y = "Density") +
      coord_cartesian(xlim = c(0,50))
  } else if (plot_type == "Violin") {
    plot <- ggplot(ll, aes(x=difficulty, y=n_events, fill=difficulty)) +
      geom_violin() +
      geom_boxplot(width = 0.1) +
      labs(title = paste0("Number of Event Sessions Completed per Player\n",
                          "Min: ", min(ll$n_events), "\n",
                          "Max: ", max(ll$n_events), "\n"),
           x = "Difficulty",
           y = "Number of Events") +
      coord_cartesian(ylim = c(0,50))
  }
  return(plot)
}