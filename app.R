library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(viridis)
library(visNetwork)
library(ggalluvial)

# quanteda_options(threads = (RcppParallel::defaultNumThreads() - 2))

##### SET UP ENVIRONMENT #####
rnd_events_sessions_indexed <- readRDS(file = "../WIP/rnd_events_sessions_indexed.RDS")
client_names <- readRDS(file = "../WIP/client_names.RDS")
tokens_obj <- readRDS(file = "../WIP/tokens_obj_collocations_seq_analysis.RDS")
vis_nodes <- readRDS(file = "../WIP/vis_nodes_collocation_collocations_seq_analysis.RDS")
vis_edges <- readRDS(file = "../WIP/vis_edges_collocation_collocations_seq_analysis.RDS")
leg_nodes <- readRDS(file = "../WIP/vis_nodes_legend.RDS")
leg_edges <- readRDS(file = "../WIP/vis_edges_legend.RDS")
p_alluvial <- readRDS(file = "../WIP/p_alluvial_all_users.RDS")
user_grid_test <- readRDS(file = "../WIP/user_grid_activity_status.RDS")
user_client_table <- readRDS(file = "../WIP/user_client_table.RDS")
##### Set Event Labels: ----
event_labs <- setNames(object = c("User Created", 
                                  "First Login", 
                                  "First Guide", 
                                  "First Project", 
                                  "First Story", 
                                  "First Activity", 
                                  "First Outcome",
                                  "First Task",
                                  "First Meeting",
                                  "First Decision"), 
                       nm = c("user_created", 
                              "first_login", 
                              "first_guide", 
                              "first_project", 
                              "first_story", 
                              "first_activity", 
                              "first_outcome",
                              "first_task",
                              "first_meeting",
                              "first_decision"))

##############
##### UI #####
##############

ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      title = "Action Counts",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            pickerInput(inputId = "selected_instances_counts", label = "Select clients to explore:", 
                        choices = client_names,
                        selected = client_names,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE), 
            materialSwitch(inputId = "facet_wrap_counts", label = "Multi Panel:", status = "danger")
          )
        ),
        mainPanel(
          plotOutput(outputId = "action_counts_output", height = "800px")
        )
      )
    ),
    tabPanel(
      title = "Action Lags",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            pickerInput(inputId = "selected_instances_lags", label = "Select clients to explore:", 
                        choices = client_names,
                        selected = client_names,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE), 
            sliderInput(inputId = "day_range_lags", "Choose number of days to look at:",
                        min = 0, max = 180, value = 90)
          )
        ),
        mainPanel(
          plotOutput(outputId = "action_lags_output", height = "800px")
        )
      )
    ),
    tabPanel(
      title = "Value Waste",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            pickerInput(inputId = "selected_instances_waste", label = "Select clients to explore:", 
                        choices = client_names,
                        selected = client_names[1:5],
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE), 
            materialSwitch(inputId = "facet_wrap_waste", 
                           label = "Multi Panel:", 
                           status = "danger")
            )
          ),
        mainPanel(
          fluidRow(style = "margin-right:auto; margin-left:auto",
                   column(width = 12, 
                          align = "center",
                          h3("Time users did not use a feature which s/he eventually ended up using")
                          )
                   ),
          fluidRow(
            column(width = 4,
                   plotOutput(outputId = "action_waste_output_mean", height = "800px")
            ),
            column(width = 4,
                   plotOutput(outputId = "action_waste_output_median", height = "800px")
                   ),
            column(width = 4,
                   plotOutput(outputId = "action_waste_output_total", height = "800px")
                   )
            )
          )
      )
    ),
    tabPanel(
      title = "Action Sequences",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            actionBttn(inputId = "load_global_seqs", label = "Learn Global Action Sequences", 
                       style = "material-flat", color = "primary")
            ),
          wellPanel(
            pickerInput(inputId = "selected_instances_seqs", label = "Select clients to explore:", 
                        choices = c("sharktower", "weare", "nbs", "hsbc", "aegon", "fil", "lbg", "talbot", "hastings", "theanalysts", "adidas"),                        
                        selected = "sharktower",
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE), 
            sliderInput(inputId = "seq_length", "Choose length of action sequence:",
                        min = 2, max = 15, value = 10),
            sliderInput(inputId = "min_count_action", "Choose minimum occurrence of individual action:",
                        min = 5, max = 100, value = 50),
            sliderInput(inputId = "min_count_collocation", "Choose minimum occurrence of action sequence:",
                        min = 0, max = 200, value = 50),
            actionBttn(inputId = "extract_collocations", label = "Learn Bespoke Action Sequences", 
                       style = "material-flat", color = "primary")
          )
        ),
        mainPanel(
          visNetworkOutput("vis_seqs", height = "800px")
        )
      )
    ),
    tabPanel(
      title = "User Transitions",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            pickerInput(inputId = "selected_instances_trans", label = "Select clients to explore:", 
                        choices = client_names,
                        selected = client_names,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE)
          )
        ),
        mainPanel(
          plotOutput(outputId = "user_transitions_output", height = "800px"),
          br(),
          DT::dataTableOutput(outputId = "user_transitions_dt")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  #########################
  ##### ACTION COUNTS #####
  #########################
  
  ##### Shape data:
  shape_data_first_actions <- reactive({
    data_first_actions <- rnd_events_sessions_indexed %>%
      select(idx_day, day, week, month, year, client_name, user_creation, starts_with("first_")) %>% 
      pivot_longer(-c(idx_day, day, week, month, year, client_name,), names_to = "event", values_to = "occurrence") %>% 
      filter(occurrence == TRUE) %>%
      mutate(event = factor(event, levels = c("user_creation", 
                                              "first_login", 
                                              "first_guide", 
                                              "first_project", 
                                              "first_story", 
                                              "first_activity", 
                                              "first_outcome",
                                              "first_task",
                                              "first_meeting",
                                              "first_decision"))) %>% 
      group_by(idx_day, day, week, month, year, client_name, event) %>% 
      summarise(total_users = n()) %>% 
      ungroup()
  })
  
  ##### Plot action counts:
  plot_action_counts <- reactive({
    p <- ggplot(shape_data_first_actions() %>%
                  filter(client_name %in% input$selected_instances_counts) %>% 
                  mutate(event = recode(event, 
                                        !!! c("user_creation" = "User Creation", 
                                              "first_login" = "Log in", 
                                              "first_guide" = "Check Guide", 
                                              "first_project" = "Create Project", 
                                              "first_story" = "Create Story", 
                                              "first_activity" = "Create Activity", 
                                              "first_outcome" = "Create Outcome",
                                              "first_task" = "Create Task",
                                              "first_meeting" = "Create Meeting",
                                              "first_decision" = "Create Decision"))) %>%
                  group_by(client_name, event) %>% 
                  summarise(total_users = sum(total_users)), 
                aes(x = event, y = total_users, fill = event)) +
      geom_bar(stat = "identity") +
      labs(title="Total Unique Users per Type of Action",
           subtitle = "Funnel-like transition across product features",
           y = "# of Users",
           x = "User Action") +
      theme_bw() + 
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::label_comma(accuracy = 1))
  
    if (input$facet_wrap_counts & length(input$selected_instances_counts) > 0) {
      p <- p +
        facet_wrap(~ client_name, scales = "free_y", ncol = 3)
      return(p)
    }
    return(p)
  })
  
  output$action_counts_output <- renderPlot({
    plot_action_counts()
  })
  
  
  #######################
  ##### ACTION LAGS #####
  #######################
  
  ##### Plot action lags:
  plot_action_lags <- reactive({
    p <- ggplot(shape_data_first_actions() %>%
           filter(event != "user_creation") %>%
           filter(client_name %in% if (length(input$selected_instances_lags) == 0) {client_names} else {input$selected_instances_lags}) %>%
           filter(idx_day <= input$day_range_lags)  %>%
           group_by(idx_day, event) %>%
           summarise(total = sum(total_users)),
         aes(x = idx_day, y = total, colour = event)) +
      geom_line() +
      labs(title="Time from User Creation to First User Action",
         subtitle = paste0("First ", input$day_range_lags, " days"),
         x ="# of Days", y = "# of Users") +
      facet_wrap(~ event, scales = "free_y", labeller = labeller(event = event_labs)) +
      theme_bw() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::label_comma(accuracy = 1))
    return(p)
  })

  
  output$action_lags_output <- renderPlot({
    plot_action_lags()
  })
  
  #######################
  ##### VALUE WASTE #####
  #######################
  
  ##### Shape data for calculating lag days statistics:
  shape_data_first_actions_kpi <- reactive({
    data_first_actions_kpi <- shape_data_first_actions() %>% 
      filter(event != "user_creation") %>% 
      left_join(shape_data_first_actions() %>% 
                  group_by(client_name) %>% 
                  summarise(global_users = sum(total_users)), 
                by = "client_name") %>% 
      mutate(lag_days_acum = idx_day * total_users,
             lag_days_partial = lag_days_acum / global_users) %>% 
      group_by(client_name, event) %>% 
      summarise(action_users = sum(total_users), 
                lag_days_total = sum(lag_days_acum), 
                lag_days_global_avg = sum(lag_days_partial),
                lag_days_action_avg = lag_days_total / action_users,
                lag_days_action_median = median(rep.int(x = idx_day, times = total_users)))
  })
  
  ##### Shape data to recode factor levels:
  shape_data_action_waste <- reactive({
    data_plot <- shape_data_first_actions_kpi() %>%
      filter(client_name %in% if (length(input$selected_instances_waste) == 0) {client_names} else {input$selected_instances_waste}) %>%
      mutate(event = recode(event, 
                            !!! c("user_creation" = "User Creation", 
                                  "first_login" = "Log in", 
                                  "first_guide" = "Check Guide", 
                                  "first_project" = "Create Project", 
                                  "first_story" = "Create Story", 
                                  "first_activity" = "Create Activity", 
                                  "first_outcome" = "Create Outcome",
                                  "first_task" = "Create Task",
                                  "first_meeting" = "Create Meeting",
                                  "first_decision" = "Create Decision")))
  })
  
  ##### Plot mean wasted days per user action type:
  plot_action_waste_mean <- reactive({
    p <- ggplot(shape_data_action_waste(), 
             aes(x = event, y = lag_days_action_avg, fill = event)) +
      geom_bar(stat = "identity") +
      labs(title = paste0("Mean # of Wasted Days",  ifelse(!input$facet_wrap_waste, " - Aggregated Clients", "")),
           #subtitle = "Average days users did not use a feature which s/he eventually ended up using.",
           y = "# of Days",
           x = "User Action") +
      #facet_wrap(~ client_name, scales = "free_y", ncol = 1) +
      theme_bw() + 
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::label_comma(accuracy = 1))
    
    if (input$facet_wrap_waste & length(input$selected_instances_counts) > 0) {
      p <- p +
        facet_wrap(~ client_name, scales = "free_y", ncol = 1)
      return(p)
    }
    return(p)
    })
  
  ##### Plot median wasted days per user action type:
  plot_action_waste_median <- reactive({
    p <- ggplot(shape_data_action_waste(), 
               aes(x = event, y = lag_days_action_median, fill = event)) +
          geom_bar(stat = "identity") +
          labs(title = paste0("Median # of Wasted Days",  ifelse(!input$facet_wrap_waste, " - Aggregated Clients", "")),
               #subtitle = "Median days users did not use a feature which s/he eventually ended up using.",
               y = "# of Days",
               x = "User Action") +
          #facet_wrap(~ client_name, scales = "free_y", ncol = 1) +
          theme_bw() + 
          theme(legend.position = "none") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_y_continuous(labels = scales::label_comma(accuracy = 1))
    
    if (input$facet_wrap_waste & length(input$selected_instances_counts) > 0) {
      p <- p +
        facet_wrap(~ client_name, scales = "free_y", ncol = 1)
      return(p)
    }
    return(p)
  })
  
  
  ##### Plot total wasted days per user action type:
  plot_action_waste_total <- reactive({
    p <- ggplot(shape_data_action_waste(), 
                aes(x = event, y = lag_days_total, fill = event)) +
      
      geom_bar(stat = "identity") +
      labs(title = paste0("Total Wasted Days",  ifelse(!input$facet_wrap_waste, " - Aggregated Clients", "")),
           #subtitle = "Total days users did not use a feature which s/he eventually ended up using.",
           y = "# of Days",
           x = "User Action") +
      #facet_wrap(~ client_name, scales = "free_y", ncol = 1) +
      theme_bw() + 
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::label_comma(accuracy = 1))
    
    if (input$facet_wrap_waste & length(input$selected_instances_counts) > 0) {
      p <- p +
        facet_wrap(~ client_name, scales = "free_y", ncol = 1)
      return(p)
    }
    return(p)
  })

  
  output$action_waste_output_mean <- renderPlot({
      plot_action_waste_mean()
  })
  
  output$action_waste_output_median <- renderPlot({
    plot_action_waste_median()
  })
  
  output$action_waste_output_total <- renderPlot({
    plot_action_waste_total()
  })
  
  
  #############################
  ##### SEQUENCE ANALYSIS #####
  #############################
  
  get_global_seqs <- eventReactive(input$load_global_seqs, {
    visNetwork(vis_nodes, vis_edges,  width = 1800, height = 1200,
               main = paste0("User Action Sequences - All Clients")) %>%
      visLegend(useGroups = FALSE, addNodes = leg_nodes, addEdges = leg_edges) %>% 
      visIgraphLayout() %>%
      visEdges(smooth = FALSE) %>%
      visExport() %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = list(enabled = TRUE))
    
  })
  
  
  # extract_bespoke_collocations <- eventReactive(input$extract_collocations, {
  #   
  #   tokens_obj_filtered <- tokens_obj %>% 
  #     tokens_subset(client_name %in% selected_instances_seqs) ##### CHECK IF IT WORKS FINE !!!!!
  #   #browser()
  #   
  #   mwe_obj <- textstat_collocations(x = tokens_obj_filtered, 
  #                                    size = c(2:input$seq_length), 
  #                                    min_count = input$min_count_action) %>%
  #     mutate(collocation = str_replace_all(string = collocation, pattern = " ", replacement = " -> ")) %>% 
  #     mutate(unique_events = collocation %>% 
  #              str_split(pattern = " -> ") %>% 
  #              lapply(., function(x) unique(x)) %>% 
  #              lapply(., function(x) length(x)) %>% 
  #              unlist()) %>% 
  #     arrange(desc(length), desc(count), count_nested, unique_events) %>% 
  #     filter(count > input$min_count_collocation)
  # })
  # 
  # 
  # build_edge_list <- reactive({
  #   
  #   temp_mwe_analysis <- extract_bespoke_collocations()
  #   
  #   events_edge_list <- tibble()
  #   for (i in 1:length(temp_mwe_analysis$collocation)) {
  #     temp_rule <- temp_mwe_analysis$collocation[i]
  #     temp_steps <- str_split(temp_rule, pattern = " -> ") %>% 
  #       unlist()
  #     temp_freq <- temp_mwe_analysis$count[i]
  #     temp_nested <- temp_mwe_analysis$count_nested[i]
  #     temp_length <- temp_mwe_analysis$length[i]
  #     temp_lambda <- temp_mwe_analysis$lambda[i]
  #     temp_ztest <- temp_mwe_analysis$z[i]
  #     temp_unique <- temp_mwe_analysis$unique_events[i]
  #     for (j in 1:length(temp_steps) - 1) {
  #       temp_from <- temp_steps[j]
  #       temp_to <- temp_steps[j + 1]
  #       temp_edge <- tibble(from = temp_from, 
  #                           to = temp_to, 
  #                           freq = temp_freq,
  #                           nested = temp_nested,
  #                           length = temp_length,
  #                           lambda = temp_lambda,
  #                           ztest = temp_ztest,
  #                           unique = temp_unique)
  #       events_edge_list <- bind_rows(events_edge_list, temp_edge)
  #     }
  #   }
  # })
  # 
  # 
  # build_graph <- reactive({
  #   
  #   events_edge_list <- build_edge_list()
  #   
  #   g <- igraph::graph_from_data_frame(events_edge_list, directed = TRUE)
  #   deg_in <- igraph::degree(g, mode = "in")
  #   deg_out <- igraph::degree(g, mode = "out")
  #   
  #   vis_nodes <- data.frame(id=igraph::V(g)$name, label=igraph::V(g)$name, stringsAsFactors = FALSE)
  #   vis_nodes$size <- if_else(igraph::V(g)$name %in% unique(events_edge_list$from), 30, 10) + log(deg_out)
  #   vis_nodes$color.background <- heat.colors(length(levels(factor(deg_out))), alpha = 0.75, rev = TRUE)[factor(deg_out)]
  #   vis_nodes$color.border <- "#E6E6E6"
  #   vis_nodes$color.highlight.background <- "orange"
  #   vis_nodes$color.highlight.border <- "darkred"
  #   
  #   vis_edges <- data.frame(from=events_edge_list$from, to=events_edge_list$to)
  #   vis_edges$arrows <- "to"
  #   
  #   visNetwork(vis_nodes, vis_edges, 
  #              main = paste0("User Action Sequences - ",  )) %>%
  #     visIgraphLayout() %>%
  #     visEdges(smooth = FALSE) %>%
  #     visExport() %>%
  #     visOptions(highlightNearest = TRUE,
  #                nodesIdSelection = list(enabled = TRUE))
  #   
  # })
  
  output$vis_seqs <- renderVisNetwork({
    get_global_seqs()
  })
  
  
  #####################################
  ##### USER ACTIVITY TRANSITIONS #####
  #####################################
  
  get_user_transition_table <- reactive({
    
    user_id_dict <- setNames(user_client_table$client_name, user_client_table$user_id)
    user_grid_test <- user_grid_test %>% 
      ungroup() %>% 
      mutate(client_name = user_id_dict[.$user_id]) %>% 
      select(client_name, user_id, year_month, activity_cohort) %>% 
      as.data.frame()
      
    return(user_grid_test)
  })
  
  
  output$user_transitions_output <- renderPlot({
    p_alluvial
  })
   
  output$user_transitions_dt <- DT::renderDT(
    get_user_transition_table() %>% as.data.frame(),
    filter = "top")
  
}

shinyApp(ui, server)