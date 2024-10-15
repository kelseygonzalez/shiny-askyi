# about page w/ kpis 
# logs usage page 
# logs feedback page

#TODO 
# [X] fix page 1 plot
# [X] fix page 2 plot
# [X] fix dates not being selected
# [X] fix colors flipping when selection changes
# [X] add info cards
# [X] add info to info cards
# [X] publish to shiny connect

#nice to have 
# [] add askyi logo
# [] get transparent IBM logo
# [] make top bar colorful on click
# [] 


# Setup -------------------------------------------------------------------
pacman::p_load(shiny, tidyverse, ggplot2, lubridate, tidytext, scales, ggtext, 
               glue, gt, here, bslib, shiny, bsicons, shinyWidgets, plotly)

# Read in data and join with collection info
logs_df <- read_csv('data.csv')
collections <- logs_df %>% 
  distinct(collection) %>% 
  pull(collection)

phase_1a_start = ymd("2024-06-03")
phase_1b_start = ymd("2024-07-18")
phase_1c_start = ymd("2024-08-20")

source(here("R/theme.R"))


csat_average <- logs_df %>% 
  count(feedback) %>%
  drop_na() %>%
  pivot_wider(names_from=feedback, values_from = n) %>%
  mutate(CSAT = scales::percent(positive / (positive+negative))) %>% 
  pull(CSAT)

csat_peak <- logs_df %>% 
  select(conversation_date, feedback) %>% 
  drop_na() %>% 
  mutate(floor = floor_date(conversation_date, "weeks", week_start = 1)) %>%
  count(floor, feedback) %>%
  group_by(floor) %>%
  pivot_wider(names_from=feedback, values_from = n) %>%
  mutate(CSAT = positive / (positive+negative)) %>% 
  ungroup() %>% 
  slice_max(order_by = CSAT, n=1, with_ties = FALSE) %>% 
  mutate(CSAT = scales::percent(CSAT)) %>% 
  select(date = floor, CSAT) 

interact_per_week <- 
  logs_df %>% 
  mutate(floor = floor_date(conversation_date, "weeks", week_start = 1)) %>%
  count(floor) %>% 
  summarize(interact_per_week = round(mean(n), 2)) %>% 
  pull(interact_per_week)

convo_per_week <- 
  logs_df %>%
  select(conversation_id, conversation_date) %>% 
  distinct() %>% 
  mutate(floor = floor_date(conversation_date, "weeks", week_start = 1)) %>%
  count(floor) %>%
  summarize(convo_per_week = round(mean(n), 2)) %>% 
  pull(convo_per_week)

genai_per_week <- 
  logs_df %>%
  select(conversation_date, gen_ai_response) %>% 
  drop_na() %>% 
  mutate(floor = floor_date(conversation_date, "weeks", week_start = 1)) %>%
  count(floor, gen_ai_response) %>%
  filter(gen_ai_response!= FALSE) %>% 
  summarize(genai_rate = round(mean(n)), 2) %>% 
  pull(genai_rate)



# UI ----------------------------------------------------------------------
ui <- page_navbar(
  includeCSS("custom.css"),
  theme = bs_theme(
    bootswatch = 'yeti',
    primary = "#1D4CDB",
    secondary = "#262626", 
    success = "#FA2AE6", 
    info = "#9C46FF",
    warning = "#00A1FF",
    danger = '#67C6FF',
    base_font = font_google('IBM Plex Sans Condensed'),
    heading_font = font_google('IBM Plex Sans'), 
    font_scale = .8,
    # Make everything a little tighter together
    `card-cap-padding-y` = "0.2rem", #padding around contents of card_header()
    `card-spacer-y` = "0.5rem", #padding around contents of card
    spacer = "0.7rem" #spacing between cards
  ),
  
  title = "AskYI Performance Metrics", 
  id = "navbar",
  # fillable = FALSE, # make scrollable.  Try with and without this
  sidebar = sidebar(
    id = "sidebar",
    open = FALSE, #sidebar initially closed on landing page
    # This could be a value_box instead of just plain text
    paste("Data last updated ", 
          format(max(logs_df$conversation_date , na.rm = TRUE),
                 "%Y/%m/%d %H:%M")),
    checkboxGroupInput(
      inputId = "source",
      label = "source",
      choices = unique(logs_df$source),
      selected = unique(logs_df$source)
    ),
    checkboxGroupInput(
      inputId = "collection",
      label = "collection",
      choices = unique(logs_df$collection),
      selected = unique(logs_df$collection)
    ),
    airDatepickerInput(
      inputId = "dates",
      label = "Date Range",
      range = TRUE,
      # Default date range
      value = c(phase_1a_start, today()),
      dateFormat = "MM/dd/yy",
      maxDate = today(),
      minDate = phase_1a_start,
      addon = "none",
      # update_on = "close"
    )
  ),
  nav_panel(
    "About",
    div(
      img(src = "bannerimage.png", align = "center", style = "width: 100%"),
      div(
        h2("AskYI"),
        h3("Dummy KPI and Performance Metrics"),
        class = "centered"
      ),
      class = "container"
    ),
    card(
      includeMarkdown("about.md"),
      fill = FALSE #change this to TRUE to have the row of value boxes "frozen" to the top
    )
  ),
  nav_panel(
    "Usage",
    card(
      full_screen = TRUE,
      plotlyOutput("usage_plot")
    ),
  ),
  nav_panel(
    "CSAT",
    card(
      full_screen = TRUE,
      plotlyOutput("UX_plot")
    ),
  ),
  
  layout_column_wrap(
    width = 1/4,
    heights_equal = "all",
    fill = FALSE,
    value_box(
      title = "CSAT",
      value = textOutput("CSATBox"),
      showcase = plotlyOutput("CSAT_sparkline"),
      p(glue("Average {csat_average}")),
      p(glue("Peaked at {csat_peak$CSAT}  week of {format(csat_peak$date, '%B %d')}")),
      full_screen = TRUE
      
    ),

    value_box(
      title = "Interactions",
      value = textOutput("InteractionsBox"),
      showcase = plotlyOutput("interactions_sparkline"),
      p(glue("{interact_per_week} average interactions/week")),
      full_screen = TRUE
    ),

    value_box(
      title = "Conversations",
      value = textOutput("ConversationsBox"),
      showcase = plotlyOutput("conversations_sparkline"),
      p(glue("{convo_per_week} average interactions/week")),
      full_screen = TRUE
      ),

    value_box(
      title = "Generative AI",
      value = textOutput("GenAIBox"),
      showcase = plotlyOutput("genai_sparkline"),
      p(glue("GenAI triggered about {genai_per_week} times/week")),
      full_screen = TRUE

    ),
  ),
  img(src='logo.png', height ='50px', width ='50px', style="position: fixed; bottom: 24px; right: 24px; z-index: 7"),
  # tags$head(
  #   tags$style(HTML("
  #     .navbar-default {
  #       background-color: #3498db; /* Change this to your preferred color */
  #       border-color: #2980b9; /* Optional: border color */
  #     }
  #     .navbar-default .navbar-brand {
  #       color: #ffffff; /* Text color for brand name */
  #     }
  #     .navbar-default .navbar-nav > li > a {
  #       color: #ffffff; /* Text color for links */
  #     }
  #   "))
  # )
  )



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # bs_themer() #temporary! Remove before deploying
  
  
  ## Open sidebar on other tabs --------------------------------------------
  
  # sidebar starts hidden, but this opens it when you switch to any tab other
  # than "About"
  observe({
    sidebar_toggle(
      id = "sidebar",
      open = input$navbar != "About"
    )
  })  
  
  
  ## Get filtered data -----------------------------------------------------
  data_filtered <- reactive({
    logs_df %>% 
      filter(collection %in% input$collection) %>%  
      filter(between(conversation_date, ymd(input$dates[1]), input$dates[2])) %>% 
      filter(source %in% input$source)
  })
  
  
  output$selectedText <- renderText({
    paste("You selected:", input$dates)
  })
  ## Legend -------
  #can't re-use output objects, so make one for each tab
  # output$legend1 <- output$legend2 <- output$legend3 <- renderUI({
  #   make_legend(input$collection)
  # })
  # 
  ## Plots --------
  output$usage_plot <- renderPlotly({
  #   make_usage_plot(data_filtered(), start = input$dates[1], end = input$dates[2], colors = ibm_colors[c(1,4)])
      ggplotly(data_filtered() %>%
                 select(conversation_id, source, conversation_date) %>%
                 mutate(type = 'Conversation') %>%
                 distinct() %>%
                 group_by(conversation_date, source) %>%
                 count() %>%
                 ungroup() %>%
                 ggplot(aes(x=conversation_date, y = n, fill = source,
                            text = glue::glue('{format(conversation_date, "%B %d")} had {n} conversations via {source}'))) +
                 geom_col() +
                 theme(ibm_theme) +
                 scale_fill_manual(values = c("slack"=ibm_colors[1],
                                       "web"=ibm_colors[4])) +
                 labs(title = glue("Total Conversations with AskYI, {input$dates[1]} to {input$dates[2]}"),
                      y = "Conversations",
                      x = NULL,
                      color=NULL,
                      shape = NULL),
               tooltip =  'text')
    })

  CSAT_trend_df  <- reactive({
    data_filtered() %>%
    select(conversation_date, feedback) %>% 
    drop_na() %>% 
    mutate(floor = floor_date(conversation_date, "weeks", week_start = 1)) %>%
    count(floor, feedback) %>%
    group_by(floor) %>%
    mutate(CSAT = n / sum(n)) %>%
    filter(feedback != 'Negative') 
  })
  
  output$UX_plot <- renderPlotly({
    # make_csat_plot(data_filtered(), start = input$dates[1], end = input$dates[2], colors = ibm_colors[c(1,4)])
    ggplotly(
      ggplot() +
        #phase 1a
        geom_rect(aes(xmin = phase_1a_start, xmax = phase_1b_start - 1, ymin = 0, ymax = 1), fill = ibm_colors[4], alpha = 0.05) +
        geom_text(aes(x = phase_1a_start + ((phase_1b_start - phase_1a_start)/2), y = .95), label = 'Pilot', vjust = 1.5) +
        #phase 1b
        geom_rect(aes(xmin = phase_1b_start, xmax = phase_1c_start - 1, ymin = 0, ymax = 1), fill = ibm_colors[4], alpha = 0.1) +
        geom_text(aes(x = phase_1b_start + ((phase_1c_start - phase_1b_start)/2), y = .95), label = 'Pilot + Slackbot', vjust = 1.5) +
        #phase 1c
        geom_rect(aes(xmin = phase_1c_start, xmax = today(), ymin = 0, ymax = 1), fill = ibm_colors[4], alpha = 0.2) +
        geom_text(aes(x = phase_1c_start + ((today() - phase_1c_start)/2), y = .95), label = 'Improved GenAI', vjust = 1.5) +
        #actual plot
        geom_smooth(data = CSAT_trend_df(), aes(x=floor, y = CSAT), fill = ibm_colors[8], color = ibm_colors[3], alpha = 0.3) +
        
        geom_line(data = CSAT_trend_df(), aes(x=floor, y = CSAT), color = ibm_colors[7] ) +
        geom_point(data = CSAT_trend_df(), aes(x=floor, y = CSAT, text = glue('Week of {format(floor, "%m/%d")} CSAT {round(CSAT, 2)}')), color = ibm_colors[7]) +
        
        scale_fill_manual(values = ibm_colors[c(1,4,5)]) +
        scale_y_continuous(labels = label_percent(), expand = c(0,0)) +
        scale_x_date(expand = c(0,0)) +
        coord_cartesian(ylim = c(0,1), xlim = c(floor_date(input$dates[1], 'month'), today())) +
        theme(ibm_theme) + 
        labs(x=NULL, fill=NULL, y = NULL,
             title = "Positive Feedback Trend",
             subtitle = "Weekly Average CSAT score, LOESS Smoothing"),
      tooltip = 'text')
      
  })
  
  ## Info Boxes --------
  
  output$CSATBox <- renderText({
    data_filtered() %>% 
      count(feedback) %>%
      drop_na() %>%
      pivot_wider(names_from=feedback, values_from = n) %>%
      mutate(CSAT = scales::percent(positive / (positive+negative))) %>% 
      pull(CSAT)
  })
  
  output$CSAT_sparkline <- renderPlotly({
    CSAT_trend_df() %>% 
      select(floor, CSAT) %>% 
      plot_ly() %>%
      add_lines(
        x = ~floor, y = ~CSAT,
        color = I("#FA2AE6"), span = I(1),
        fill = 'tozeroy', alpha = 0.2
      ) %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "#FA2AE6"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
    htmlwidgets::onRender(
      "function(el) {
      el.closest('.bslib-value-box')
        .addEventListener('bslib.card', function(ev) {
          Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
        })
    }"
    )
  })
  
  ### 
  
  output$InteractionsBox <- renderText({
    data_filtered() %>% 
      nrow() %>%
      scales::comma()
  })
  
  output$interactions_sparkline <- renderPlotly({
    data_filtered() %>% 
    mutate(floor = floor_date(conversation_date, "weeks", week_start = 1)) %>%
    count(floor) %>% 
    plot_ly() %>%
      add_lines(
        x = ~floor, y = ~n,
        color = I("#FA2AE6"), span = I(1),
        fill = 'tozeroy', alpha = 0.2
      ) %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "#FA2AE6"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
      el.closest('.bslib-value-box')
        .addEventListener('bslib.card', function(ev) {
          Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
        })
    }"
      )
  })
  
  ### 
  
  output$ConversationsBox <- renderText({
    data_filtered() %>% 
      count(conversation_id) %>%
      nrow() %>%
      scales::comma()
  })
  
  
  output$conversations_sparkline <- renderPlotly({
    data_filtered() %>% 
      select(conversation_id, conversation_date) %>% 
      distinct() %>% 
      mutate(floor = floor_date(conversation_date, "weeks", week_start = 1)) %>%
      count(floor) %>% 
      plot_ly() %>%
      add_lines(
        x = ~floor, y = ~n,
        color = I("#9C46FF"), span = I(1),
        fill = 'tozeroy', alpha = 0.2
      ) %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "#9C46FF"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
      el.closest('.bslib-value-box')
        .addEventListener('bslib.card', function(ev) {
          Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
        })
    }"
      )
  })
  

  ### 

  output$GenAIBox <- renderText({
    data_filtered() %>% 
      count(gen_ai_response) %>%
      filter(gen_ai_response==TRUE)%>% 
      pull(n)
    })
  
  output$genai_sparkline <- renderPlotly({
    data_filtered() %>% 
      select(conversation_date, gen_ai_response) %>% 
      drop_na() %>% 
      mutate(floor = floor_date(conversation_date, "weeks", week_start = 1)) %>%
      count(floor, gen_ai_response) %>%
      filter(gen_ai_response!= FALSE) %>% 
      plot_ly() %>%
      add_lines(
        x = ~floor, y = ~n,
        color = I("#00A1FF"), span = I(1),
        fill = 'tozeroy', alpha = 0.2
      ) %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "#00A1FF"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
      el.closest('.bslib-value-box')
        .addEventListener('bslib.card', function(ev) {
          Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
        })
    }"
      )
  })
  
  
}

shinyApp(ui, server)