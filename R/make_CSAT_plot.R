library(tidyverse)
library(ggtext)
library(here)

# source("theme.R")


make_csat_plot <- function(filtered_data, start, end, colors) {
  ## User Experience - CSAT in context
  CSAT_trend_df  <-
    data %>%
    # drop_na(feedback) %>%
    filter(collection %in% input$Collection,
           !is.na(feedback),
           between(conversation_date, input$dates[1], input$dates[2]),
           source %in% str_to_lower(input$Source)) %>%
    mutate(floor = floor_date(conversation_date, "weeks", week_start = 1)) %>%
    count(floor, feedback) %>%
    group_by(floor) %>%
    mutate(CSAT = n / sum(n)) %>%
    filter(feedback != 'Negative')
  
  CSAT_trend <- ggplot() +
    #phase 1a
    geom_rect(aes(xmin = phase_1a_start, xmax = phase_1b_start - 1, ymin = 0, ymax = 1), fill = ibm_colors[4], alpha = 0.05) +
    geom_text(aes(x = phase_1a_start + ((phase_1b_start - phase_1a_start)/2), y = 1), label = 'Pilot', vjust = 1.5) +
    #phase 1b
    geom_rect(aes(xmin = phase_1b_start, xmax = phase_1c_start - 1, ymin = 0, ymax = 1), fill = ibm_colors[4], alpha = 0.1) +
    geom_text(aes(x = phase_1b_start + ((phase_1c_start - phase_1b_start)/2), y = 1), label = 'Pilot + Slackbot', vjust = 1.5) +
    #phase 1c
    geom_rect(aes(xmin = phase_1c_start, xmax = today(), ymin = 0, ymax = 1), fill = ibm_colors[4], alpha = 0.2) +
    geom_text(aes(x = phase_1c_start + ((today() - phase_1c_start)/2), y = 1), label = 'Improved GenAI', vjust = 1.5) +
    geom_line(data = CSAT_trend_df, aes(x=floor, y = CSAT), color = ibm_colors[7] ) +
    geom_point(data = CSAT_trend_df, aes(x=floor, y = CSAT), color = ibm_colors[7]) +
    geom_smooth(data = CSAT_trend_df, aes(x=floor, y = CSAT), fill = ibm_colors[8], color = ibm_colors[3] ) +
    scale_fill_manual(values = ibm_colors[c(1,4,5)]) +
    scale_y_continuous(labels = label_percent(), expand = c(0,0)) +
    scale_x_date(expand = c(0,0)) +
    coord_cartesian(ylim = c(0,1), xlim = c(floor_date(input$dates[1], 'month'), today())) +
    theme(ibm_theme) + 
    labs(x=NULL, fill=NULL, y = NULL,
         title = "Positive Feedback Trend",
         subtitle = "Weekly Average CSAT score, LOESS Smoothing")
  
  if (nrow(data) == 0) {
    p + annotate("text", x = I(0.5), y = I(0.5), label = "No data available for these dates")
  } else {
    p
  }
}

