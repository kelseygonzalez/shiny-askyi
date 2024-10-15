library(tidyverse)
library(ggtext)
library(here)
# source("theme.R")
# source(here("app/R/theme.R"))


make_usage_plot <- function(filtered_data, start, end, colors) {
   p <- ggplotly(filtered_data %>%
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
                   scale_fill_manual(values = colors) +
                   labs(title = glue("Total Conversations with AskYourInsight, {filtered_data} to {end}"),
                        y = "Conversations",
                        x = NULL,
                        color=NULL,
                        shape = NULL),
                 tooltip =  'text')
     
  # if (nrow(filtered_data) == 0) {
  #   p + annotate("text", x = I(0.5), y = I(0.5), label = "No data available for these dates")
  # } else {
  #   p
  # }
   p
}

