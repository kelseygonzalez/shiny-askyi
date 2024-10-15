library(tidyverse)
library(lubridate)

set.seed(42)  # For reproducibility
n <- 1500  # Number of rows

collections = c("Access Questions", "Compensation Dashboard", "Data Definitions", "FAQ", "Headcount", "Metrics", "Reports", "Technical Problems") 

phase_1a_start = ymd("2024-06-03")
phase_1b_start = ymd("2024-07-18")
phase_1c_start = ymd("2024-08-20")




# Create a data frame with 10 rows and 3 columns of random data
df <- tibble(
    conversation_id = replicate(n, paste0(sample(c(letters, LETTERS, 0:9), 15, replace = TRUE), collapse = "")),
    conversation_date = sample(seq.Date(phase_1a_start, today(), by = "day"), n, replace = TRUE),
    source = sample(c("web", "slack"), n, replace = TRUE, prob = c(0.95, 0.05)),
    collection = sample(collections, n, replace = TRUE),
    response_time = rnorm(n, mean = 0.81, sd = .15), 
    feedback = sample(c(NA, "positive", "negative"), n, replace = TRUE, prob = c(0.95, 0.025, 0.025)),
    gen_ai_response = sample(c(FALSE, TRUE), n, replace = TRUE, prob = c(.90, 0.10))
    ) %>% 
  mutate(conversation_date = case_when((source == 'slack') &  (conversation_date < phase_1b_start) ~ sample(seq.Date(phase_1b_start, today(), by = "day"), 1, replace = TRUE), 
                                    TRUE ~ ymd(conversation_date)))

# View the data frame
print(df)

write_csv(df, 'data.csv')
