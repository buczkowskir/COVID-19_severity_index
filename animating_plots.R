

library(tidyverse)
library(gganimate)

read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") -> covid_data


#Creating a vector of 50 state names to filter by
state_names <- state.name %>% 
  paste0(collapse = '|')


#Summarizing cases by date
covid_data %>% 
  filter(str_detect(state, state_names) == TRUE) %>% 
  group_by(state, date) %>%
  summarize(cases  = sum(cases),
            deaths = sum(deaths)) -> state_data

#static plot
state_data %>% 
  ggplot(aes(x = reorder(state, cases), y = cases)) +
  geom_col(aes(fill = state)) +
  scale_fill_viridis_d(guide = FALSE) +
  labs(x = 'State',
       y = 'Number of Cases') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = .5)) -> plot

#animated plot
plot +
  transition_states(states = date,
                    transition_length = 2,
                    state_length = 1)
