
#-------------------------------------------------------------------------------------#

#### Rudimentary Mapping of COVID-19 data geographically ####

# Ryan Buczkowski -- University of Mississippi -- Political Science Department

# 4-03-2020

#-------------------------------------------------------------------------------------#

#Loading Libraries
pacman::p_load('tidyverse', 'broom', 'scales', 'extrafont', 'maps', 'ggmap')

#Importing data
read_csv('https://raw.githubusercontent.com/buczkowskir/COVID-19_severity_index/master/healthcare_income_covid_etc.csv') -> data

#Mapping data
map_data(map = 'county') %>% 
  as_tibble() %>% 
  rename('state'  = region,
         'county' = subregion) %>% 
  mutate(state  = str_to_title(state),
         county = str_to_title(county)) %>% 
  left_join(data
            
            ) -> data2


#Mapping home state of Michigan
data2 %>% 
  filter(state == 'Michigan') %>% 
  mutate(covid_per_1000 = (cases / population) * 1000) %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = 'black', aes(fill = covid_per_1000)) +
  coord_fixed(1.3) +
  scale_fill_distiller(palette = 'YlOrRd', direction = 1, name = 'Cases Per 1000 People') +
  labs(y        = 'Latitude',
       x        = 'Longitude',
       title    = 'COVID-19 Cases by County',
       subtitle = 'Number of COVID-19 cases divided by population (in thousands) | Gray indicates "Missing data"',
       caption  = 'Map created using ggplot2 in RStudio \nSource: John Hopkins University \nCreator: Ryan Buczkowski - University of Mississippi - Political Science Department') +
  theme_minimal() +
  theme(axis.title      = element_text(face  = 'bold.italic',
                                       size  = 12),
        axis.text       = element_text(face  = 'bold'),
        legend.position = 'bottom',
        legend.text     = element_text(face  = 'bold.italic',
                                       size  = 8),
        legend.title    = element_text(face  = 'bold',
                                       size  = '12'),
        plot.title      = element_text(size  = 18,
                                       face  = 'bold'),
        plot.subtitle   = element_text(face  = 'italic',
                                       size  = 8),
        plot.caption    = element_text(hjust = 0,
                                       size  = 8,
                                       face  = 'italic'))
