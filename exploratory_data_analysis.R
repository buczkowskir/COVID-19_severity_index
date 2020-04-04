
#-------------------------------------------------------------------------------#

#### Exploratory Data Analysis of COVID-19 Severity data ####

# Ryan Buczkowski -- University of Mississippi -- Political Science Department

# 4-03-2020

#-------------------------------------------------------------------------------#

#Loading Libraries
pacman::p_load('tidyverse', 'broom', 'scales', 'extrafont', 'maps', 'ggmap')

#Importing data
read_csv('https://raw.githubusercontent.com/buczkowskir/COVID-19_severity_index/master/healthcare_income_covid_etc.csv') -> data

#Using built-in data for state abbreviation, and state region

#State identifiers -- Changing Delaware and Maryland to "Northeast" region
tibble(state = state.name, abb = state.abb, region = state.region) %>% 
  mutate(region = case_when(str_detect(state, 'Delaware|Maryland') == TRUE ~ 'Northeast',
                            TRUE ~ as.character(region))) %>% 
  right_join(data
             
             ) -> data2

#operationalizing some different measures based on existing data
data2 %>% 
  mutate(phys_per_1000  = (num_physicians / population) * 1000,
         covid_per_1000 = (cases / population) * 1000,
         covid_stress   = cases / num_physicians
         
         ) -> data3

#visualizing variables by state -- grouped by region

# Ratio of physicians to population
data3 %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(x = reorder(state, phys_per_1000), y = phys_per_1000)) +
  geom_boxplot(aes(fill = region)) + 
  ylim(c(0.1,1.5)) +
  scale_fill_brewer(palette = 'Spectral', guide = FALSE) +
  facet_wrap(~region, scales = 'free_x') +
  labs(y        = 'Ratio of Physicians (per 1000 people)',
       x        = 'State',
       title    = 'Ratio of Physicians to Population - State',
       subtitle = 'Ratio created using number of "Primary Care Physicians" of county divided by Population of county',
       caption  = 'Visualization Created Using ggplot2 in RStudio \nSources: United States Census Bureau & https://www.countyhealthrankings.org/ \nCreator: Ryan Buczkowski - University of Mississippi - Political Science Department') +
  theme_minimal() +
  theme(axis.text.x   = element_text(face  = 'bold',
                                     hjust = 1,
                                     vjust = 0.5,
                                     angle = 90,
                                     size  = 10,
                                     color = 'black'),
        axis.title    = element_text(face  = 'bold',
                                     size  = 14), 
        plot.title    = element_text(size  = 18,
                                     face  = 'bold'),
        plot.subtitle = element_text(face  = 'italic',
                                     size  = 8),
        plot.caption  = element_text(hjust = 0,
                                     size  = 8,
                                     face  = 'italic'),
        strip.text    = element_text(size  = 14,
                                     face  = 'bold.italic'))

# Percent of population without healthcare coverage
data3 %>% 
  group_by(state, region) %>% 
  filter(!is.na(region)) %>% 
  summarize(median_uncovered = median(pct_no_healthcare)) %>% 
  ggplot(aes(x = reorder(state, median_uncovered), y = median_uncovered)) +
  geom_col(aes(fill = region), color = 'black') + 
  scale_fill_brewer(palette = 'Spectral', name = 'Region') +
  scale_y_continuous(labels = percent) +
  labs(y        = 'Percent without Health Insurance Coverage \n (Median value)',
       x        = 'State',
       title    = 'Median Percentage of Population without Health Insurance Coverage - State',
       subtitle = 'Median value based on percentage of uninsured across counties',
       caption  = 'Visualization Created Using ggplot2 in RStudio \nSources: United States Census Bureau & https://www.countyhealthrankings.org/ \nCreator: Ryan Buczkowski - University of Mississippi - Political Science Department') +
  theme_minimal() +
  theme(axis.text.x       = element_text(face  = 'bold',
                                         hjust = 1,
                                         vjust = 0.5,
                                         angle = 90,
                                         size  = 10,
                                         color = 'black'),
        axis.text.y       = element_text(face  = 'bold.italic',
                                         color = 'black',
                                         size  = 12),
        axis.title        = element_text(face  = 'bold',
                                         size  = 14), 
        plot.title        = element_text(size  = 16,
                                         face  = 'bold'),
        plot.subtitle     = element_text(face  = 'italic',
                                         size  = 8),
        plot.caption      = element_text(hjust = 0,
                                         size  = 8,
                                         face  = 'italic'),
        strip.text        = element_text(size  = 14,
                                         face  = 'bold.italic'),
        legend.position   = 'bottom',
        legend.text       = element_text(face  = 'bold.italic',
                                         size  = 11),
        legend.title      = element_text(face  = 'bold',
                                         size  = 14),
        legend.background = element_rect(color = 'black',
                                         size  = 1))

# Percentage of people below the median income
data3 %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(x = reorder(state, pct_below_median_income), y = pct_below_median_income)) +
  geom_boxplot(aes(fill = region)) + 
  scale_fill_brewer(palette = 'Spectral', name = 'Region') +
  scale_y_continuous(labels = percent) +
  labs(y        = 'Population Below National Median Income (%)',
       x        = 'State',
       title    = 'Population Making less than National Median Income',
       subtitle = 'Percentage calulated as the "sum of the population included in Census income brackets of $60,000 or less" divided by the total population - by county',
       caption  = 'Visualization Created Using ggplot2 in RStudio \nSource: United States Census Bureau \nCreator: Ryan Buczkowski - University of Mississippi - Political Science Department') +
  theme_minimal() +
  theme(axis.text.x       = element_text(face  = 'bold',
                                         hjust = 1,
                                         vjust = 0.5,
                                         angle = 90,
                                         size  = 10,
                                         color = 'black'),
        axis.text.y       = element_text(face  = 'bold.italic',
                                         size  = 12,
                                         color = 'black'),
        axis.title        = element_text(face  = 'bold',
                                         size  = 14), 
        plot.title        = element_text(size  = 18,
                                         face  = 'bold'),
        plot.subtitle     = element_text(face  = 'italic',
                                         size  = 7),
        plot.caption      = element_text(hjust = 0,
                                         size  = 8,
                                         face  = 'italic'),
        strip.text        = element_text(size  = 14,
                                         face  = 'bold.italic'),
        legend.position   = 'bottom',
        legend.text       = element_text(face  = 'bold.italic',
                                         size  = 11),
        legend.title      = element_text(face  = 'bold',
                                         size  = 14),
        legend.background = element_rect(color = 'black',
                                         size  = 1))


