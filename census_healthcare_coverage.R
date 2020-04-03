
#------------------------------------------------------------------------------#

#### Gather more county-level data for COVID index ####

# Healthcare Coverage

# Ryan Buczkowski -- University of Mississippi -- Political Science Department

# 4-03-2020

#------------------------------------------------------------------------------#

#Loading Libraries
pacman::p_load('tidyverse', 'tidycensus', 'janitor')

#Loading variables
load_variables(year = 2018, dataset = 'acs5', cache = TRUE) -> variables

#Pulling ACS data ---------------------

#Creating vector of variables for "Family Income"
c('B27010_00') %>% 
  paste0(1:9) %>% 
  c(c('B27010_0') %>% 
      paste0(10:66)) -> var_list

map_dfr(var_list, function(x){get_acs(geography = 'county', variables = x, year = 2018)}) -> data

data %>% 
  clean_names() %>% 
  select(-name) %>% 
  rename('name' = variable) %>% 
  left_join(variables) %>% 
  filter(str_detect(name, 'B27010_001|B27010_002|B27010_018|B27010_034|B27010_051') == FALSE) %>% 
  mutate(label = str_remove(label, 'Estimate!!Total!!'),
         label = str_replace(label, '!!With one type of health insurance coverag.+', '_one_type_coverage'),
         label = str_replace(label, '!!No health insurance coverag.+', '_no_coverage'),
         label = str_replace(label, '!!With two or more types of health insurance coverag.+', '_two_type_coverage'),
         covered = case_when(str_detect(label, '_no_coverage') == TRUE ~ 'Not Covered',
                             str_detect(label, '_no_coverage') != TRUE ~ 'Covered')) %>% 
  group_by(geoid, covered) %>% 
  summarize(estimate = sum(estimate)) %>% 
  pivot_wider(names_from = covered, values_from = estimate) %>% 
  clean_names() %>% 
  mutate(pct_no_healthcare = not_covered / (covered + not_covered)) %>% 
  select(geoid, pct_no_healthcare
         
         ) -> health_coverage2

# Joining with COVID-19 index data
read_csv('https://raw.githubusercontent.com/buczkowskir/COVID-19_severity_index/master/income_covid_physicians_population.csv') %>% 
  left_join(health_coverage2) %>% 
  select(geoid, county, state, everything()) %>% 
  #Writing CSV file for combined data
  write_csv(path = 'healthcare_income_covid_etc.csv')

