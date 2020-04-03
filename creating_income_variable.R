
#------------------------------------------------------------------------------#

#### Gather more county-level data for COVID index ####

# Ryan Buczkowski -- University of Mississippi -- Political Science Department

# 4-02-2020

#------------------------------------------------------------------------------#

#Loading Libraries
pacman::p_load('tidyverse', 'tidycensus', 'janitor')

#Loading variables
load_variables(year = 2018, dataset = 'acs5', cache = TRUE) -> variables

variables %>% 
  write_csv('census_acs5_variables.csv')

#Pulling ACS data ---------------------

#Creating vector of variables for "Family Income"
c('B19101_00') %>% 
  paste0(2:9) %>% 
  c(c('B19101_0') %>%
  paste0(10:17)
  
  ) -> var_list


#Using purrr to map function
map_dfr(var_list, function(x){get_acs(geography = 'county', variables = x, year = 2018)}) -> income_data

#Preparing and cleaning data
income_data %>% 
  clean_names() %>% 
  rename('county' = name,
         'name'   = variable) %>% 
  left_join(variables) %>% 
  select(-moe, -concept, -name) %>% 
  separate(county, c('county', 'state'), sep = ', ') %>% 
  mutate(label  = str_remove(label, '.+\\!\\!'),
         county = str_remove(county, ' County'),
         label  = str_replace(label, 'Less than', 'under'),
         label  = str_replace_all(label, '.+to\\s', 'under'),
         label  = str_remove(label, ',')) %>% 
  pivot_wider(names_from = label, values_from = estimate) %>% 
  clean_names(
    
    
  ) -> income_level_data #Final data object here creates a wide data frame which has an estimate for the population in each income bracket
  
  
#Calculating percent of population under the US median household income for 2018 -- $63,179 -- county-level 

### To capture this the data only gives us the population which makes under $59,999 so that will make it the threshold
income_level_data %>% 
  mutate(#First calculating the sum of the estimated population surveyed
         total                   = under_10000 + under_14999 + under_19999 + under_24999 +
                                   under_29999 + under_34999 + under_39999 + under_44999 + 
                                   under_49999 + under_59999 + under_74999 + under_99999 + 
                                   under_124999 + under_149999 + under_199999 + x200000_or_more,
         #Calculating a sum of the population under the median income or $60,000
         below_median            = under_10000 + under_14999 + under_19999 + under_24999 +
                                   under_29999 + under_34999 + under_39999 + under_44999 + 
                                   under_49999 + under_59999,
         #Calculating ratio of population under the median income or $60,000
         pct_below_median_income = below_median / total,
         #Removing certain strings to avoid future joining issues
         county                  = str_remove(county, ' Parish'),
         county                  = str_replace(county, 'Ste\\. ', 'St\\. '),
         county                  = str_replace(county, 'city', 'City'),
         #Fixing some specific character issues with county names
         county                  = case_when(geoid == 35013 ~ 'Doña Ana',
                                             TRUE ~ as.character(county))) %>% 
  select(geoid, county, state, pct_below_median_income
         
         ) -> below_median_income_data

#Joining income data with COVID-19 severity index data
read_csv('https://raw.githubusercontent.com/buczkowskir/COVID-19_severity_index/master/covid_physicians_population_with_imputed.csv') %>% 
  select(-yhat) %>% 
  left_join(below_median_income_data) -> joined_data

#Writing .csv file with income data
joined_data %>% 
  write_csv(path = 'income_covid_physicians_population.csv')



        