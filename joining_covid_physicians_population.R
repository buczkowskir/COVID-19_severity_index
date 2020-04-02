# Creating dataset with Number of physicians, population, COVID-19 cases -- County-Level

# Ryan Buczkowski -- University of Mississippi -- Political Science Department

# 4-01-2020

#######################################################

# Loading Libraries
pacman::p_load('tidyverse', 'janitor', 'readxl')

### Importing data --------------------------------------------

# Number of physicians
read_csv('https://raw.githubusercontent.com/buczkowskir/COVID-19_severity_index/master/num_physicians_by_county.csv') -> physicians
# Population data
census_file <- 'https://www2.census.gov/programs-surveys/popest/tables/2010-2019/counties/totals/co-est2019-annres.xlsx'

destfile <- 'co-est2019-annres.xlsx'

curl::curl_download(census_file, destfile) #downloading file from census

read_xlsx('co-est2019-annres.xlsx', skip = 3) -> pop_data

### Cleaning Census data and joining with physician data -----------
pop_data %>% 
  clean_names() %>% 
  rename('county'     = c(1),
         'population' = x2019) %>% 
  slice(-c(1, 3144:3149)) %>% 
  select(county, population) %>% 
  separate(county, c('county', 'state'), sep = ', ') %>% 
  mutate(county = str_remove_all(county, '\\.| County')) %>% 
  left_join(physicians) %>% 
  mutate(phys_per_cap = num_physicians / population) -> phys_and_pop

### Importing and cleaning COVID-19 data
read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") -> covid_data

#filtering out by most recent date
covid_data %>% 
  filter(date == '2020-03-31') %>% 
  select(county, state, cases) %>% 
  right_join(phys_and_pop) %>% 
  mutate(cases = if_else(is.na(cases) == TRUE, 0, cases)) -> data_full

#Creating CSV file
data_full %>% 
  write_csv(path = 'physicians_population_covid.csv')


