
#----------------------------------------------------------------------------------#

#### Web scraping program to gather the number of physicians per country ####

#### Ryan Buczkowski -- University of Mississippi -- Political Science Department

#### 4-1-2020


#-----------------------------------------------------------------------------------#


#Loading libraries
pacman::p_load('tidyverse', 'rvest', 'janitor', 'readxl')

#Function to scrape web table -- solution by Daniel Sussman
read_dynamic_html <- function(url) { #<<
  
  os <- .Platform$OS.type
  if(os == "unix" && str_detect(.Platform$pkgType, "mac")){
    chrome <- "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
  } else if (os == "unix"){
    chrome <- "/usr/bin/google-chrome"
  } else {
    chrome <- "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"
  }
  
  quoted_url <- shQuote(url, type = "cmd")
  
  output <- system2(chrome,
                    list("--headless --dump-dom --disable-gpu", quoted_url),
                    stdout = TRUE) %>%
    str_c(collapse = "") %>% read_html()
}

# Nesting Daniel Sussman's function in a function to scrape the data
data_scrape <- function(state){
  
  url <- c('https://www.countyhealthrankings.org/app/') %>% 
    paste0(state) %>% 
    paste0('/2020/measure/factors/4/data') %>% 
    as_tibble()
  
  read_dynamic_html(url) %>% 
    html_table(fill = TRUE) %>% 
    .[[3]] %>% 
    clean_names() %>% 
    as_tibble() %>% 
    mutate(state = state) %>% 
    select(c(1,3,7)) %>% 
    rename('num_physicians' = c(2)) %>% 
    mutate(num_physicians = as.numeric(num_physicians))
  
}

# Creating a list of state names to fit into the url format
state.name %>% 
  str_to_lower() %>% 
  str_replace(' ', '-') %>% 
  as_tibble() -> state_names

# Mapping all 50 states to data frame
map_dfr(state_names$value, data_scrape) -> num_physicians_national

# Cleaning scraped data
num_physicians_national %>% 
  mutate(state = str_replace(state, '-', ' '),
         state = str_to_title(state),
         county = case_when(!is.na(borough) == TRUE ~ borough,
                            !is.na(parish)  == TRUE ~ parish,
                            TRUE ~ as.character(county)),
         num_physicians = if_else(is.na(num_physicians) == TRUE, 1, num_physicians)) %>% 
  select(state, county, num_physicians
         
  ) -> physicians_data

# Writing CSV file
physicians_data %>% 
  write_csv('num_physicians_by_county.csv')