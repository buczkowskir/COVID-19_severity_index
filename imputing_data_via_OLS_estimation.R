
#------------------------------------------------------------------------------#

#### Deeper Cleaning of COVID-19 Severity data ####

# Ryan Buczkowski - University of Mississippi - Political Science Department

# 4-2-2020

#------------------------------------------------------------------------------#

#Loading libraries
library(tidyverse)
library(scales)
library(broom)

#Getting rid of scientific notation
options(scipen = 999)

#Importing data
read_csv('https://raw.githubusercontent.com/buczkowskir/COVID-19_severity_index/master/physicians_population_covid.csv') -> data

#Setting up data to estimate number of physicians by population
data %>% 
  mutate(num_physicians = if_else(num_physicians == 1, NA_real_, num_physicians)) -> data2

#First visual look at the relationship between population and number of primary care physicians
data2 %>% 
  filter(!is.na(num_physicians)) %>% 
  ggplot(aes(x = population, y = num_physicians)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) +
  scale_x_continuous(labels = comma)

#Looking at correlation between population and the number of physicians
data2 %>% 
  na.omit() %>% 
  select(population, num_physicians) %>% 
  cor() #Correlation Coefficient = 0.945

#Train regression model on data
reg1 <- lm(num_physicians ~ population, data = data2)
summary(reg1)

#Looking at the distribution of the errors
augment(reg1) %>% 
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 100)

#Imputing data using the paramater estimate from the OLS model
data2 %>% 
  mutate(yhat = reg1$coefficients[2] * population, #Pulling Coefficient directly from the model -- Beta = 0.0008
         num_physicians = if_else(is.na(num_physicians) == TRUE, yhat, num_physicians), #NA values for num_physician = y-hat
         num_physicians = round(num_physicians)
         
         ) -> data3

#Writing .csv file with imputed data
data3 %>% 
  write_csv('covid_physicians_population_with_imputed.csv')



