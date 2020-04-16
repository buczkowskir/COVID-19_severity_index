

#Estimating the number of doctors by county to impute missing data


#Loading libraries
pacman::p_load('tidyverse', 'broom')

#Importing data

#data with missing values
read_csv('https://raw.githubusercontent.com/buczkowskir/COVID-19_severity_index/master/physicians_population_covid.csv') -> data_w_NA
#data with incorrectly imputed values
read_csv('https://raw.githubusercontent.com/buczkowskir/COVID-19_severity_index/master/healthcare_income_covid_etc.csv') -> data

#Joining updated data with 
data_w_NA %>% 
  select(county, state, num_physicians) %>% 
  rename('phys_w_missing' = num_physicians) %>% 
  mutate(phys_w_missing = if_else(phys_w_missing == 1, NA_real_, phys_w_missing)) %>% 
  inner_join(data) -> data2

#histograms of variables
hist(data2$phys_w_missing)          #heavy postitive skew
hist(data2$population)              #heavy postitive skew
hist(data2$pct_below_median_income) #normally distributed
hist(data2$pct_no_healthcare)       #moderate positive skew

#correlation matrix of variables
data2 %>% 
  select(phys_w_missing, population, pct_below_median_income, pct_no_healthcare) %>% 
  na.omit() %>% 
  cor()

#adding polynomials for population 
data2 %>% 
  mutate(population2 = population ^ 2,
         population3 = population ^ 3) -> data3


#Creating models to estimate parameter for population

#Poisson models - unconstrained
pos1 <- glm(phys_w_missing ~ population,
            data   = data3,
            family = 'poisson')

pos2 <- glm(phys_w_missing ~ population + population2,
            data   = data3,
            family = 'poisson')

pos3 <- glm(phys_w_missing ~ population + pct_below_median_income,
            data   = data3,
            family = 'poisson')

pos4 <- glm(phys_w_missing ~ population + population2 + pct_below_median_income,
            data   = data3,
            family = 'poisson')

pos5 <- glm(phys_w_missing ~ population + population2 + population3 + pct_below_median_income,
            data   = data3,
            family = 'poisson')

#Poisson outputs
summary(pos1)
summary(pos2)
summary(pos3)
summary(pos4)
summary(pos5)

#Negative Binomial models - constrained 
nb1 <- MASS::glm.nb(phys_w_missing ~ population,
                    data = data3)

nb2 <- MASS::glm.nb(phys_w_missing ~ population + population2, #convergence issues
                    data = data3)

nb3 <- MASS::glm.nb(phys_w_missing ~ population + pct_below_median_income,
                    data = data3)

nb4 <- MASS::glm.nb(phys_w_missing ~ population + population2 + pct_below_median_income, #convergence issues
                    data = data3)

nb5 <- MASS::glm.nb(phys_w_missing ~ population + population2 + population3 + pct_below_median_income, #convergences issues
                    data = data3)

#Negative binomial models with polynomials all fail to converge - Models 2, 4, and 5

#Negative Binomial outputs
summary(nb1)
summary(nb2)
summary(nb3)
summary(nb4)
summary(nb5)

pchisq(2 * (logLik(nb1) - logLik(pos1)), df = 1, lower.tail = FALSE)
pchisq(2 * (logLik(nb2) - logLik(pos2)), df = 1, lower.tail = FALSE)
pchisq(2 * (logLik(nb3) - logLik(pos3)), df = 1, lower.tail = FALSE)
pchisq(2 * (logLik(nb4) - logLik(pos4)), df = 1, lower.tail = FALSE)
pchisq(2 * (logLik(nb5) - logLik(pos5)), df = 1, lower.tail = FALSE)

#Creating table of AIC scores across models
tibble(model_number = c(1:5),
       possion      = c(AIC(pos1),
                        AIC(pos2),
                        AIC(pos3),
                        AIC(pos4),
                        AIC(pos5)),
       neg_binomial = c(AIC(nb1),
                        AIC(nb2),
                        AIC(nb3),
                        AIC(nb4),
                        AIC(nb5)))


#Creating a data frame of Poisson model outputs
output_table <- function(model, model_name){
  
  augment(model) %>% 
    mutate(model = model_name)
}

#Poisson models stacked outputs table using broom::augment
output_table(pos1, 'pos1') %>% 
  bind_rows(output_table(pos2, 'pos2')) %>% 
  bind_rows(output_table(pos3, 'pos3')) %>% 
  bind_rows(output_table(pos4, 'pos4')) %>% 
  bind_rows(output_table(pos5, 'pos5')) -> poisson_outputs

#Plotting
poisson_outputs %>% 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(aes(color = model)) +
  facet_wrap(~model)

#Negative Binomial models stacked outputs table using broom::augment
output_table(nb1, 'nb1') %>% 
  bind_rows(output_table(nb2, 'nb2')) %>% 
  bind_rows(output_table(nb3, 'nb3')) %>% 
  bind_rows(output_table(nb4, 'nb4')) %>% 
  bind_rows(output_table(nb5, 'nb5')) -> nb_outputs

#Plotting
nb_outputs %>% 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(aes(color = model)) +
  facet_wrap(~model)

#Estimating expected counts
coef <- pos4$coef
vcov <- vcov(pos4)
#Values to estimate across
population <- seq(0,500000, 1000)
#Simulating Beta coefficients
sim_betas <- MASS::mvrnorm(1000, coef, vcov)
#Creating container matrix
out <- matrix(0, nrow = length(population), ncol = 3)
#Creating coefficient matrix
X <- as.matrix(cbind(1, 0, median(data3$population2, na.rm = TRUE), median(data3$pct_below_median_income, na.rm = TRUE)))

#Running for-loop for expected counts
for(i in 1:length(population)){
  
  X[,2] <- population[i]
  simfit <- exp(sim_betas %*% t(X))
  out[i,] <- c(mean(simfit), quantile(simfit, c(0.025, 0.975)))
  
}

out %>% 
  as_tibble() %>% 
  mutate(population = population) %>% 
  ggplot(aes(x = population)) +
  geom_line(aes(y = V1), size = 1.5) +
  geom_line(aes(y = V2), size = 1, linetype = 'dashed') +
  geom_line(aes(y = V3), size = 1, linetype = 'dashed')
