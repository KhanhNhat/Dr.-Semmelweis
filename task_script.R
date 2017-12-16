#Task 1: Load package and data
library(tidyverse)

yearly = read_csv('yearly_deaths_by_clinic.csv')

#Task 2: Create a new column: proportion_deaths
yearly = mutate(yearly, proportion_deaths = deaths/births)

#Taks 3: Plot proportion_deaths by year
ggplot(yearly, aes(x = year, y = proportion_deaths, col = clinic)) +
  geom_line()

#Taks 4: Import monthly_deaths.csv
monthly = read_csv('monthly_deaths.csv')
monthly = mutate(monthly, proportion_deaths = deaths/births)
head(monthly)

#Taks 5: Plot proportion_death by month
ggplot(monthly, aes(x = date, y = proportion_deaths)) +
  geom_line()

#Taks 6: Explore the effect of handwashing
handwashing_start = as.Date('1847-06-01')
monthly = mutate(monthly, handwashing_started = date >= handwashing_start)
ggplot(monthly, aes(x = date, y = proportion_deaths, col = handwashing_started)) +
  geom_line() + labs(x ='Date', y = 'Proportion of death: Before and After handwashing')

#Task 7: Explore... continue
summary_monthly = monthly %>%
  group_by(handwashing_started) %>%
  summarise(mean(proportion_deaths))

summary_monthly

#Task 8: Statistic analysis T-Test with montly dataset
test_result = t.test(proportion_deaths ~ handwashing_started, data = monthly)
test_result

#Task 9: Find linear regression model that death is a function of birth
model_lm = lm(deaths ~ births, data = yearly)
summary(model_lm)

#Draw residuals between real value of death and regression model line
ggplot(yearly, aes(x = births, y = deaths)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) + 
  geom_segment(aes(xend = births, yend = fitted.values(model_lm)), 
               arrow = arrow(length = unit(0.25, 'cm')), col = 'grey')

