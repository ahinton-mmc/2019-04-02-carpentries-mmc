# Make a plot of life expectancy - Africa

# load libraries
library(here)
library(tidyverse)

# Read in the data
gapminder <- readr::read_csv("data/gapminder/raw/gapminder_data.csv")
# View data
head(gapminder)

# Old R
mean(gapminder$gdpPercap[gapminder$continent=="Africa"])
mean(gapminder$gdpPercap[gapminder$continent=="Americas"])

# New R
year_country_gdp <- gapminder %>% 
  filter(continent == "Africa") %>% 
  select(year, country, gdpPercap)

  
  challenge_1 <- gapminder %>% 
    filter(continent == "Africa") %>% 
    select(lifeExp, country, year) 
  
  gapminder %>% 
    group_by(continent) %>% 
    summarize(mean_val = mean(gdpPercap))

  challenge_2 <- gapminder %>% 
    group_by(country) %>% 
    summarize(country_lifeExp = mean(lifeExp)) 
  challenge_2b <- challenge_2 %>% 
    filter(country_lifeExp == min(country_lifeExp) | country_lifeExp == max(country_lifeExp))

  gapminder %>% 
    group_by(continent) %>% 
    summarize(mean_val = mean(gdpPercap),
              sd_gdpPercap = sd(gdpPercap))
# Plot
  gapminder %>% 
    filter(continent=="Africa") %>% 
  ggplot(aes(x=year, y=lifeExp, color = continent)) +
  geom_line() +
    facet_wrap( ~ country)
  
