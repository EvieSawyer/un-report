library(tidyverse)
getwd()
gapminder_data <- read_csv("data/gapminder_data.csv")

summarize(gapminder_data, averageLifeExp = mean(lifeExp), medianLifeExp = median(lifeExp))

# Learning to pipe

gapminder_data%>%
  summarize(averageLifeExp = mean(lifeExp))

gapminder_summary

#Filtering
gapminder_summary_2007<-gapminder_data%>%
  filter(year == 2007)%>%
  summarize(average = mean(lifeExp))

#Finding average gdpPercap for the first year in the dataset
gapminder_data%>%
  summarize(Firstyear = min(year))
gapminder_data%>%
  filter(year == 1952)%>%
  summarize(average = mean(gdpPercap))

#OR
gapminder_data%>%
  filter(year == 1952)%>%
  summarize(Average_GDP = mean(gdpPercap))


# Using group_by()
gapminder_data%>%
  group_by(year, continent)%>%
  summarize(average = mean(lifeExp), 
            error = sd(lifeExp))
#Mutate function

gapminder_data%>%
  mutate(gdp = pop * gdpPercap)

#Mutate a new column which is population in millions

gapminder_data%>%
  mutate(popInMillions = pop/1000000)

#Select function

gapminder_data%>%
  select(pop, year)

gapminder_data%>%
  select(-continent, -pop)

# Pivot _wider

gapminder_data%>%
  select(country, continent, year, lifeExp)%>%
  pivot_wider(names_from = year, values_from = lifeExp)%>%
  view()

# Working with messy data

co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip=2,
         col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emissions <- co2_emissions_dirty%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total emissions", 
                         "Emissions per capita (metric tons of carbon dioxide" = "per_capita_emissions"))%>%
  pivot_wider(names_from = series, values_from = value)%>%
#filter for year 2005
#select to remove the year column
#store as an object with a descriptive name (added co2_emissions_2005 above with an arrow)
  filter(year == 2005) %>%
  select(-year)
co2_emissions_2005

# Bringing in 2007 population data
gapminder_data_2007 <- read_csv("data/gapminder_data.csv")%>%
  filter(year == 2007)%>%
  select(country, pop, lifeExp, gdpPercap)

joined_co2_pop <- inner_join(co2_emissions, gapminder_data_2007, by = "country") 
#adding by = country is not necessary here, but explicitly tells R to join by country

anti_join(gapminder_data_2007, co2_emissions, by = "country")
#tells us what countries the two tables do NOT have in common; only one of the tables has each of the countries listed; order matters here as it reads the missing countries based on what's in the first table

full_join(co2_emissions, gapminder_data_2007)

# Writing a csv
write_csv(joined_co2_pop, file = "data/joined_co2_pop.csv")

# Reading csv back in 
joined_co2_pop <- read_csv("data/joined_co2_pop.csv")%>%
  view()

#making histograms of data to view distribution

joined_co2_pop%>%
  ggplot(aes(x = gdpPercap))+
  geom_histogram()

gdp_co2_plot <- joined_co2_pop%>%
  ggplot(aes(x = gdpPercap, y = `Emissions per capita (metric tons of carbon dioxide)`)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "GDP Per Capita", y = "CO2 Emissions Per Capita (metric tons)", title = "Comparing Per Capita CO2 Emissions and GDP") +
  theme_classic()+ 
  ggpubr::stat_regline_equation(aes(label = after_stat(rr.label)))

ggsave(gdp_co2_plot, filename = "figures/gdp_vs_co2_plot.png", 
       height = 4, width = 6, units = "in", 
       dpi = 300)
