#Analyze life expectancy and CO2 emissions versus population with gapminder data
#Date: Jan 17th, 2023
#Author: Evie Brahmstedt

# load in packages necessary for analysis
library(tidyverse)
library(readr)

#read in data for analysis
gapminder_1997 <- read_csv("gapminder_1997.csv")

#plotting data for visualization
ggplot(data = gapminder_1997) + 
    aes(x = gdpPercap) +
    labs(x = "GDP Per Capita") +
    aes(y = lifeExp) +
    labs(y = "Life Expectancy (yrs)") +
    geom_point() +
    labs(title = "Do people in wealthy countries live longer?") +
    aes(color = continent) + 
    scale_color_brewer(palette = "Set1") +
    aes(size = pop/1000000) +
    labs(size = "Population (in millions)") +
    aes(shape = continent)

# Short handed ggplot
ggplot(data = gapminder_1997,
       aes(x = gdpPercap, y = lifeExp, color = continent,
           shape = continent, size = pop)) +
  labs(x = "GDP Per Capita", y = "Life Expectancy (yrs)",
       title = "Do people in wealthy countries live longer?",
       size = "Population (in millions)") +
  geom_point()

# Read in all of the data from gapminder (more years than 1997)

gapminder_data <- read_csv("gapminder_data.csv")
View(gapminder_data)
dim(gapminder_data)
head(gapminder_data)
tail(gapminder_data)

# Challenge: predicting the output
ggplot(data = gapminder_data) +
  aes(x=year, y=lifeExp, color=continent, group = country) +
  geom_line()

#learn about data
str(gapminder_data)

#make a boxplot with x = continent and y = lifeExp
ggplot(data = gapminder_1997) +
  aes(x=continent, y=lifeExp, color = continent) +
  geom_violin() +
  geom_jitter(aes(size = pop))

# histogram
ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_classic()




#to install more themes (have not run this)
install.packages("ggthemes")
install.packages("ggprism") #a specific theme (have not run this) 



ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(rows = vars(continent))

ggsave(figures/"awesome_plot.jpg", width = 6, height = 4)


#--------------xX---

name <- "Ben"
name
age <- 26
age
name <- "Harry Potter"
name

read_csv()
read_csv(file = "gapminder_1997.csv")

#get today's date
Sys.Date()
#locate where you are in your system directory
getwd()

sum(5, 6)

round()
round(3.1415)
round(3.1415,3)
round(x = 3.1415)
round(x = 3.1415, digits = 2)
round(digits = 2, x = 3.1415)
round(2, 3.1415)
#order matters unless you are explicit


