library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")
incarceration <- read.csv("~/Documents/info201/data/incarceration_trends.csv")
View(incarceration)
## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}
test_query1() 

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}
test_query2()

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

max_jail_pop <- incarceration %>% 
  group_by(state) %>% 
  

#largest_increase

#num_majority_black


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function takes the incarceration dataset and calculates the total jail pop for each year 
get_year_jail_pop <- function() {
  df <- incarceration %>% 
    group_by(year) %>% 
    summarise(yearly_jail_pop = sum(total_jail_pop, na.rm = T)) %>% 
    select(year, yearly_jail_pop)
return(df)   
}

yearly_pop <- get_year_jail_pop()
View(yearly_pop)

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  yearly_pop <- get_year_jail_pop()
  labels <- labs(
    title = "Yearly Jail Population in the United States 1970-2018",
    x = "Year", 
    y = "Total Jail Population", 
    caption = "This graphic shows the growth of the total population of all jails in the United States by year, from 1970-2018"
  )
  chart <- ggplot(yearly_pop) +
    geom_col(mapping = aes(x = year, y= yearly_jail_pop)) +
    scale_y_continuous(labels = scales::comma)+
    labels
  return(chart)   
} 

jail_pop_graph <- plot_jail_pop_for_us()
jail_pop_graph


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>

get_jail_pop_by_states <- function(states) {
  df <- incarceration %>% 
    filter(state %in% states) %>% 
    group_by(state, year) %>% 
    summarise(yearly_jail_pop =  sum(total_jail_pop, na.rm = T))
  return(df)
}

state_test <- c("NY", "FL", "WA", "MI", "ME", "NH")
test <- get_jail_pop_by_states(state_test)
View(test)

plot_jail_pop_by_states <- function(states) {
  state_pop <- get_jail_pop_by_states(states)
  labels <- labels <- labs(
    title = "Yearly Jail Population in the United States by State - 1970-2018",
    x = "Year", 
    y = "Total Jail Population", 
    caption = "This graphic shows the growth of the total population of jails in selected states by year, from 1970-2018"
  ) 
  chart <- ggplot(state_pop) +
  geom_line(mapping = aes(x = year, 
                           y = yearly_jail_pop, 
                           color = state)) +
    scale_y_continuous(labels = scales::comma) +
    labels
  return(chart) 
  
}

jail_pop_state <- plot_jail_pop_by_states(state_test)
jail_pop_state


#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
get_data <- function() {
  
}
make_data <- incarceration %>% #do this by region instead 
  select(year, state, division, total_jail_pop, black_jail_pop, white_jail_pop) %>% 
  group_by(year, division) %>% 
  summarise(total_jail = sum(total_jail_pop, na.rm = T), 
            black_jail = sum(black_jail_pop, na.rm = T), 
            white_jail = sum(white_jail_pop, na.rm = T)) %>% 
  filter(year >= 1985) %>% 
  mutate(black_percent = (black_jail/total_jail)*100, 
         white_percent = (white_jail/total_jail)*100,
         black_white_ratio = black_percent/white_percent)
View(make_data) 

plot <- ggplot(make_data) +
  geom_point(mapping = aes(x = white_percent, y = black_percent, 
                           size = total_jail, color = division),
             alpha = 0.3) +
  labs(
    title = "Comparing the Percentage of Black vs White Individuals in Jail, by Sub-Region", 
    x = "Percent of Jail Pop. that is White", 
    y = "Percent of Jail Pop. that is Black"
  )
plot

plot <- ggplotly(plot)
plot

# data <- incarceration %>% 
#   filter(year == 2018) %>% 
#   select(county_name, state, division, total_pop, total_jail_pop, ) %>% 
#   group_by(division) 
# View(data)
data <- incarceration %>% #do this by region instead 
  select(year, state, division, total_jail_pop, black_jail_pop, white_jail_pop) %>% 
  group_by(year, division) %>% 
  summarise(total_jail = sum(total_jail_pop, na.rm = T), 
            black_jail = sum(black_jail_pop, na.rm = T), 
            white_jail = sum(white_jail_pop, na.rm = T)) %>% 
  filter(year == 2018)
View(data)

plot <- ggplot(data) +
  geom_point(mapping = aes(x = white_jail, y = black_jail, color = division))
  #ylim(0, 10000)
plot


# scatterplot showing per capita prison pop
# - population size x prison pop
# 
# percent white pop x percent black pop


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>

# per capita prison pop map
# - Which areas inprison the most people (adjusted for population size)
# 
# ratio (black/white)

make_data <- incarceration %>% #do this by region instead 
  select(year, state, division, total_jail_pop, black_jail_pop, white_jail_pop) %>% 
  group_by(year, division) %>% 
  summarise(total_jail = sum(total_jail_pop, na.rm = T), 
            black_jail = sum(black_jail_pop, na.rm = T), 
            white_jail = sum(white_jail_pop, na.rm = T)) %>% 
  filter(year >= 1985) %>% 
  mutate(black_percent = (black_jail/total_jail)*100, 
         white_percent = (white_jail/total_jail)*100,
         black_white_ratio = black_percent/white_percent)
View(make_data) 


#----------------------------------------------------------------------------#

## Load data frame ---- 

play <- function(choices) {
  data <- incarceration %>% 
    group_by(state, year) %>% 
    summarise(yearly_total = sum(total_jail_pop, na.rm = T)) %>% 
    head
  # filter(state %in% choices)
  
  chart <- ggplot(data) + 
    geom_point(mapping = aes(x = year, y= yearly_total, color = state), 
               shape = 21, 
               alpha = 0.5) +
    geom_line(mapping = aes(x = year, y = yearly_total, color = state))
  
  return(chart)
}

test <- play(c("WA", "OR", "CA", "MA", "ME", "NH", 'PA', 'AZ', "TN"))
test
