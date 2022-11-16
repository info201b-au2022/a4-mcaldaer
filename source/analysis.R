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

#max_jail_pop

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

test <- get_jail_pop_by_states("WA")
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

plot_jail_pop_by_states(c("WA", "CA", "NY", "KY", "RI", "SD", "OR", "FL"))

# tst <- ggplot(test) +
#   geom_line(mapping = aes(x = year, y = yearly_jail_pop))
# tst

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


