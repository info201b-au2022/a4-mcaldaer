library(tidyverse)
library(plotly)

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
  group_by(state) 
  

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
    caption = "This graphic shows the growth of the total population of all jails in the United States from 1970-2018"
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
# test <- get_jail_pop_by_states(state_test)
# View(test)

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
get_percentage_data <- function() {
  bw_data <- incarceration %>% 
    filter(year == 2018) %>% 
    select(state, county_name, division, total_pop_15to64, total_jail_pop, white_pop_15to64, white_jail_pop, black_pop_15to64, black_jail_pop) %>% 
    mutate(black_pop_percent = (black_pop_15to64/total_pop_15to64)*100, 
           black_jail_percent = (black_jail_pop/total_jail_pop)*100,
           white_pop_percent = (white_pop_15to64/total_pop_15to64)*100, 
           white_jail_percent = (white_jail_pop/total_jail_pop)*100) %>% 
    filter(white_jail_percent <= 100, 
           black_jail_percent <= 100) %>% 
    select(state, county_name, division, black_pop_percent, black_jail_percent, white_pop_percent, white_jail_percent)
  return(bw_data)
}

tester <- get_percentage_data()
View(tester)

plot_percentage_data <- function() {
  percentage_plot <- ggplot(bw_data) +
    geom_point(mapping = aes(x = white_pop_percent, y = white_jail_percent),
               alpha = 0.8, 
               color = "red") +
    geom_smooth(mapping = aes(x = white_pop_percent, y = white_jail_percent), 
                color = "red") +
    geom_point(mapping = aes(x = black_pop_percent, y = black_jail_percent),
               alpha = 0.8, 
               color = "blue") +
    geom_smooth(mapping = aes(x = black_pop_percent, y = black_jail_percent), 
                color = "blue") +
    geom_abline()
  
  percentage_plotly <- ggplotly(percentage_plot)
  return(percentage_plotly)
}

test_plot <- plot_percentage_data()
test_plot





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
  select(year, state, county_name, total_jail_pop, black_jail_pop, white_jail_pop) %>%
  filter(year == 2018) %>% 
  group_by(county_name) %>% 
  summarise(total_jail = sum(total_jail_pop, na.rm = T), 
            black_jail = sum(black_jail_pop, na.rm = T), 
            white_jail = sum(white_jail_pop, na.rm = T)) %>% 
  mutate(black_percent = (black_jail/total_jail)*100, 
         white_percent = (white_jail/total_jail)*100,
         black_white_ratio = black_percent/white_percent) %>% 
  rename(county = county_name)
make_data <- gsub("County", "", make_data$county)
View(make_data) 

county_shape <- map_data("county") %>% 
  rename(state = region, county = subregion)
View(county_shape)  
View(county_shape)
p <- ggplot(county_shape) +
  geom_polygon( 
    mapping = aes(x = long, y= lat, group = group, color = black_white_ratio),
    fill = "grey",
    color = "black",
    size  = .1, 
  ) +
  coord_map()
p

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
