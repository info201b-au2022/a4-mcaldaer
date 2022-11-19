library(tidyverse)
library(plotly)

# The functions might be useful for A4
#source("../source/a4-helpers.R")
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

#avg percent in population vs average percent in 

#what year had largest increase in 
largest_increase <- incarceration %>% 
  select(year, state, total_jail_pop) %>% 
  group_by(year) %>% 
  summarise(yearly_total = sum(total_jail_pop, na.rm = T)) %>% 
  mutate(new_inmates = yearly_total - lag(yearly_total)) %>% 
  filter(new_inmates == max(new_inmates, na.rm = T)) %>% 
  pull(year)
largest_increase

#what state had largest increase
largest_state_increase <- incarceration %>% 
  select(year, state, total_jail_pop) %>% 
  group_by(state, year) %>% 
  summarise(yearly_total = sum(total_jail_pop, na.rm = T)) %>% 
  mutate(new_inmates = yearly_total - lag(yearly_total)) %>% 
  arrange(-new_inmates) %>% 
  filter(new_inmates == max(new_inmates, na.rm = T)) %>% 
View(largest_state_increase)

largest_state_increase <- largest_state_increase %>% 
  filter(new_inmates == max(new_inmates, na.rm = T))
View(largest_state_increase)


#num_majority_black_jail_pop


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
get_percentage_data <- function() { #This function wrangles the data needed to make the plot in a few steps 
  bw_data <- incarceration %>%  #first, 
    filter(year == 2018) %>% 
    select(state, county_name, total_pop_15to64, white_pop_15to64, black_pop_15to64, #select relevant rows
           total_jail_pop, white_jail_pop, black_jail_pop) %>% 
    mutate(black_pop_percent = (black_pop_15to64/total_pop_15to64)*100, #calculate % of black ppl in general pop
         white_pop_percent = (white_pop_15to64/total_pop_15to64)*100, #calculate % of white ppl in general pop
         black_jail_percent = (black_jail_pop/total_jail_pop)*100, #calculate % of black ppl in jail pop
         white_jail_percent = (white_jail_pop/total_jail_pop)*100) %>% #calculate % of white ppl in jail pop
    filter(white_jail_percent <= 100, #I noticed some rows had percentages <100, which indicates some kind of error, so I removed these
         black_jail_percent <= 100) %>% 
    select(state, county_name, black_pop_percent, white_pop_percent, black_jail_percent, white_jail_percent) 

#In order to enable ggplot to assign color by race, I need to combine the black_pop_percent and white_pop_percent columns into a single col...
  gen_pop <- bw_data %>% 
    select(black_pop_percent, white_pop_percent) %>% 
    rename(Black = black_pop_percent, White = white_pop_percent) %>% #changing the names here is what will allow ggplot to plot the data by racial group
    gather(key = "race", value = "percent_of_pop", 1:2) %>% #put all pop_percent data into a single column w a new column that keeps track of the race
    mutate(id = c(1:5420)) #need a numerical id to make sure the rows correspond properly during the final left_join()
  
#Now doing the same thing for the jail populations 
 jail_pop <- bw_data %>% 
    select(black_jail_percent, white_jail_percent) %>% 
    rename(Black = black_jail_percent, White = white_jail_percent) %>% 
    gather(key = "race", value = "percent_of_jail_pop", 1:2) %>% 
    mutate(id = c(1:5420)) 
  
#Then recombine them so that population % and jail % are in two respective columns (instead of 4!) along with the new column that tracks race
  bw <- left_join(gen_pop, jail_pop, by = "id") %>% 
    select(id, race.x, percent_of_pop, percent_of_jail_pop) %>% 
    rename(race = race.x)

  return(bw)
}

tester <- get_percentage_data()
View(tester)

#This function plots the data wrangled above on a scatterplot
plot_percentage_data <- function() {
  label <- labs( #labels for the plot
    title = "Comparing the Representation of Black and White Individuals in Jail
    vs. the General Population", 
    subtitle = "Data from 2018",
    caption = "This plot compares the representation of Black and White individuals 
    in the general population and jail populations. Each dot represents a singular U.S. county.", 
    x = "Percent of General Population", 
    y = "Percent of Jail Population"
  )
  data <- get_percentage_data() #call data wrangling fxn from above to get data 
  plot <- ggplot(data) + #specifying plot parameters
    geom_point(mapping = aes(x = percent_of_pop, y = percent_of_jail_pop, color = race),
               alpha = 0.75) +
    geom_smooth(mapping = aes(x = percent_of_pop, y = percent_of_jail_pop, color = race),
    linetype = "bold", se=FALSE) +
    scale_color_manual(values=c("#b3697a", "#69b3a2")) +
    label +
    geom_abline(color = "#468a7a", size = 1)
    
  plotly_fied <- ggplotly(plot)

    return(plotly_fied)
}

bw_percentage_plot <- plot_percentage_data()
bw_percentage_plot

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
