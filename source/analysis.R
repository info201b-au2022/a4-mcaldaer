# install.packages("ggplot2")
library(tidyverse)
library(plotly)
library(leaflet)
library(knitr)
# install.packages("usmap")
library(usmap)

# The functions might be useful for A4
#source("../source/a4-helpers.R")
incarceration <- read.csv("~/Documents/info201/data/incarceration_trends.csv")
View(incarceration)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Here I calculate a summary variable for each of my sections
#----------------------------------------------------------------------------#

#Section 3: variable
#what year had largest increase in jail population
largest_increase <- incarceration %>% 
  select(year, state, total_jail_pop) %>% 
  group_by(year) %>% 
  summarise(yearly_total = sum(total_jail_pop, na.rm = T)) %>% 
  mutate(new_inmates = yearly_total - lag(yearly_total)) %>% 
  filter(new_inmates == max(new_inmates, na.rm = T)) %>% 
  pull(year)
largest_increase

year_largest_pop <- incarceration %>% 
  select(year, state, total_jail_pop) %>% 
  group_by(year) %>% 
  summarise(yearly_total = sum(total_jail_pop, na.rm = T)) %>% 
  filter(yearly_total == max(yearly_total, na.rm = T)) %>% 
  pull(year)
year_largest_pop

#Section 4: variable
#what state has largest incarcerated pop
state_largest <- incarceration %>% 
  group_by(state, year) %>% 
  summarise(yearly_total = sum(total_jail_pop, na.rm = T)) %>% 
  filter(yearly_total > 0) %>% 
  filter(yearly_total == max(yearly_total)) %>% 
  arrange(-yearly_total) 
# View(state_largest)

state_largest_table <- kable(state_largest)

largest_pop_state <- state_largest[1,] %>% 
  pull(state)
largest_pop_state

largest_pop <- state_largest[1,] %>% 
  pull(yearly_total)
largest_pop

largest_pop_year <- state_largest[1,] %>% 
  pull(year)
largest_pop_year 

#Section 5: 
bw_disparities <- incarceration %>%  
  filter(year == 2018) %>% #first, select only data from 2018 
  select(state, county_name, total_pop_15to64, white_pop_15to64, black_pop_15to64, #select relevant columns
         total_jail_pop, white_jail_pop, black_jail_pop) %>% 
  mutate(black_pop_percent = (black_pop_15to64/total_pop_15to64)*100, #calculate % of black ppl in general pop
         white_pop_percent = (white_pop_15to64/total_pop_15to64)*100, #calculate % of white ppl in general pop
         black_jail_percent = (black_jail_pop/total_jail_pop)*100, #calculate % of black ppl in jail pop
         white_jail_percent = (white_jail_pop/total_jail_pop)*100) %>% #calculate % of white ppl in jail pop
  filter(white_jail_percent <= 100, #I noticed some rows had percentages <100, which indicates some kind of error, so I removed these outliers
         black_jail_percent <= 100) %>% 
  select(state, county_name, black_pop_percent, black_jail_percent, white_pop_percent, white_jail_percent) %>% 
  mutate(black_ratio = black_jail_percent/black_pop_percent, 
         white_ratio = white_jail_percent/white_pop_percent)
View(bw_disparities)

max_white_ratio <- bw_disparities %>% 
  filter(white_ratio == max(white_ratio, na.rm = T)) %>% 
  pull(county_name)
max_white_ratio

max_black_ratio <- bw_disparities %>% 
  filter(black_ratio == max(black_ratio, na.rm = T)) %>% 
  pull(county_name)
max_black_ratio

#Section 6: variable
#avg ratio for black vs white? 
avg_risk_ratio <- incarceration %>% 
  filter(year == 2018) %>% 
  rename(county = county_name) %>% 
  select(county, state, 
         black_jail_pop, black_pop_15to64, 
         white_jail_pop, white_pop_15to64) %>% 
  mutate(black_risk = black_jail_pop/black_pop_15to64, 
         white_risk = white_jail_pop/white_pop_15to64, 
         relative_risk = (black_risk/white_risk)) %>% 
  select(county, state, black_risk, white_risk, relative_risk) %>% 
  summarise(avg_rr = median(relative_risk, na.rm = T)) %>% 
  pull(avg_rr)
avg_risk_ratio


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

# This function calls the above data wrangling function and creates a bar chart with that data
#yearly pop on the yaxis and year on the x axis 
plot_jail_pop_for_us <- function()  {
  yearly_pop <- get_year_jail_pop()
  labels <- labs( #creating labels for the chart
    title = "Yearly Jail Population in the United States 1970-2018",
    x = "Year", 
    y = "Total Jail Population", 
    caption = "Figure 1: This graphic shows the growth of the total population of all jails in the United States from 1970-2018"
  )
  chart <- ggplot(yearly_pop) + #specifying parameters for the ggplot fxn
    geom_col(mapping = aes(x = year, y= yearly_jail_pop)) +
    scale_y_continuous(labels = scales::comma)+
    labels
  return(chart)   
} 

jail_pop_graph <- plot_jail_pop_for_us() #here is the graph!
jail_pop_graph


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# These functions a) wrangle specified state-level jail pops by year and b) create a line chart that data

get_jail_pop_by_states <- function(states) { #this fxn creates a df with yearly jail population totals by state
  df <- incarceration %>% 
    filter(state %in% states) %>% #this filters the incarceration df for states that match the input vector
    group_by(state, year) %>% 
    summarise(yearly_jail_pop =  sum(total_jail_pop, na.rm = T))
  return(df)
}

state_vector <- c("NY", "FL", "WA", "MI", "ME", "NH") #this is a vector that can be put into the above fxn
# test <- get_jail_pop_by_states(state_vector) #testing the fxn wrangles the data properly 
# View(test)

plot_jail_pop_by_states <- function(states) { #this fxn calls the above data wrangling fxn, and then creates 
  #a line chart showing the growth of jail populations by the states selected in "state_vector"
  state_pop <- get_jail_pop_by_states(states)
  labels <- labels <- labs( #labels for the chart 
    title = "Yearly Jail Population in the United States by State - 1970-2018",
    x = "Year", 
    y = "Total Jail Population", 
    caption = "Figure 2: This graphic shows the growth of the total population of jails in selected states by year, from 1970-2018"
  ) 
  chart <- ggplot(state_pop) + #specifying parameters for ggplot line chart 
  geom_line(mapping = aes(x = year, 
                           y = yearly_jail_pop, 
                           color = state)) + #one line for each state 
    scale_y_continuous(labels = scales::comma) + #making sure pop size is show in standard notation  
    labels
  return(chart) 
  
}

jail_pop_state <- plot_jail_pop_by_states(state_vector)
jail_pop_state

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Comparing the Representation of Black and White Individuals in Jail vs. the General Population
# Data Wrangling Function 
get_percentage_data <- function() { #This function wrangles the data needed to make the plot in a few steps 
  bw_data <- incarceration %>%  
    filter(year == 2018) %>% #first, select only data from 2018 
    select(state, county_name, total_pop_15to64, white_pop_15to64, black_pop_15to64, #select relevant columns
           total_jail_pop, white_jail_pop, black_jail_pop) %>% 
    mutate(black_pop_percent = (black_pop_15to64/total_pop_15to64)*100, #calculate % of black ppl in general pop
         white_pop_percent = (white_pop_15to64/total_pop_15to64)*100, #calculate % of white ppl in general pop
         black_jail_percent = (black_jail_pop/total_jail_pop)*100, #calculate % of black ppl in jail pop
         white_jail_percent = (white_jail_pop/total_jail_pop)*100) %>% #calculate % of white ppl in jail pop
    filter(white_jail_percent <= 100, #I noticed some rows had percentages <100, which indicates some kind of error, so I removed these outliers
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
    rename(Race = race.x)

  return(bw)
}

# tester <- get_percentage_data()
# View(tester)

#This function plots the data wrangled above on a scatterplot
plot_percentage_data <- function() {
  label <- labs( #labels for the plot
    title = "Comparing the Representation of Racial Groups in Jail vs. the General Population", 
    x = "Percent of General Population", 
    y = "Percent of Jail Population"
  )
  
  data <- get_percentage_data() #call data wrangling fxn from above to get data 
  plot <- ggplot(data) + #specifying scatterplot parameters
    geom_point(mapping = aes(x = percent_of_pop, y = percent_of_jail_pop, color = Race),
               alpha = 0.75) +
    geom_smooth(mapping = aes(x = percent_of_pop, y = percent_of_jail_pop, color = Race),
    linetype = "bold", se=FALSE) + #adding linear regression line to emphasize trends
    scale_color_manual(values=c("#b3697a", "#69b3a2")) + #picking a nice color scheme :) 
    label +
    geom_abline(color = "#468a7a", linewidth = 1) #adding an x=y line to show diversion from expected result
  
  plotly_fied <- ggplotly(plot) %>% #making it interactive 
    layout(title = list(text = paste0('Comparing the Representation of Racial Groups in Jail vs. the General Population (2018)',
                                      '<br>',
                                      '<sup>',
                                      'Figure 3: Comparing the percentage of Black and White individuals in jail vs. the general population. Each dot represents a singular U.S. county. Hover over a dot to see population breakdowns. ' , '</sup>')))
  
  
    return(plotly_fied)
}

bw_percentage_plot <- plot_percentage_data()
bw_percentage_plot

 #----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Comparing the Risk of Incarceration for Black and White Individuals at the 
# Your functions might go here ... <todo:  update comment>

#-------------------------------------------------------------------------------
# Map #3: Risk Ratio 
get_map_data <- function() {
  data <- incarceration %>% 
    filter(year == 2018) %>% 
    select(fips, state, 
           black_jail_pop, black_pop_15to64, 
           white_jail_pop, white_pop_15to64) %>% 
    group_by(state) %>%
    summarise(black_jail = sum(black_jail_pop, na.rm = T, finite = T), 
              white_jail = sum(white_jail_pop, na.rm = T, finite = T), 
              black_pop = sum(black_pop_15to64, na.rm = T, finite = T), 
              white_pop = sum(white_pop_15to64, na.rm = T, finite = T), 
              black_risk = black_jail/black_pop, 
              white_risk = white_jail/white_pop, 
              Relative_Risk = log(black_risk/white_risk)) %>% 
    filter(! state %in% c("VT", "HI", "RI", "DE", "CT"))
  return(data)
}

# work <- get_map_data()
# View(work)

make_map <- function() {
  state_rr <- get_map_data()
  usmap <- plot_usmap(data = state_rr, values = "Relative_Risk") +
    scale_fill_continuous(
      low = "white", high = "#8a4655", name = "Relative Risk") + 
    theme(legend.position = "right") +
    labs(
      title = "Comparing the Relative Risk of Incarceration for Black and White Americans (2018)", 
      subtitle = "Data collected by the Vera Institute")

  usmap_ly <- ggplotly(usmap) %>% 
    layout(title = list(text = paste0('Comparing the Relative Risk of Incarceration for Black and White Americans (2018)',
                                                               '<br>',
                                                               '<sup>',
                                                               'Figure 4: The above map shows the relative risk of being incarcerated for Black and White Americans in most states comprising the United States. All states had a risk ratio greater than zero', '</sup>')))
  
  
  return(usmap_ly)
  
}

map_ly <- make_map()
map_ly

## Load data frame ---- 
