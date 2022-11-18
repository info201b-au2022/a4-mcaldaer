#This is a space for me to explore the dataset 
incarceration <- read.csv("~/Documents/info201/data/incarceration_trends.csv")
View(incarceration)

dim(incarceration_file)
colnames(incarceration_file)
range(incarceration_file$year)

chart <- ggplot(yearly_pop) +
  geom_histogram(mapping = aes(x = year))
return(chart)  

library(tidyverse)
install.packages("hrbrthemes")
library(hrbrthemes)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)
View(data)
# plot
p <- data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 3") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
p

# ----------------------------------------------------------------------------
# Section 5 unused work:

#Black percent prison vs percent gen public 
b_data <- incarceration %>% 
  filter(year == 2018) %>% 
  select(state, county_name, division, total_pop_15to64, total_jail_pop, black_pop_15to64, black_jail_pop) %>% 
  mutate(black_pop_percent = (black_pop_15to64/total_pop_15to64)*100, 
         black_jail_percent = (black_jail_pop/total_jail_pop)*100) %>% 
  filter(black_jail_percent <= 100)
View(b_data)

plot <- ggplot(b_data) +
  geom_point(mapping = aes(x = black_pop_percent, y = black_jail_percent, 
                           color = division),
             alpha = 0.8) 
plot

plot <- ggplotly(plot)
plot

#White pop vs jail pop 
w_data <- incarceration %>% 
  filter(year == 2018) %>% 
  select(state, county_name, division, total_pop_15to64, total_jail_pop, white_pop_15to64, white_jail_pop) %>% 
  mutate(white_pop_percent = (white_pop_15to64/total_pop_15to64)*100, 
         white_jail_percent = (white_jail_pop/total_jail_pop)*100) %>% 
  filter(white_jail_percent <= 100)
View(w_data)
plot <- ggplot(w_data) +
  geom_point(mapping = aes(x = white_pop_percent, y = white_jail_percent, 
                           color = division),
             alpha = 0.8) 
plot