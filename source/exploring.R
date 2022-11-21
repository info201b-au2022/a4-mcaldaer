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
get_percentage_data <- function() {
  data <- incarceration %>% 
    filter(year == 2018) %>% 
    select(state, county_name, division, total_pop_15to64, total_jail_pop, white_pop_15to64, white_jail_pop, black_pop_15to64, black_jail_pop) %>% 
    mutate(black_pop_percent = (black_pop_15to64/total_pop_15to64)*100, 
           black_jail_percent = (black_jail_pop/total_jail_pop)*100,
           white_pop_percent = (white_pop_15to64/total_pop_15to64)*100, 
           white_jail_percent = (white_jail_pop/total_jail_pop)*100) %>% 
    filter(white_jail_percent <= 100, 
           black_jail_percent <= 100) %>% 
    select(state, county_name, division, black_pop_percent, black_jail_percent, white_pop_percent, white_jail_percent)
  return(data)
}

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
View(bw_data)

black_pop <- bw_data %>% 
  select(black_pop_percent, white_pop_percent) %>% 
  rename(Black = black_pop_percent, White = white_pop_percent) %>% 
  gather(key = "race", value = "percent_of_pop", 1:2) %>% 
  mutate(id = c(1:5420)) 
View(pop)  

white_pop <- bw_data %>% 
  select(black_jail_percent, white_jail_percent) %>% 
  rename(Black = black_jail_percent, White = white_jail_percent) %>% 
  gather(key = "race", value = "percent_of_jail_pop", 1:2) %>% 
  mutate(id = c(1:5420))
View(pop2)

bw <- left_join(pop, pop2, by = "id") %>% 
  select(id, race.x, percent_of_pop, percent_of_jail_pop) %>% 
  rename(race = race.x)
View(bw)

plot_new <- ggplot(bw) +
  geom_point(mapping = aes(x = percent_of_pop, y = percent_of_jail_pop, color = race))
plot_new  


# select(black_pop_percent, white_pop_percent, black_jail_percent, white_jail_percent) %>% 
#   gather(key="race", value="pop_percentage", 1:2) %>% 
#   gather(key = "race", value = "jail_percentage", 1:2) %>% 
#   select(race, pop_percentage, jail_percentage)
# 


# geom_point(mapping = aes(x = black_pop_percent, y = black_jail_percent),
#            alpha = 0.75, 
#            color = "#b3697a") +
# geom_smooth(mapping = aes(x = black_pop_percent, y = black_jail_percent), 
#             color = "#8a4655", se=FALSE) +
# geom_abline(color = "#10201d") +
# labels






# -----------------------------------------------------------------------------
#Percentage B vs percent W in jail 
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

#Comparing total pop to total_jail pop
data <- incarceration %>% #do this by region instead 
  select(year, state, division, total_jail_pop, black_jail_pop, white_jail_pop) %>% 
  group_by(year, division) %>% 
  summarise(total_jail = sum(total_jail_pop, na.rm = T), 
            black_jail = sum(black_jail_pop, na.rm = T), 
            white_jail = sum(white_jail_pop, na.rm = T)) %>% 
  filter(year == 2018)
View(data)

plot <- ggplot(data) +
  geom_point(mapping = aes(x = total_pop, y = total_jail_pop, color = division)) +
  ylim(0, 5000) +
  xlim(0, 2.5*10^6)
plot

# ------------------------------------------------------------------------------
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

# Section 6: Scratchwork
# ------------------------------------------------------------------------------
bw_data <- incarceration %>%  
  filter(year == 2018) %>% #first, select only data from 2018 
  select(state, county_name, total_pop_15to64, white_pop_15to64, black_pop_15to64, #select relevant columns
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
bw_ratio <- left_join(gen_pop, jail_pop, by = "id") %>% 
  select(id, race.x, percent_of_pop, percent_of_jail_pop) %>% 
  rename(race = race.x) %>% 
  mutate(ratio = percent_of_jail_pop/percent_of_pop)
View(bw_ratio)

#-------------------------------------------------------------------------------
#Map trial #1
county_shape <- map_data("county") %>% 
  rename(state = region, county = subregion)
View(county_shape) 

map_data <- incarceration %>% 
  filter(year == 2018) %>% 
  rename(county = county_name) %>% 
  select(state, county, total_pop, total_pop_15to64, black_pop_15to64, white_pop_15to64, 
         total_jail_pop, black_jail_pop, white_jail_pop) %>% 
  mutate(black_pop = (black_pop_15to64/total_pop_15to64)*100, 
         black_jail = (black_jail_pop/total_jail_pop)*100, 
         black_ratio = black_jail/black_pop, 
         
         white_pop = (white_pop_15to64/total_pop_15to64)*100, 
         white_jail = (white_jail_pop/total_jail_pop)*100, 
         white_ratio = white_jail/white_pop, 
         
         bw_ratio = (black_ratio/white_ratio)) %>% 
  filter(white_jail <= 100, 
         black_jail <= 100) %>% 
  select(state, county, bw_ratio, black_pop, black_jail, black_ratio, 
         white_pop, white_jail, white_ratio)
View(map_data)

county <- tolower(gsub(" County", "", map_data$county))

map_data$county <- county 

View(map_data)

join_bw <- left_join(county_shape, map_data, by = "county") %>% 
  select(long, lat, group, county, state.y, 
         bw_ratio,
         black_pop, black_jail, black_ratio, 
         white_pop, white_jail, white_ratio) %>% 
  rename(state = state.y)
View(join_bw)


p <- ggplot(join_bw) +
  geom_polygon( 
    mapping = aes(x = long, y= lat, group = group, fill = bw_ratio),
    # fill = "grey",
    # color = "black",
    size  = .1
  ) +
  coord_map()

p

#-----------------------------
# Map #2: Most represented incarcerated racial group by county 
map2_data <- incarceration %>% 
  filter(year == 2018) %>% 
  select(state, county_name, division, total_jail_pop, 
         aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, 
         white_jail_pop, other_race_jail_pop) %>% 
  mutate(aapi_percent = (aapi_jail_pop/total_jail_pop)*100, 
         black_percent = (black_jail_pop/total_jail_pop)*100, 
         latinx_percent = (latinx_jail_pop/total_jail_pop)*100, 
         native_percent = (native_jail_pop/total_jail_pop)*100, 
         white_percent = (white_jail_pop/total_jail_pop)*100, 
         other_percent = (other_race_jail_pop/total_jail_pop)*100,
         total_percent = aapi_percent + black_percent + latinx_percent + native_percent + white_percent + other_percent) %>% 
  select(state, county_name, division, aapi_percent, black_percent, latinx_percent, 
         native_percent, white_percent, other_percent)
View(map2_data)

trial <- map2_data %>% 
  gather(key = "race", value = "percent_of_jail", 4:9) %>% 
  group_by(county_name) %>% 
  summarise(percent_of_jail = which.max(percent_of_jail))
View(trial)

