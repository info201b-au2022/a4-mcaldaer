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
