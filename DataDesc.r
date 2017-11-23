library(tidyverse)
setwd("A:/Dataset/New folder")
data<- read_csv("wowah_data.csv")

data %>%
    group_by(level) %>%
    summarise(chars = n_distinct(char), 
              logins = n()) %>%
    gather(var, value, -level) %>%
    ggplot(aes(x = level, y = value)) +
    geom_line() +
    facet_wrap(~var, ncol = 1, scales = "free_y")


data$date <- as.Date(gsub(" .+","",df$timestamp),format = "%m/%d/%y")

data %>%  
  group_by(date,char) %>%
  summarize(hoursLoggedIn=n()/6) -> hours

hours %>%
  ggplot(aes(x=date,hoursLoggedIn))+geom_point(alpha=0.01)

hours$hoursLoggedIn %>% 
  table %>% data.frame %>%
  plot()
