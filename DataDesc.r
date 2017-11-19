library(tidyverse)
setwd("A:/Dataset/New folder")
data<- read_csv("wowah_data.csv")

userLevel<- data %>%
  mutate(Date=as.Date(data$timestamp, "%m/%d/%y")) %>%
  group_by(char) %>%
  summarise(n=n(), minDate = min(Date), maxDate = max(Date), maxLevel=max(level)) %>%
  arrange(desc(n)) %>%
  mutate(TimeDiff = as.numeric(difftime(maxDate,minDate, units="days")))

dtdf<-filter(userLevel,n<17000)
ggplot(data=dtdf,mapping = aes(x=dtdf$n))+
  geom_histogram() +
  labs(x="Number of Interactions by user",y="Frequency of User Interactions",title="Histogram")