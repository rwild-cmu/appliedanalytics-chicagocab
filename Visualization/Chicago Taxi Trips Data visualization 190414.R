#==================================================================#
# Applied Analytics: the Machine Learning Pipeline
# Creation date: April 14, 2019
# Last modification: April 14, 2019
# Created by: David Mitre Becerril                                 
# Objective: Create some data visualizations for the Chicago Taxi Trips dataset
#==================================================================#


#1) Install libraries ------
library(ggplot2)
library(dplyr)


#2) Data visualization based on processed data------
#Read data
df <- read.csv("weekly_stats_2017_nonzero.csv")
df <- df %>%
  mutate(ratio = rain/(rain + norain))

#No differences on revenue between driver with and without a company
means <- df %>% mutate(company=ifelse(company=="", "w/taxi company", "w/o taxi company")) %>%
  group_by(company) %>%
  summarise(mean=which.max(tabulate(revenue)))
p <- ggplot(df %>% mutate(company=ifelse(company=="", "w/taxi company", "w/o taxi company")), 
       aes(x = revenue, color = as.factor(company))) +
  geom_density(fill = "white", alpha = 0.1, position = "identity", size=1) +
  labs(x = "Revenue per week ($/week)",
       title = "Weekly revenue density based on taxi company, 2017") +
  xlim(0,1500) +
  theme(legend.position=c(0.85,0.70), legend.background=element_blank(), 
        legend.title=element_text(face="bold", size=10),
        panel.background=element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(name = "Legend",
                     labels = c("w/taxi company", "w/o taxi company"),
                     values = c("green4", "royalblue4"))
ggsave("Plot taxi company and revenue.jpg", plot = p, width = 7, height = 6, dpi = 300)


#Rain trips and revenue
p <- ggplot(df %>% sample_n(20000) %>% filter(ratio>=0) %>% mutate(ratio = ratio*100), 
            aes(x = revenue, y = ratio)) +
  geom_point(alpha = 0.1, color = "darkblue") +
  labs(x = "Revenue ($/week)",
       y = "Percentage of trips with rain (%)") +
  xlim(0,1500) + 
  ylim(0,100) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
ggsave("Plot rainy and revenue.jpg", plot = p, width = 4, height = 4, dpi = 300)

