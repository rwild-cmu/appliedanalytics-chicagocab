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
library(ggmap)

#2) Data visualization based on processed data------

#Read data
df <- read.csv("weekly_stats_nonzero.csv")

#No differences on revenue between driver with and without a company
means <- df %>% mutate(company=ifelse(company=="", "w/taxi company", "w/o taxi company")) %>%
  group_by(company) %>%
  summarise(mean=which.max(tabulate(revenue)))
p <- ggplot(df %>% mutate(company=ifelse(company=="", "w/taxi company", "w/o taxi company")), 
       aes(x=revenue, color=as.factor(company))) +
  geom_density(fill="white", alpha=0.1, position="identity", size=1) +
  labs(x="Revenue per week ($/week)",
       title="Weekly revenue density based on taxi company, 2017") +
#  geom_vline(xintercept=means$mean, color=alpha("black", 0.6), linetype="dashed") +
  xlim(0,1500) +
  theme(legend.position=c(0.85,0.70), legend.background=element_blank(), 
        legend.title=element_text(face="bold", size=10),
        panel.background=element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(name = "Legend",
                     labels = c("w/taxi company", "w/o taxi company"),
                     values= c("green4", "royalblue4"))
ggsave("Plot taxi company and revenue.jpg", plot=p, width=7, height=6, dpi=72)


#Rain trips and revenue
p <- ggplot(df, aes(x=norain, y=revenue)) +
  geom_point(aes(color="No Rain"), alpha=0.05) +
  geom_point(aes(x=rain, y=revenue, color="Rain"), alpha=0.05) +
  labs(x="Number of taxi trips per week", 
       y="Revenue per week ($/week)",
       title="Correlation between weekly trips and revenue per driver, 2017") +
  theme(legend.position=c(0.85,0.90), legend.background=element_blank(), 
        legend.title=element_text(face="bold", size=10),
        panel.background=element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0,1500) + 
  xlim(0,100) +
  guides(colour=guide_legend(override.aes=list(alpha = 1))) +
  scale_color_manual(name = "Legend",
                     labels = c("No Rain", "Rain"),
                     values= c("green4", "royalblue4"))
ggsave("Plot trips and revenue.jpg", plot=p, width=7, height=6, dpi=72)


