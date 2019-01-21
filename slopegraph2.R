rm(list=ls())

library(tidyverse)
library(dplyr)
library(CGPfunctions)
require(dplyr)
require(ggplot2)
require(ggrepel)
require(kableExtra)
library(readxl)
library(sqldf)
library(RColorBrewer)


##################
#                #
#   PREP DATA    #
#                #
##################

### read in World Health Organization data
WHO_data <- read.csv("MMR-data-1990-2015 (1)/countryresults_all.csv")

### read and clean World Bank data
WorldBank_data <- read_excel("world bank data.xlsx")
WorldBank_data <- WorldBank_data[-c(1),]
WorldBank_data <- WorldBank_data[,-c(1,2,5)]

### create a new table of maternal mortality ratio point estimates
mmr_data <- filter(WHO_data, 
                     indicator == "mmr", 
                     estimate == "point estimate",
                     rounded == "FALSE")

### merge WHO and WB data
new_data <- sqldf("SELECT * from mmr_data 
                  LEFT OUTER join WorldBank_data 
                  ON mmr_data.iso = WorldBank_data.Code")

### group and summarize into a new table
sum_region_data <- summarize(group_by(new_data, year, Region), 
                             avg_mmr=round(mean(value, na.rm=TRUE), digits=0))
show_years <- c(2015, 2010, 2005, 2000, 1995, 1990, 1985)
sum_region_data <- subset(sum_region_data, year %in% show_years)


################
#              #
#     PLOT     #
#              #
################

ggplot(data = sum_region_data, 
       aes(x = year, y = avg_mmr, group = Region)) +
  geom_line(aes(color = year, alpha = 2)) +
  geom_point(aes(color = year, alpha = 2), size = 2) +
  # scale_color_manual(values = c("grey", "red", "blue")) +
  # scale_size_manual(values = c(0.25,1,1)) +
  geom_text_repel(data = sum_region_data %>% filter(year == 1985),
                  aes(label = paste0(Region, " - ", avg_mmr)) ,
                  hjust = "left",
                  # fontface = "bold",
                  size = 3,
                  nudge_x = -.45,
                  direction = "y") +
  geom_text_repel(data = sum_region_data %>% filter(year == 2015),
                  aes(label = paste0(Region, " - ", avg_mmr)) ,
                  hjust = "right",
                  # fontface = "bold",
                  size = 3,
                  nudge_x = .5,
                  direction = "y") +
  scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  labs(title = "Maternal mortality ratio on the decline, though not enough for \nchildbearing women in Sub-Saharan African countries",
       subtitle = "Average maternal mortality ratio (deaths per 100,000 live births) by global region",
       caption = "Sources:  The World Health Organization / The World Bank") +
  xlab("Year") +
  ylab("Average number of maternal deaths per 100,000 live births") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(margin = margin(t=1, r=0, b=0, l=0, unit="cm")),
    axis.title.y = element_text(margin = margin(t=0, r=1, b=0, l=0, unit="cm")),
    axis.text.x = element_text(size=9,
                               color = "black", 
                               margin = margin(t=.5, r=0, b=0, l=0, unit="cm")),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    
    legend.position = "none",
    
    panel.border = element_blank(),
    panel.background = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(t=0,r=0,b=1.5,l=0,unit="cm")),
    plot.caption = element_text(size = 7, 
                                margin = margin(t=1,r=0,b=0,l=0,unit="in"), 
                                hjust = 1,
                                face = "italic"),
    plot.margin = unit(c(.5,.5,.5,.5),"in")
  )



#####################
#                   #
#   DATA SOURCES    #
#                   #
#####################

# The World Bank:  https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
# The World Health Organization:  https://www.who.int/reproductivehealth/publications/monitoring/maternal-mortality-2015/en/


