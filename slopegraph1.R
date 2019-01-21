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
library(gridExtra)
library(grid)


##################
#                #
#   PREP DATA    #
#                #
##################

WHO_data <- read.csv("MMR-data-1990-2015 (1)/countryresults_all.csv")

### create a new table of mmr point estimates
mmr_data <- filter(WHO_data, 
                     indicator == "mmr", 
                     estimate == "point estimate",
                     rounded == "FALSE")

### subset dataframes into low-, low-middle-, upper-middle-, upper-income economy groups; from World Bank 

WorldBank_data <- read_excel("country_by_economy.xlsx")

upper_income <- filter(WorldBank_data, economy == "High income")
low_income <- filter(WorldBank_data, economy == "Low income")
lower_middle_income <- filter(WorldBank_data, economy == "Lower middle income")
upper_middle_income <- filter(WorldBank_data, economy == "Upper middle income")

mmr_upper_inc <- subset(mmr_data, iso %in% upper_income$iso)
mmr_low_inc <- subset(mmr_data, iso %in% low_income$iso)
mmr_lower_mid_inc <- subset(mmr_data, iso %in% lower_middle_income$iso)
mmr_upper_mid_inc <- subset(mmr_data, iso %in% upper_middle_income$iso)

mmr_upper_inc_rank_by_year <- arrange(mmr_upper_inc, year, value) %>% 
  group_by(year) %>%              
  mutate(rank = order(value))

mmr_upper_mid_inc_rank_by_year <- arrange(mmr_upper_mid_inc, year, value) %>% 
  group_by(year) %>%              
  mutate(rank = order(value))

mmr_lower_mid_inc_rank_by_year <- arrange(mmr_lower_mid_inc, year, value) %>% 
  group_by(year) %>%              
  mutate(rank = order(value))

mmr_low_inc_rank_by_year <- arrange(mmr_low_inc, year, value) %>% 
  group_by(year) %>%              
  mutate(rank = order(value))


### Find change in rank from 1985 to 2015

Overall_rank_change_upper <-  subset(mmr_upper_inc_rank_by_year, year %in% c(1985, 2015))
drops <- c("estimate", "rounded", "indicator", "value", "Year_formatted")
Overall_rank_change_upper <- Overall_rank_change_upper[, !(names(Overall_rank_change_upper) %in% drops)]
data_wide <- spread(Overall_rank_change_upper, year, rank)
data_wide[3:4] <- lapply(data_wide[3:4], as.numeric)
data_wide <- mutate(data_wide, rank_change = data_wide$`1985` - data_wide$`2015`)
# looking at the change in rank, the U.S. has fallen the most 
# and Poland has increased the most

### show only every 5 years 
mmr_upper_inc_rank_by_year$Year_formatted <- as.character(mmr_upper_inc_rank_by_year$year)
show_years <- c(2015, 2010, 2005, 2000, 1995, 1990, 1985)
mmr_upper_inc_rank_by_year <- subset(mmr_upper_inc_rank_by_year, year %in% show_years)

### note rows for USA and POL, so that they can be highlighted later in visualization
mmr_upper_inc_rank_by_year <- mutate(mmr_upper_inc_rank_by_year,
                                     highlight_country = case_when(iso == "USA" ~ 1,
                                                                   iso == "POL" ~ 2,
                                                                   TRUE ~ 0))



################
#              #
#     PLOT     #
#              #
################



sg <- ggplot(data = mmr_upper_inc_rank_by_year, 
       aes(x = year, y = rev(rank), group = iso, color = factor(highlight_country), size = factor(highlight_country))) +
  geom_line(alpha = 1) +
  scale_color_manual(values = c("grey", "red", "blue")) +
  scale_size_manual(values = c(0.25,1,1)) +
  geom_text(data = mmr_upper_inc_rank_by_year %>% filter(year == 1985),
            aes(label = iso) ,
            hjust = 1.5,
            fontface = "bold",
            size = 2) +
  geom_text(data = mmr_upper_inc_rank_by_year %>% filter(year == 2015),
            aes(label = iso) ,
            hjust = -1,
            fontface = "bold",
            size = 2) +
  geom_label(aes(label = rank), 
             size = 2.5, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0) +
  # show all years on x-axis:
  scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  labs(subtitle = "Ranking of maternal mortality ratios of high-income* countries",
       caption = "*as categorized by the World Bank's 2019 fiscal year estimates \n\nSources: The World Health Organization / The World Bank") +
  # labs(title = "United States falls in rank of maternal mortality ratio; Poland on the rise",
  #      subtitle = "Ranking of maternal mortality ratios of high-income* countries",
  #      caption = "*as categorized by the World Bank's 2019 fiscal year estimates \n\nSources: The World Health Organization / The World Bank") +
  xlab("Year") +
  ylab("Rank \n(1 = lowest number maternal deaths per 100,000 live births)") +
  # change to black and white theme:
  theme_bw() + 
  theme(
    axis.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(margin = margin(t=1, r=0, b=0, l=0, unit="cm")),
    axis.title.y = element_text(margin = margin(t=0, r=.5, b=0, l=0, unit="cm")),
    axis.text.x = element_text(size=8, 
                               angle=45, 
                               margin = margin(t=.3, r=0, b=0, l=0, unit = "cm")),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    
    legend.position = "none", 
    
    panel.border = element_blank(),  
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    # plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(t=0,r=0,b=1.5,l=0,unit="cm")),
    plot.caption = element_text(size = 7,
                                margin = margin(t=.35,r=0,b=0,l=0,unit="in"),
                                hjust = 1,
                                face = "italic"),
    plot.margin = unit(c(.5,.5,.5,.5),"cm")
  )



title = c("United States", " falls in ranking of maternal mortality ratio; ", "Poland", " on the rise")
colors = c("red", "black", "blue", "black")
grid.arrange(sg, 
             top = tableGrob(t(title), 
                             theme=ttheme_minimal(padding=unit(c(0,2),'mm'),
                                                  base_colour = colors)))

#####################
#                   #
#   DATA SOURCES    #
#                   #
#####################

# The World Bank:  https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
# The World Health Organization:  https://www.who.int/reproductivehealth/publications/monitoring/maternal-mortality-2015/en/


