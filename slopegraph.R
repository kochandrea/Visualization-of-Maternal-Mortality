library(tidyverse)
library(dplyr)
library(CGPfunctions)
require(dplyr)
require(ggplot2)
require(ggrepel)
require(kableExtra)
library(readxl)

WHO_country <- read.csv("MMR-data-1990-2015 (1)/countryresults_all.csv")

### create a new table of mmr point estimates
global_mmr <- filter(WHO_country, 
                     indicator == "mmr", 
                     estimate == "point estimate",
                     rounded == "FALSE")

### subset dataframes into low-, low-middle-, upper-middle-, upper-income economy groups; from World Bank 

country_by_economy <- read_excel("country_by_economy.xlsx")

upper_income <- filter(country_by_economy, economy == "High income")
low_income <- filter(country_by_economy, economy == "Low income")
lower_middle_income <- filter(country_by_economy, economy == "Lower middle income")
upper_middle_income <- filter(country_by_economy, economy == "Upper middle income")

mmr_upper_inc <- subset(global_mmr, iso %in% upper_income$iso)
mmr_low_inc <- subset(global_mmr, iso %in% low_income$iso)
mmr_lower_mid_inc <- subset(global_mmr, iso %in% lower_middle_income$iso)
mmr_upper_mid_inc <- subset(global_mmr, iso %in% upper_middle_income$iso)

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



set_theme <- theme(legend.position = "none", 
                   panel.border = element_blank(),  
                   # axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   axis.ticks = element_blank(),
                   # axis.title.x = element_blank(),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   axis.text.x = element_text(size=8, 
                                              angle=45, 
                                              margin = margin(t=-5))
                   )


ggplot(data = mmr_upper_inc_rank_by_year, 
       aes(x = year, y = rev(rank), group = iso, color = factor(highlight_country), size = factor(highlight_country))) +
  geom_line(alpha = 0.5) +
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
  # change to black and white theme:
  theme_bw() + 
  set_theme +
  labs(title = "Among high income countries, United States falls in ranking of maternal mortality ratios; Poland shows greatest rise in rank",
    subtitle = "Countries ranked by maternal mortality ratio (1 = lowest number of maternal deaths per 100,000 live births)",
    caption = "Sources: https://www.who.int/reproductivehealth/publications/monitoring/maternal-mortality-2015/en/ and ") +
  xlab("Year") +
  ylab("Ranking (1 = lowest number maternal deaths per 100,000 live births)")
