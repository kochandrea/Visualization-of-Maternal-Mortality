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

# library(extrafont)
# font_import()


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
matdeath_data <- filter(WHO_data, 
                   indicator == "matdeaths", 
                   estimate == "point estimate",
                   rounded == "TRUE")

### merge WHO and WB data
new_data <- sqldf("SELECT * from matdeath_data 
                  LEFT OUTER join WorldBank_data 
                  ON matdeath_data.iso = WorldBank_data.Code")

show_years <- c(2015, 2010, 2005, 2000, 1995, 1990, 1985)
new_data <- subset(new_data, year %in% show_years)

### group and summarize into a new table
sum_region_data <- summarize(group_by(new_data, year, Region), 
                             total_matdeaths_region = round(sum(value, na.rm=TRUE), digits=0))


sum_year_data <- summarize(group_by(sum_region_data, year), 
                             total_matdeaths_year = sum(total_matdeaths_region, na.rm=TRUE))

merge_data <- merge(sum_region_data, sum_year_data, by="year")


# sum_year_data2 <- summarize(group_by(sum_region_data, year), 
#                            total_matdeaths_year = format(sum(total_matdeaths_region, na.rm=TRUE), big.mark = ","))
# merge_data2 <- merge(sum_region_data, sum_year_data2, by="year")

################
#              #
#     PLOT     #
#              #
################

ggplot(merge_data, aes(x = year, y = total_matdeaths_region, fill = Region)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rev(c("#003C30", "#01665E", "#35978F", 
                                   "#80CDC1", "#C7EAE5", "#DFC27D", "#BF812D"))) +
  # coord_flip() +
  # scale_x_reverse(breaks = seq(1985, 2015, 5)) +
  scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  geom_text(aes(y = total_matdeaths_year, label = total_matdeaths_year), 
            size = 4, vjust = -.7, colour = "black") +
  labs(title = "More maternal deaths occur in Sub-Saharan Africa",
       subtitle = "Total maternal deaths from 1985 to 2015, by world region",
       caption = "Sources:  The World Health Organization / The World Bank") +
  xlab("Year") +
  ylab("Total maternal deaths") +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size=9,
                               color = "black", angle = 45),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    
    legend.background = element_rect(fill = "#f2f2f2"),
    
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    plot.background = element_rect(fill = "#f2f2f2"),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(t=0,r=0,b=1.5,l=0,unit="cm")),
    plot.caption = element_text(size = 7, 
                                margin = margin(t=1,r=0,b=0,l=0,unit="in"), 
                                hjust = 1.5,
                                face = "italic"),
    plot.margin = unit(c(.75,.75,.75,.75),"in")
    )



#####################
#                   #
#   DATA SOURCES    #
#                   #
#####################

# The World Bank:  https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
# The World Health Organization:  https://www.who.int/reproductivehealth/publications/monitoring/maternal-mortality-2015/en/


