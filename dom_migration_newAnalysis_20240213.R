library(dplyr)
#library(popmodules)
library(gglaplot)
library(ggplot2)
library(data.table)
library(lubridate)
library(plotly)
library(tidyverse)

devtools::load_all("Q:/Teams/D&PA/Demography/demogtools/R")

data_dir <- 'C:/Migration/Migration_R/DATA/Domestic/copied_from_Qdrive_20240213/'


chart_dir <- 'charts/'
colour_palette <- gla_colour_palette()[c(3,6,2,4,5,7,1)]

#-------------------------------------------------------------------------------

# read data

input_longit_full <- readRDS(paste0(data_dir, "origin_destination_lad_2002_2022.rds"))  %>%   data.frame()

summary(input_longit_full)
#gssOutList <- as.list(unique(input_longit_full$gss_out))
#

#-------------------------------------------------------------------------------
### OUT MIGRATION
#---------------------------------------------------------------------------------

## CREATE FILE WITH THOSE MOVING OUT FROM LONDON in 2021 - 2022

# Create domestic out-migration file selecting only for those originating in London
# Select only those who have destination out of London 
# Select only England and Wales
# The data are by single years and by gender.

input_london_dom_mign_out_2022 <- input_longit_full %>%
  filter(substr(gss_out,1,3) %in% c("E09")) %>% # "out" is origin
  filter(!(substr(gss_in,1,3) %in% c("E09"))) %>%   # "in" is destination
  filter(gss_in != "N92000002") %>% # Remove Northern Ireland
  filter(gss_in != "S92000003") %>% # Remove Scotland
  filter(year == "2022") %>%
#  group_by(gss_in) %>%
#  summarise(sumvalue_out = sum(value)) %>%
 # mutate(sumvalueK_out = sumvalue_out/1000) %>%
  data.frame() 

write.csv(input_london_dom_mign_out_2022, paste0(data_dir, "/london_origin_2022.csv"))

## CREATE FILE WITH THOSE MOVING OUT FROM LONDON in 2002 - 2022

# Create domestic out-migration file selecting only for those originating in London
# Select only those who have destination out of London 
# Select only England and Wales
# The data are by single years and by gender.

input_london_dom_mign_out_2002to2022 <- input_longit_full %>%
  filter(substr(gss_out,1,3) %in% c("E09")) %>% # "out" is origin
  filter(!(substr(gss_in,1,3) %in% c("E09"))) %>%   # "in" is destination
  filter(gss_in != "N92000002") %>% # Remove Northern Ireland
  filter(gss_in != "S92000003") %>% # Remove Scotland
  #  group_by(gss_in) %>%
  #  summarise(sumvalue_out = sum(value)) %>%
  # mutate(sumvalueK_out = sumvalue_out/1000) %>%
  data.frame() 

write.csv(input_london_dom_mign_out_2002to2022, paste0(data_dir, "/london_origin_2002to2022.csv"))




#------------------------------------------------------------------------
### IN MIGRATION
#---------------------------------------------------------------------------------

## CREATE FILE WITH THOSE MOVING INTO LONDON BETWEEN JUNE 2021 and JUNE 2022

# Create domestic in-migration file selecting only for those originating outside London
# Select only those who have destination in London 
# Select only England and Wales
# The data are by single years and by gender.

input_london_dom_mign_in_2022 <- input_longit_full %>%
  filter(substr(gss_in,1,3) %in% c("E09")) %>% # "in" is destination
  filter(!(substr(gss_out,1,3) %in% c("E09"))) %>%   # "out" is origin
  # so that data are comparable to out analysis, only include England and Wales
  filter(gss_out != "N92000002") %>% # Remove Northern Ireland
  filter(gss_out != "S92000003") %>% # Remove Scotland
  filter(year == "2022") %>%
 # group_by(gss_out) %>%
 # summarise(sumvalue_in = sum(value)) %>%
  data.frame() 

write.csv(input_london_dom_mign_in_2022, paste0(data_dir, "london_destination_2022.csv"))

input_london_dom_mign_in_2002to2022 <- input_longit_full %>%
  filter(substr(gss_in,1,3) %in% c("E09")) %>% # "in" is destination
  filter(!(substr(gss_out,1,3) %in% c("E09"))) %>%   # "out" is origin
  # so that data are comparable to out analysis, only include England and Wales
  filter(gss_out != "N92000002") %>% # Remove Northern Ireland
  filter(gss_out != "S92000003") %>% # Remove Scotland
  # group_by(gss_out) %>%
  # summarise(sumvalue_in = sum(value)) %>%
  data.frame() 

write.csv(input_london_dom_mign_in_2002to2022, paste0(data_dir, "london_destination_2002to2022.csv"))



#------------------------------------------------------------------------
### WITHIN LONDON MIGRATION
#---------------------------------------------------------------------------------

## CREATE FILE WITH THOSE MOVING BETWEEN LONDON BOROUGHS JUNE 2021 - JUNE 2022

# Create domestic in-migration file selecting only for those moving within London
# Select only those who have destination in London 
# Select only those who have origin in London 
# The data are by single years and by gender.

input_london_dom_mign_within_2022 <- input_longit_full %>%
  filter(substr(gss_in,1,3) %in% c("E09")) %>% # "in" is destination
  filter(substr(gss_out,1,3) %in% c("E09")) %>%   # "out" is origin
  filter(year == "2022") %>%
  # group_by(gss_out) %>%
  # summarise(sumvalue_in = sum(value)) %>%
  data.frame() 

write.csv(input_london_dom_mign_within_2022, paste0(data_dir, "london_within_2022.csv"))

input_london_dom_mign_within_2002to2022 <- input_longit_full %>%
  filter(substr(gss_in,1,3) %in% c("E09")) %>% # "in" is destination
  filter(substr(gss_out,1,3) %in% c("E09")) %>%   # "out" is origin
  # group_by(gss_out) %>%
  # summarise(sumvalue_in = sum(value)) %>%
  data.frame() 

write.csv(input_london_dom_mign_within_2002to2022, paste0(data_dir, "london_within_2002to2022.csv"))





#---------------------------------


## NOTHING BELOW IS USED IN FEB


## OUT-MIGRATION
# Select only for those originating in London

colours = c("Female" = "blue", "Male" = "red")

london_longit_origin2 <- input_longit_full %>% 
  filter(grepl('E09', gss_out)) %>% # out is origin
  data.frame() 

# calculate total out migration by sex by year
london_longit_origin_sex2 <- london_longit_origin2 %>% 
  group_by(year, sex) %>% 
  summarise(sumvalue = sum(value), .groups = 'drop_last') %>%
  mutate(sumvalueK = sumvalue/1000) %>% 
    data.frame() 

# Line chart of out migration for London by sex, 2002 - 2022
london_longit_origin_sex_line2 <- ggplot(data = london_longit_origin_sex2, aes(x=year, y=sumvalueK, color=sex)) + 
  theme_gla(y_axis_title = TRUE, free_y_facets = TRUE) +
  theme(axis.title.y = element_text(size = rel(0.9)))+
  theme(plot.margin = margin(30,30,30,30))+
  geom_line(size=2) +
  #scale_color_manual(name = "", values = colours) +
  labs(title = "Domestic migration from London by gender, 2002 - 2022", 
       subtitle = "",
       caption = "Source: ONS; Chart: GLA demography\nYear refers to year ending June",
       y = "Outflow (thousands)")
london_longit_origin_sex_line2  
  
# calculate total out migration by age group by year
london_longit_origin_age2 <- london_longit_origin2 %>% 
  mutate(agegroup = cut(age, c(-1,17, 24,44,110), 
                        labels = c("0-17", "18 - 24", "25 - 44", "45+"))) %>%
  group_by(year, agegroup) %>% 
  summarise(sumvalue = sum(value), .groups = 'drop_last') %>%
  mutate(sumvalueK = sumvalue/1000) %>% 
  data.frame() 

my_palette <- gla_colour_palette()[c( 1, 7, 4, 3, 2, 6, 5)]

# Line chart of out migration from London by age group, 2002 - 2022
london_longit_origin_age_line2 <- ggplot(data = london_longit_origin_age2, aes(x=year, y=sumvalueK, color=agegroup)) + 
  theme_gla(y_axis_title = TRUE, free_y_facets = TRUE) +
  theme(axis.title.y = element_text(size = rel(0.9)))+
  theme(plot.margin = margin(30,30,30,30))+
  geom_line(size=2) +
  #scale_color_manual(name = "", values = colours) +
  labs(title = "Domestic migration from London by age group, 2002 - 2022", 
       subtitle = "",
       caption = "Source: ONS; Chart: GLA demography\nYear refers to year ending June",
       y = "Outflow (thousands)")
london_longit_origin_age_line2

# Stacked area chart of out migration from London by age group, 2002 - 2022
london_longit_origin_age_area2 <- london_longit_origin_age2 %>% 
  ggplot(aes(x=year, y=sumvalueK, fill=agegroup, group=agegroup)) + 
  labs(y="Thousands per year") +
  geom_area(alpha = 0.8) +
  theme_gla(y_axis_title = TRUE) +
  scale_fill_manual(values=my_palette) +
  labs(title = "Domestic migration from London by age group, 2002 - 2022",
       caption = paste0("Source: Home Office, Chart: GLA demography.\n",
                        "Year refers to year ending June"))
london_longit_origin_age_area2



#-------------------------------------------------------------------------------

# IN-MIGRATION
# select only for those with destination London

colours = c("Female" = "blue", "Male" = "red")

london_longit_destination2 <- input_longit_full %>% 
  filter(grepl('E09', gss_in)) %>% # out is origin
  data.frame() 


# calculate total in migration by sex by year
london_longit_destination_sex2 <- london_longit_destination2 %>% 
  group_by(year, sex) %>% 
  summarise(sumvalue = sum(value), .groups = 'drop_last') %>%
  mutate(sumvalueK = sumvalue/1000) %>% 
  data.frame() 

# Line chart of in migration for London by sex, 2002 - 2022
london_longit_destination_sex_line2 <- ggplot(data = london_longit_destination_sex2, aes(x=year, y=sumvalueK, color=sex)) + 
  theme_gla(y_axis_title = TRUE, free_y_facets = TRUE) +
  theme(axis.title.y = element_text(size = rel(0.9)))+
  theme(plot.margin = margin(30,30,30,30))+
  geom_line(size=2) +
  #scale_color_manual(name = "", values = colours) +
  labs(title = "Domestic migration to London by gender, 2002 - 2022", 
       subtitle = "",
       caption = "Source: ONS; Chart: GLA demography\nYear refers to year ending June",
       y = "inflow (thousands)")
london_longit_destination_sex_line2  

# calculate total in migration by age group by year
london_longit_destination_age2 <- london_longit_destination2 %>% 
  mutate(agegroup = cut(age, c(-1,17, 24,44,110), 
                        labels = c("0-17", "18 - 24", "25 - 44", "45+"))) %>%
  group_by(year, agegroup) %>% 
  summarise(sumvalue = sum(value), .groups = 'drop_last') %>%
  mutate(sumvalueK = sumvalue/1000) %>% 
  data.frame() 

my_palette <- gla_colour_palette()[c( 1, 7, 4, 3, 2, 6, 5)]

# Line chart of in migration to London by age group, 2002 - 2022
london_longit_destination_age_line2 <- ggplot(data = london_longit_destination_age2, aes(x=year, y=sumvalueK, color=agegroup)) + 
  theme_gla(y_axis_title = TRUE, free_y_facets = TRUE) +
  theme(axis.title.y = element_text(size = rel(0.9)))+
  theme(plot.margin = margin(30,30,30,30))+
  geom_line(size=2) +
  #scale_color_manual(name = "", values = colours) +
  labs(title = "Domestic migration to London by age group, 2002 - 2022", 
       subtitle = "",
       caption = "Source: ONS; Chart: GLA demography\nYear refers to year ending June",
       y = "inflow (thousands)")
london_longit_destination_age_line2

# Stacked area chart of in migration to London by age group, 2002 - 2022
london_longit_destination_age_area2 <- london_longit_destination_age2 %>% 
  ggplot(aes(x=year, y=sumvalueK, fill=agegroup, group=agegroup)) + 
  labs(y="Thousands per year") +
  geom_area(alpha = 0.8) +
  theme_gla(y_axis_title = TRUE) +
  #scale_fill_manual(values=my_palette) +
  labs(title = "Domestic migration to London by age group, 2002 - 2022",
       caption = paste0("Source: Home Office, Chart: GLA demography.\n",
                        "Year refers to year ending June"))
london_longit_destination_age_area2



#-----------------------------------------------------------------------------

# Adding net migration to the line chart and removing gender

## OUT-MIGRATION
# Includes only those originating in London

# calculate total out migration by year
london_longit_origin3 <- london_longit_origin2 %>% 
  group_by(year) %>% 
  summarise(sumvalue_out = sum(value), .groups = 'drop_last') %>%
  mutate(sumvalue_outK = sumvalue_out/1000) %>% 
  data.frame() 

# IN-MIGRATION
# Includes only those with destination London

# calculate total in migration by year
london_longit_destination3 <- london_longit_destination2 %>% 
  group_by(year) %>% 
  summarise(sumvalue_in = sum(value), .groups = 'drop_last') %>%
  mutate(sumvalue_inK = sumvalue_in/1000) %>% 
  data.frame() 

# match and calculate net migration by year
london_longit_net_wide2 <- merge(london_longit_origin3, london_longit_destination3, by = 'year') %>%
  mutate(sumvalue_netK = sumvalue_inK - sumvalue_outK) %>% 
  data.frame() 

write.csv(london_longit_net_wide2, paste0(data_dir, "london_domMig_net_longit_wide2.csv"))

# Line chart of net migration for London, 2002 - 2022
london_longit_net_line2 <- ggplot(data = london_longit_net_wide2, aes(x = year)) +
  geom_line(aes( y = sumvalue_inK), color = "red") +
  geom_line(aes( y = sumvalue_outK), color = "blue") +
  geom_line(aes( y = sumvalue_netK), color = "green") +
  theme_gla(y_axis_title = TRUE, free_y_facets = TRUE) +
  theme(axis.title.y = element_text(size = rel(0.9)))+
  theme(plot.margin = margin(30,30,30,30))+
  #geom_line(size=2) +
  #scale_y_continuous(limits = c(-200, 200))+
  labs(title = "Domestic migration, to and from London, 2002-22", 
       subtitle = "",
       caption = "Source: ONS; Chart: GLA demography\nYear refers to year ending June",
       y = "flow (thousands)")
london_longit_net_line2 

# Interactive bar chart using Wil's code from chart club
london_longit_net_int2  <- london_longit_net_wide2 %>% 
  plot_ly(x=~year, y=~sumvalue_inK, 
          type = "bar",
          color = I("#6da7de"), 
          name = "Migration to London", 
          hovertemplate = paste("Year: %{x|%Y}",
                      "<br>", "Thousands migrating to London: %{y:.0f}<extra></extra>")) %>%
      #add additional series as 'trace' variables
  add_trace(y = ~sumvalue_outK, type = "bar", color = I('#9e0059'),
            name = 'Migration from London',
            hovertemplate = paste("Year: %{x|%Y}",
                                  "<br>", "Thousands migrating from London: %{y:.0f}<extra></extra>")) %>% 
  add_trace(y = ~sumvalue_netK, 
            name = "Net domestic migration",
            type = "scatter",
            mode = "line", 
            line = list(shape = "spline", color = "black"),
            hovertemplate = paste("Year: %{x|%Y}",
                                  "<br>", "London's net domestic migration (thousands): %{y:.0f}<extra></extra>")) %>%
  layout(
      #title
    title = list(text = paste0('Domestic Migration, London',"<br>","<sup>","Thousands"), x = 0.1),
      #axis options
    xaxis = list(title = "", showgrid = FALSE),
    yaxis = list(title = "", showgrid = FALSE),
      #legend
    legend = list(orientation = "h",   # show entries horizontally
                  x = 0, y = 1.05, # position
                  font = list(size = 10),
                  bgcolor = 'rgba(0,0,0,0)'), # legend background transparent
      #Source at bottom of chart
    annotations = list(x = 0, y = -0.2, text = "Source: ONS. Chart: GLA demography", 
                       showarrow = F, xref='paper', yref='paper', 
                       xanchor='left', yanchor='auto', xshift=0, yshift=0,
                       font=list(size=11, color="grey"))) %>%
    layout(margin = list(l = 50, r = 50, b = 80, t = 50))
london_longit_net_int2


# Create long datafile instead of wide 

london_longit_net_long2 <- london_longit_net_wide2 %>%
  select(year, sumvalue_inK, sumvalue_outK, sumvalue_netK) %>%
  gather(key = "direction", value = "value", -year)
summary(london_longit_net_long)

ggplot(london_longit_net_long2, aes(x = year, y = value)) + 
  theme_gla(y_axis_title = TRUE, free_y_facets = TRUE) +
  geom_line(aes(color = as.factor(direction), linetype = as.factor(direction))) + 
  scale_color_manual(values = c("darkred", "steelblue", "darkgreen"))



