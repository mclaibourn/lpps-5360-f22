# # Data Viz in R
# 2021-11-11
# Practicing with R
# Supplemental Examples


# ..................................................
# Load libraries and data ----
# Libraries
library(tidyverse)
library(scales)

# Albemarle County property data
property <- readRDS("data/property_cleaned.RDS")



# ..................................................
# Bar plot with values  ----
# Initial plot
p <- property %>% 
  filter(magisterialdistrict != "Unassigned") %>% 
  ggplot(aes(x = fct_rev(fct_infreq(magisterialdistrict)))) +
  geom_bar() +
  labs(x = "", y = "Number of Residential Properties") +
  coord_flip()

p

# Create a vector of values
mdprop <- property %>% 
  filter(magisterialdistrict != "Unassigned") %>% 
  group_by(magisterialdistrict) %>% 
  summarize(num = n())

# And add them to the figure at base of bar
p + geom_text(data = mdprop, 
              aes(x = magisterialdistrict, y = 100, label = comma(num)), 
              color = "white", hjust = 0)
# hjust tells the text to start at the y-value rather than center on the y-value
# I added the scales::comma() function on the number value to make it easier to read

# And add them to the figure at top of bar
p + geom_text(data = mdprop, 
              aes(x = magisterialdistrict, y = num+100, label = comma(num)), 
              color = "blue", hjust = 0, size = 3) +
  expand_limits(y = c(0, 9000)) 
# I had to extend the axis to the right to make space for the values


# ..................................................
# Reordering the legend  ----
# Initial graph
property %>% 
  filter(magisterialdistrict != "Unassigned") %>% 
  ggplot(aes(x = fct_rev(fct_infreq(magisterialdistrict)),
             fill = era)) +
  geom_bar() +
  labs(x = "", y = "Number of Residential Properties") +
  coord_flip() +
  theme(legend.position = "bottom")

# I want the legend to appear in the order of the fill on the bars
# a. Reorder the legend
property %>% 
  filter(magisterialdistrict != "Unassigned") %>% 
  ggplot(aes(x = fct_rev(fct_infreq(magisterialdistrict)),
             fill = era)) +
  geom_bar() +
  scale_fill_discrete(guide = guide_legend(reverse=TRUE)) +
  labs(x = "", y = "Number of Residential Properties") +
  coord_flip() +
  theme(legend.position = "bottom")
# added the scale_fill_discrete function

# I might have wanted to switch the sequence of the fill
# (so unknown is added first and post-2000 is added last)
# and make sure the legend appears in the same sequence
property %>% 
  filter(magisterialdistrict != "Unassigned") %>% 
  ggplot(aes(x = fct_rev(fct_infreq(magisterialdistrict)),
             fill = fct_rev(era))) +
  geom_bar() +
  scale_fill_discrete(guide = guide_legend(reverse=TRUE),
                      name = "Era Built") +
  labs(x = "", y = "Number of Residential Properties") +
  coord_flip() +
  theme(legend.position = "bottom")
# added fct_rev to fill
