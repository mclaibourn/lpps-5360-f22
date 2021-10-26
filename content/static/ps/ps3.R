---
title: "Problem Set 3"
author: "Michele Claibourn"
date: "11/02/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# World Bank Development Indicators

Read in the WDI data we downloaded in ps3_prep.R and have a peek.

```{r, message = FALSE}
library(tidyverse)
# add other needed libraries here

wdi <- read_csv("data/wdi_clean.csv")

glimpse(wdi)
```

Region and income are characters, let's have a look.

```{r}
wdi %>% count(region)

wdi %>% count(income)
```


## 1. Data Prep

Make both region and income factors and set the levels of income in a reasonable way. Then make a bar graph of each (individually), choosing colors with intention (e.g., not the defaults).

## 2. Overview of Relationships

Make EITHER a scatterplot matrix or a correlogram to visualize the relationships between the key quantitative variables in this data set (that is, the three life expectancy variables, access to electricity, gdp per capita, CO2 emissions, and the percent in extreme poverty). What stands out?


## 3. Scatterplots!

1. Pick any year (between 2008 and 2018) and any two quantitative variables from above to focus on. Using just the data for your chosen year, make an initial scatterplot of your selected variables.

2. Try multiple additional versions, including (separately or simultaneously, it's up to you)
   - color the points by income
   - facet by region the figure by region 
   - add a size aesthetic mapped to another quantitative variable

3. Based on the experimentation above, generate one final scatterplot you find the most useful; choose the colors for this final figure intentionally (e.g., not the defaults).


## 4. Change!

1. Using just the county-level data in 2008 and 2018, create EITHER a slope graph or a dumbbell plot to visualize the change/difference in overall life expectancy across these two periods.

2. Recreate the above graph for female life expectancy and for male life expectancy. For these, choose the colors with intention. Save each in a named object and use `patchwork` to plot them together.