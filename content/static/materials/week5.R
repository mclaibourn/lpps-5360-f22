# Data Viz in R
# 2021-11-09
# Practicing with R


# ..................................................
# Load libraries and data ----
# Libraries

library(tidyverse)
library(skimr)
library(scales)

library(plotly)
library(knitr)
library(kableExtra)
library(DT)


# Albemarle County property data
property <- readRDS("data/property_cleaned.RDS")


# ..................................................
# ggplotly  ----

## Scatterplot ----
# Initial graph 
p <- property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  ggplot(aes(x = lastsaleprice/1000, y = totalvalue/1000, 
             color = hsdistrict, size = lotsize)) +
  geom_point(pch = 21) +
  geom_abline(intercept = 0, slope = 1, color = "grey80") +
  scale_y_continuous(labels=dollar_format(prefix="$",suffix=" k")) +
  scale_x_continuous(labels=dollar_format(prefix="$",suffix=" k")) +
  scale_color_manual(
    values = c("red", "gold", "blue"),
    name = "HS District") + 
  guides(size = "none") +
  labs(x = "2020 Sale Price", y = "2020 Assessed Value") +
  theme(legend.position = "top")

p

# ggplotly it!
ggplotly(p)

# improve it 
# Let's add additional information to the tooltips
# and reposition the legend
p <- property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  ggplot(aes(x = lastsaleprice/1000, y = totalvalue/1000, 
             color = hsdistrict, size = lotsize,
             text = paste("House size (sqft):", finsqft))) +
  geom_point(pch = 21) +
  geom_abline(intercept = 0, slope = 1, color = "grey80") +
  scale_y_continuous(labels=dollar_format(prefix="$",suffix=" k")) +
  scale_x_continuous(labels=dollar_format(prefix="$",suffix=" k")) +
  scale_color_manual(
    values = c("red", "gold", "blue"),
    name = "HS District") + 
  guides(size = "none") +
  labs(x = "2020 Sale Price", y = "2020 Assessed Value") +
  theme(legend.position = "top")

p

ggplotly(p) %>% 
  layout(legend = list(orientation = "h", x = -0.1, y = 1.1))


## Line plot  ----

sale_byyear <- property %>% 
  filter(last_sale_year > 1980 & last_sale_year < 2021,
         lastsaleprice > 0,
         usecode %in% c("Single Family", "Single Family-Rental",
                        "Multi-Family", "Mobile Home",
                        "Doublewide", "3-4 Family")) %>% 
  group_by(last_sale_year, fips_tract) %>% 
  summarize(med_price = median(lastsaleprice, na.rm = TRUE),
            mean_price = mean(lastsaleprice, na.rm = TRUE),
            num_sales = n()) 

p2 <- ggplot(sale_byyear, aes(x = last_sale_year, y = med_price,
                              text = paste("Number of Sales:", num_sales))) +
  geom_line(aes(group = fips_tract)) +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  labs(x = "Year of Sale", y = "Sale Price")

ggplotly(p2)


# ..................................................
# Tables: kable  ----

tab <- property %>% 
  filter(totalvalue > 1000000) %>% 
  arrange(desc(totalvalue)) %>% 
  select(yearbuilt, finsqft, lotsize, 
              totalvalue, cardnum, owner)
tab

tab %>% 
  kbl()

# add styling: defaults to bootstrap theme
tab %>% 
  kbl(align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# change theme
tab %>% 
  kbl(align = "c") %>%
  kable_paper("hover", full_width = F)

# boostrap_options include: striped, hover, condensed, responsive
# Other options: full_width, position, font_size, etc.

# scrolling
tab %>% 
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  scroll_box(height = "500px")


## Summary tables ----
# numerica varaibles
sumtab <- property %>% 
  select(yearbuilt, finsqft, lotsize, totalvalue) %>% 
  skim() 

sumtab %>% 
  select(skim_variable, complete_rate, numeric.p50,
         numeric.mean, numeric.sd) %>% 
  kbl(col.names = c('Variable', 'Percent Non-Missing', 'Median', 'Mean', 'Std. Dev.'),
      align = "c",
      caption = "Albemarle County Residential Property: Summary Statistics") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = FALSE)

# Factor variables
sumtab2 <- property %>% 
  select(condition, grade_reduced, era, landuseprimary, magisterialdistrict, esdistrict) %>% 
  skim() 

sumtab2

sumtab2 %>% 
  select(skim_variable, complete_rate, factor.n_unique,
         factor.top_counts) %>% 
  kbl(col.names = c('Variable', 'Percent Non-Missing', 'Categories', 'Most Frequent'),
      align = "c",
      caption = "Albemarle County Residential Property: Summary Statistics") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))


# ..................................................
# Tables: DT  ----

# Default
datatable(tab)

# Change column names, add a caption
datatable(tab,
          colnames = c('Year Built', 'Finished SqFt', 'Lot Size (acres)', 'Assessed Value', 'Number of Buildings', 'Owner'),
          caption = 'Albemarle County Residential Property')

# Make columns individually filterable, change pagelength
datatable(tab, filter = 'top', options = list(pageLength = 5))


## Bonus: Pictograms?

# * https://buzzrbeeline.blog/2018/06/13/fun-and-easy-r-graphs-with-images/
# * https://evamaerey.github.io/little_flipbooks_library/ggtextures/ggtextures#1
# * https://ryo-n7.github.io/2019-01-11-visualize-asian-cup/
  