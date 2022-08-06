# Data Viz in R
# 2021-10-26
# Practicing with R

# To use the tidycensus package you need a census api key
# http://api.census.gov/data/key_signup.html
# Then you need to set your census api key
# census_api_key("YOUR API KEY GOES HERE", install = TRUE)
# install = TRUE will add the key to your .Renviron for future use
# Learn more about tidycensus
# https://walker-data.com/tidycensus/index.html


# ..................................................
# Load libraries and data ----
# Libraries
library(tidyverse)
library(tidycensus)
library(GGally) # for ggpairs/ggcorr
library(ggforce) # for sina plot
library(patchwork) # for combining plots!

# For color stuff
library(scales) 
library(RColorBrewer)
library(viridis)
library(rcartocolor)
library(wesanderson)
library(ghibli)

# Data
property <- readRDS("data/property_cleaned.RDS")


# ..................................................
# Pull American Community Survey data ----
# B25003 Tenure (number of owning and renting households by tract)
# From data.census.gov: https://data.census.gov/cedsci/table?q=b25003&g=0500000US51003%241400000&tid=ACSDT5Y2019.B25003&hidePreview=true

tenure <- get_acs(geography = "tract",
                  table = "B25003",
                  survey = "acs5",
                  year = 2019,
                  state = 51,
                  county = 003,
                  output = "wide")

# rename vars and generate proportion of owning households
tenure <- tenure %>% 
  rename(totalhh = B25003_001E,
         ownerhh = B25003_002E,
         renterhh = B25003_003E,
         totalhh_moe = B25003_001M,
         ownerhh_moe = B25003_002M,
         renterhh_moe = B25003_003M) %>% 
  mutate(prop_owner = ownerhh/totalhh,
         prop_owner_moe = moe_prop(ownerhh, totalhh, ownerhh_moe, totalhh_moe),
         prop_owner_high = ifelse(prop_owner > median(prop_owner), "GE 70%", "LT 70%"))

# graph: dotplot
ggplot(tenure, aes(x = ownerhh, y = fct_reorder(GEOID, ownerhh))) +
  geom_point()


# ..................................................
# Pivot (longer/wider)  ----
# we could have pulled the data initially into "long" form

tenure_long <- get_acs(geography = "tract",
                       table = "B25003",
                       survey = "acs5",
                       year = 2019,
                       state = 51,
                       county = 003,
                       output = "tidy")

# rename vars
tenure_long <- tenure_long %>% 
  mutate(varname = case_when(
    variable == "B25003_001" ~ "totalhh",
    variable == "B25003_002" ~ "ownerhh",
    variable == "B25003_003" ~ "renterhh"
  ))

# graph: dotplot
tenure_long %>% 
  filter(varname == "ownerhh") %>% 
  ggplot(aes(x = estimate, y = fct_reorder(GEOID, estimate))) +
  geom_point()

# or we could transform the wide data into long form
# pivot_longer
tenure_pivot_long <- tenure %>% 
  pivot_longer(-c(GEOID, NAME), 
               names_to = "variable", 
               values_to = "estimate")

# But now we have moes and estimates mixed up; 
# to get the same format as above, we need to widen it a bit
tenure_pivot_long <- tenure_pivot_long %>% 
  mutate(moe = ifelse(str_detect(variable, "moe"), "moe", "estimate"),
         variable = str_remove(variable, "_moe")) %>% 
  pivot_wider(names_from = "moe", values_from = "estimate")


# ..................................................
# Separate (Unite)  ----

tenure <- tenure %>% 
  separate(NAME, into = c("tract", "county", "state"),
           sep = ",")

# graph: dotplot
ggplot(tenure, aes(x = ownerhh, y = fct_reorder(tract, ownerhh))) +
  geom_point()


# ..................................................
# Visualize uncertainty  ----

# geom_errorbarh/geom_linerange
ggplot(tenure, aes(x = prop_owner, y = fct_reorder(tract, prop_owner))) +
  geom_errorbarh(aes(xmin = prop_owner - prop_owner_moe, 
                     xmax = prop_owner + prop_owner_moe)) +
  geom_point(color = "red") +
  geom_vline(xintercept = median(tenure$prop_owner)) +
  labs(title = "Proprtion of Households who Own",
       subtitle = "2015-2019 American Community Survey",
       y = "",
       x = "ACS estimate\n (bars represent 90% confidence intevals)")

# geom_pointrange/geom_linerange
ggplot(tenure, aes(x = prop_owner, y = fct_reorder(tract, prop_owner))) +
  geom_linerange(aes(xmin = prop_owner - prop_owner_moe, 
                     xmax = prop_owner + prop_owner_moe)) +
  geom_point(color = "red") +
  geom_vline(xintercept = median(tenure$prop_owner)) +
  labs(title = "Proprtion of Households who Own",
       subtitle = "2015-2019 American Community Survey",
       y = "",
       x = "ACS estimate\n (bars represent 90% confidence intevals)")

# or add Wilke's multiple confidence intervals 
tenure <- tenure %>% 
  mutate(ownerhh_se = ownerhh_moe/1.645,
         totalhh_se = totalhh_moe/1.645,
         prop_owner_moe80 = moe_prop(ownerhh, totalhh, ownerhh_se*1.28, totalhh_se*1.28),
         prop_owner_moe95 = moe_prop(ownerhh, totalhh, ownerhh_se*1.96, totalhh_se*1.96))

ggplot(tenure, aes(x = prop_owner, y = fct_reorder(tract, prop_owner))) +
  geom_linerange(aes(xmin = prop_owner - prop_owner_moe95, xmax = prop_owner + prop_owner_moe95),
                 color = "black", size = 2) +
  geom_linerange(aes(xmin = prop_owner - prop_owner_moe, xmax = prop_owner + prop_owner_moe),
                 color = "grey60", size = 1.5) +
  geom_linerange(aes(xmin = prop_owner - prop_owner_moe80, xmax = prop_owner + prop_owner_moe80),
                 color = "grey90", size = 1) +
  geom_point(color = "red") +
  geom_vline(xintercept = median(tenure$prop_owner)) +
  labs(title = "Proprtion of Households who Own",
       subtitle = "2015-2019 American Community Survey",
       y = "",
       x = "ACS estimate\n (bars represent 80%-90%-95% confidence intevals)") +
  theme_minimal()


# ..................................................
# Join  ----
# to property
head(property$fips_tract)
head(tenure$GEOID)

property_join <- property %>% 
  left_join(tenure, by = c("fips_tract" = "GEOID"))


# ..................................................
# Scatterplots  ----
# scatter plot: assessed vs sale  price?
property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  ggplot(aes(x = lastsaleprice, y = totalvalue)) +
  geom_point() 

# add geom_abline(intercept = 0, slope = 1)
# add geom_smooth() # try method = "lm"; method = "gam"
  

# scatter: add color aesthetic 
property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  ggplot(aes(x = lastsaleprice, y = totalvalue, color = hsdistrict)) +
  geom_point()

# add shape aesthetic
# add geom_smooth()
# add facet_wrap()


# scatter with scaled size/bubble
property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  ggplot(aes(x = lastsaleprice, y = totalvalue, 
             color = hsdistrict, size = lotsize)) +
  geom_point(pch = 21) 

# remove size legend
# add facet_wrap(~ hsdistrict, ncol = 1)
# remove color legend


# Change color aesthetic to neighborhood home ownership rate (ACS)
property_join %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  ggplot(aes(x = lastsaleprice, y = totalvalue, 
             color = prop_owner_high, size = lotsize)) +
  geom_point(pch = 21) +
  geom_abline(intercept = 0, slope = 1) + 
  guides(size = "none")


# ..................................................
# Scatterplot Matrices/Correlograms ----
# ggpairs
# https://ggobi.github.io/ggally/articles/ggpairs.html
property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  select(finsqft, lotsize, totalrooms, lastsaleprice, landvalue, improvementsvalue, hsdistrict) %>% 
  ggpairs()

# map color = hsdistrict

# ggcorr
# https://briatte.github.io/ggcorr/
property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  select(finsqft, lotsize, totalrooms, lastsaleprice, landvalue, improvementsvalue) %>% 
  ggcorr()

# try geom = "circle", max_size = 10
# try label = TRUE, label_alpha = TRUE


# ..................................................
# Slope graphs/Dumbbell Plots ----
# median sale vs median assessed by tract

# need long data frame for slope graph and dumbbell plot: 
#   tract, type (assessed/sale), median value
tract_sell_assess <- property_join %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  group_by(fips_tract) %>% 
  summarize(median_assessed = median(totalvalue),
            median_sale = median(lastsaleprice)) %>% 
  mutate(diff = median_sale - median_assessed,
         diff10 = ifelse(diff > 10000, "diff", "nodiff")) %>% 
  pivot_longer(-c(fips_tract, diff, diff10), names_to = "value_type", values_to = "value",
               names_prefix = "median_")

# Slope graph: change/difference is conveyed by slope
# Basic plot
ggplot(tract_sell_assess, aes(x = value_type, y = value)) +
  geom_line(aes(group = fips_tract), color = "gray60") +
  geom_point(color = "#0072B2")

# Fancy it up
# Make label df to identify tracts with big differences
tract_labels <- tract_sell_assess %>% 
  group_by(fips_tract) %>% 
  filter(value_type == "sale" & diff10 == "diff")

ggplot(tract_sell_assess, aes(x = value_type, y = value)) +
  geom_line(aes(group = fips_tract), color = "gray60") +
  geom_text(aes(label = round(value/1000, 0), color = diff10), size = 3) +
  geom_text(data = tract_labels,
            aes(x = value_type, y = value, label = fips_tract),
            nudge_x = 0.12, size = 3) +
  guides(color = "none") +
  scale_x_discrete(expand = expansion(add = c(0.05,.25))) +
  scale_y_continuous(breaks = seq(2e+05, 8e+05, 1e+05),
                     labels = paste0(seq(200,800,100), "K")) +
  labs(x = "", y = "Median Home Value",
       title = "Median Assessed Value vs. Median Sale Value",
       subtitle = "By Census Tract, 2020 Sales and Assessments") +
  theme_minimal()

# Could also forgo points and just put values in the figure
# replace geom_point with


# Dumbbell Plot: change/difference is conveyed by length
# Basic plot
ggplot(tract_sell_assess, aes(x= value, y= fct_reorder(fips_tract, value))) +
  geom_line(aes(group = fips_tract), color = "grey")+
  geom_point(aes(color=value_type,)) +
  guides(color = guide_legend("Value")) +
  labs(x = "", y = "",
       title = "Median Assessed Value vs. Median Sale Value",
       subtitle = "By Census Tract, 2020 Sales and Assessments") +
  theme_minimal()

# Fancy it up
# try adding size as aesthetic mapped to diff10
#   and remove size legend
#   and scale_size_discrete(range = c(2,3))
#   and make value axis labels prettier
#   and move legend to bottom of figure


# ..................................................
# Colors in R ----

# a function to display a color palette
pal <- function(col, border = "light gray", ...){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
  }

# ggplots default palettes: discrete 
# ?scale_color_hue for more about the default discrete palette
pal(hue_pal()(2))
pal(hue_pal()(10))

# ggplots default palettes: continuous
# ?scale_color_gradient for more aboutbthe default continuous palette

# Other palettes
# scale_color_brewer
pal(brewer_pal(palette = "YlGnBu")(5)) # sequential
pal(brewer_pal(palette = "BrBG")(5)) # diverging
pal(brewer_pal(palette = "Set1")(5)) # qualitative
# see https://colorbrewer2.org/ for more

# scale_fill/color_viridis
pal(viridis_pal(option = "viridis")(15)) # sequential
pal(viridis_pal(option = "plasma")(15)) # sequential
# palette options: viridis, magma, inferno, plasma, cividis, rocket, mako, turbo

# scale_fill/color_carto
pal(carto_pal(7, "Sunset")) # sequential
pal(carto_pal(7, "Fall")) # diverging
pal(carto_pal(7, "Vivid")) # qualitative
# use `metacartocolors` to see info about all palettes

# wesanderson: scale_color_manual
pal(wes_palette("Rushmore"))
pal(wes_palette("FantasticFox1"))
pal(wes_palette("Darjeeling1"))
# see https://github.com/karthik/wesanderson

# ghibli: scale_color_ghibli
pal(ghibli_palettes$PonyoMedium)
pal(ghibli_palettes$KikiLight)
pal(ghibli_palettes$TotoroLight)
# see https://ewenme.github.io/ghibli/


# ..................................................
# Using Colors in R ----

# Base plots: color by continuous variable
property %>% 
  filter(totalvalue < 1000000, 
         esdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(esdistrict, totalvalue), 
             y = totalvalue, color = totalvalue)) +
  geom_sina(shape = ".") + 
  guides(color = "none") + 
  labs(x = "Elementary School Attendance Zone",
       y = "Total Assessed Value") +
  coord_flip()

# Base plots: color by discrete variable
property %>% 
  filter(totalvalue < 1000000, 
         magisterialdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(magisterialdistrict, totalvalue), 
             y = totalvalue, color = magisterialdistrict)) +
  geom_sina(shape = ".") + 
  guides(color = "none") + 
  labs(x = "Magisterial District",
       y = "Total Assessed Value") +
  coord_flip()

## Select colors manually ----
# continuous: scale_fill/color_gradient
property %>% 
  filter(totalvalue < 1000000, 
         esdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(esdistrict, totalvalue), 
             y = totalvalue, color = totalvalue)) +
  geom_sina(shape = ".") + 
  scale_color_gradient(low = "#bfd3e6", high = "#4d004b") +
  #scale_color_gradient2(low = "#1d271cff", mid = "#44a57cff", high = "#cec917ff") +
  guides(color = "none") + 
  labs(x = "Elementary School Attensance Zone",
       y = "Total Assessed Value") +
  coord_flip()

# discrete: scale_fill/color_manual
property %>% 
  filter(totalvalue < 1000000, 
         magisterialdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(magisterialdistrict, totalvalue), 
             y = totalvalue, 
             color = fct_reorder(magisterialdistrict, totalvalue))) +
  geom_sina(shape = ".") + 
  scale_color_manual(values = rainbow(7)) +
  guides(color = "none") + 
  labs(x = "Magisterial District",
       y = "Total Assessed Value") +
  coord_flip()


## Use RColorBrewer ----
# continuous: scale_fill/color_brewer
property %>% 
  filter(totalvalue < 1000000, 
         esdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(esdistrict, totalvalue), 
             y = totalvalue, color = totalvalue)) +
  geom_sina(shape = ".") + 
  #scale_color_brewer(palette = "YlGnBu") + # not enough distinct colors
  scale_color_distiller(palette = "YlGnBu", direction = 1) +
  guides(color = "none") + 
  labs(x = "Elementary School Attensance Zone",
       y = "Total Assessed Value") +
  coord_flip()

# discrete: scale_fill/color_brewer
property %>% 
  filter(totalvalue < 1000000, 
         magisterialdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(magisterialdistrict, totalvalue), 
             y = totalvalue, 
             color = fct_reorder(magisterialdistrict, totalvalue))) +
  geom_sina(shape = ".") + 
  scale_color_brewer(palette = "Accent") + 
  guides(color = "none") + 
  labs(x = "Magisterial District",
       y = "Total Assessed Value") +
  coord_flip()

## Use viridis ----
# continuous: scale_fill/color_viridis
property %>% 
  filter(totalvalue < 1000000, 
         esdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(esdistrict, totalvalue), 
             y = totalvalue, color = totalvalue)) +
  geom_sina(shape = ".") + 
  scale_color_viridis(option = "plasma", discrete = FALSE) +
  guides(color = "none") + 
  labs(x = "Elementary School Attensance Zone",
       y = "Total Assessed Value") +
  coord_flip()

# discrete: scale_fill/color_viridis
property %>% 
  filter(totalvalue < 1000000, 
         magisterialdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(magisterialdistrict, totalvalue), 
             y = totalvalue, 
             color = fct_reorder(magisterialdistrict, totalvalue))) +
  geom_sina(shape = ".") + 
  scale_color_viridis(option = "magma", discrete = TRUE) +
  guides(color = "none") + 
  labs(x = "Magisterial District",
       y = "Total Assessed Value") +
  coord_flip()


## Use rcartocolor ----
# continuous: scale_fill/color_carto_c
property %>% 
  filter(totalvalue < 1000000, 
         esdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(esdistrict, totalvalue), 
             y = totalvalue, color = totalvalue)) +
  geom_sina(shape = ".") + 
  scale_color_carto_c(palette = "Teal") +
  guides(color = "none") + 
  labs(x = "Elementary School Attensance Zone",
       y = "Total Assessed Value") +
  coord_flip()

# discrete: scale_fill/color_carto_d
property %>% 
  filter(totalvalue < 1000000, 
         magisterialdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(magisterialdistrict, totalvalue), 
             y = totalvalue, 
             color = fct_reorder(magisterialdistrict, totalvalue))) +
  geom_sina(shape = ".") + 
  scale_color_carto_d(palette = "Vivid") + 
  guides(color = "none") + 
  labs(x = "Magisterial District",
       y = "Total Assessed Value") +
  coord_flip()


## Use fun palettes ----
# discrete: wesanderson
property %>% 
  filter(totalvalue < 1000000, 
         magisterialdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(magisterialdistrict, totalvalue), 
             y = totalvalue, 
             color = fct_reorder(magisterialdistrict, totalvalue))) +
  geom_sina(shape = ".") + 
  scale_color_manual(values = wes_palette("BottleRocket1")) +
  # several of the palettes have too few color values;
  # to use them, we have to augment the palette like so:
  #scale_color_manual(values = colorRampPalette(wes_palette("FantasticFox1"))(6)) +
  guides(color = "none") + 
  labs(x = "Magisterial District",
       y = "Total Assessed Value") +
  coord_flip()

# discrete: scale_fill/color_ghibli_d
property %>% 
  filter(totalvalue < 1000000, 
         magisterialdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(magisterialdistrict, totalvalue), 
             y = totalvalue, 
             color = fct_reorder(magisterialdistrict, totalvalue))) +
  geom_sina(shape = ".") + 
  scale_color_ghibli_d(name = "MononokeLight") + # PonyoMedium
  guides(color = "none") + 
  labs(x = "Magisterial District",
       y = "Total Assessed Value") +
  coord_flip()

# continuous: scale_fill/color_ghibli_c
property %>% 
  filter(totalvalue < 1000000, 
         esdistrict != "Unassigned") %>% 
  
  ggplot(aes(x = fct_reorder(esdistrict, totalvalue), 
             y = totalvalue, color = totalvalue)) +
  geom_sina(shape = ".") + 
  scale_color_ghibli_c(name = "PonyoLight", direction = -1) + # MarnieMedium
  guides(color = "none") + 
  labs(x = "Elementary School Attensance Zone",
       y = "Total Assessed Value") +
  coord_flip()


# ..................................................
# Combining figures ----
# https://patchwork.data-imaginist.com/index.html

# save some graphs as objects
plot1 <- ggplot(tract_sell_assess, aes(x= value, y= fct_reorder(fips_tract, value))) +
  geom_line(aes(group = fips_tract), color = "grey")+
  geom_point(aes(color=value_type,)) +
  guides(color = guide_legend("Value")) +
  labs(x = "", y = "",
       title = "",
       subtitle = "Tract Medians, 2020") +
  theme_minimal() +
  theme(legend.position = c(.8,.2)) 

plot2 <- property_join %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0,
         totalvalue < 10000000) %>% 
  mutate(diff = lastsaleprice - totalvalue) %>% 
  ggplot(aes(x = diff, color = fips_tract)) +
  geom_density() +
  coord_cartesian(xlim = c(-5e+05, 5e05)) +
  geom_vline(xintercept = 0) +
  labs(x = "", y = "",
       title = "",
       subtitle = "Tract Distributions, 2020") +
  guides(color = "none")

plot1 + plot2 +
  plot_annotation(title = "2020 Sale Prices - 2020 Assessed Values")


# ..................................................
# Save the last plot ----

ggsave(filename = "combined_figure.png", width = 8, height = 6)
