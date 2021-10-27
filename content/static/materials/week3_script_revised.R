# Data Viz in R
# 2021-10-26
# Practicing with R


# ..................................................
# Load libraries and data ----
# Libraries
library(tidyverse)
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
# Visualize uncertainty  ----
property_tract <- property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  group_by(fips_tract) %>% 
  summarize(mean_sale = mean(lastsaleprice),
            sd_sale = sd(lastsaleprice),
            mean_assess = mean(totalvalue),
            sd_assess = sd(totalvalue))

# geom_errorbarh/geom_linerange
ggplot(property_tract, aes(x = mean_sale, 
                           y = fct_reorder(fips_tract, mean_sale))) +
  geom_errorbarh(aes(xmin = mean_sale - 1.645*sd_assess,
                     xmax = mean_sale + 1.645*sd_assess)) +
  geom_point(color = "red") +
  geom_vline(xintercept = mean(property_tract$mean_sale)) +
  labs(title = "Average Sale Value by Census Tract",
       y = "",
       x = "Mean Sale Value")

# geom_pointrange/geom_linerange
ggplot(property_tract, aes(x = mean_sale, 
                           y = fct_reorder(fips_tract, mean_sale))) +
  geom_linerange(aes(xmin = mean_sale - 1.645*sd_sale,
                     xmax = mean_sale + 1.645*sd_sale)) +
  geom_point(color = "red") +
  geom_vline(xintercept = mean(property_tract$mean_sale)) +
  labs(title = "Average Residential Sale Value by Census Tract",
       subtitle = "2020 Property Sales",
       y = "", x = "Mean Sale Value")

# or add Wilke's multiple confidence intervals 
ggplot(property_tract, aes(x = mean_sale, 
                           y = fct_reorder(fips_tract, mean_sale))) +
  geom_linerange(aes(xmin = mean_sale - 1.645*sd_sale,
                     xmax = mean_sale + 1.645*sd_sale),
                 color = "black", size = 2) +
  geom_linerange(aes(xmin = mean_sale - 1.28*sd_sale,
                     xmax = mean_sale + 1.28*sd_sale),
                 color = "grey60", size = 1.5) +
  geom_point(color = "red") +
  geom_vline(xintercept = mean(property_tract$mean_sale)) +
  labs(title = "Average Residential Sale Value by Census Tract",
       subtitle = "2020 Property Sales",
       y = "", x = "(Bars represent 80%-90% confidence intervals")



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
property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  ggplot(aes(x = lastsaleprice, y = totalvalue)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth()

# scatter: add color aesthetic 
property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  ggplot(aes(x = lastsaleprice, y = totalvalue, 
             color = hsdistrict)) +
  geom_point() 

# add shape aesthetic
# add geom_smooth()
# add facet_wrap()
property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  ggplot(aes(x = lastsaleprice, y = totalvalue, 
             color = hsdistrict, shape = hsdistrict)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ hsdistrict)

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
property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  ggplot(aes(x = lastsaleprice, y = totalvalue, 
             color = hsdistrict, size = lotsize)) +
  geom_point(pch = 21) +
  guides(size = "none", color = "none") +
  facet_wrap(~ hsdistrict, ncol = 1)



# ..................................................
# Scatterplot Matrices/Correlograms ----
# ggpairs
# https://ggobi.github.io/ggally/articles/ggpairs.html
property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  select(finsqft, lotsize, totalrooms, lastsaleprice, landvalue, improvementsvalue, hsdistrict) %>% 
  ggpairs(aes(color = hsdistrict))

# map color = hsdistrict

# ggcorr
# https://briatte.github.io/ggcorr/
property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  select(finsqft, lotsize, totalrooms, lastsaleprice, landvalue, improvementsvalue) %>% 
  ggcorr(label = TRUE, label_alpha = TRUE)

# try geom = "circle", max_size = 10
# try label = TRUE, label_alpha = TRUE


# ..................................................
# Slope graphs/Dumbbell Plots ----
# median sale vs median assessed by tract

# need long data frame for slope graph and dumbbell plot: 
#   tract, type (assessed/sale), median value
tract_sell_assess <- property %>% 
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
  geom_point(aes(color = diff10)) +
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
# replace geom_point with geom_text(aes(label = round(value/1000, 0), color = diff10), size = 3) +


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
ggplot(tract_sell_assess, aes(x= value, y= fct_reorder(fips_tract, value))) +
  geom_line(aes(group = fips_tract), color = "grey")+
  geom_point(aes(color=value_type, size = fct_rev(diff10))) +
  scale_size_discrete(range = c(2,3)) +
  guides(color = guide_legend("Value"), size = "none") +
  labs(x = "", y = "",
       title = "Median Assessed Value vs. Median Sale Value",
       subtitle = "By Census Tract, 2020 Sales and Assessments") +
  theme_minimal() +
  theme(legend.position = "bottom")



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
