# Data Viz in R
# 2021-10-19
# Practicing with R


# ..................................................
# Good Coding Practices ----

# If your using packages, load them all at once at the beginning of the script

# Use code sections to break up your script into discrete, readable (labeled) chunks
#   a comment line with four or more trailing dashes (-), equal signs (=), or pound signs (#) 
#   creates easy navigation between them
#   sections are automatically foldable
#   and you can use the Jump To menu at the bottom of the editor to navigate

# Try to limit a given line of code to 80 characters 

# Add a space after a comma (for readability)
#   While you're at it, add spaces around <-, = and ==, and other operators

# Pick an object naming convention
#   I like snake_case (all lower case, connected by underscores)
#   But CamelCase is popular, too
#   Don't use spaces
#   And stay away from . (that's a dot; base R uses . in function names)


# ..................................................
# Load libraries and data ----
# Libraries
# When you open this script, RStudio will tell you if there are
#   packages you don't have installed and offer to install them!
library(tidyverse)
library(lubridate) # for date extraction
library(ggforce) # for sina plot
library(ggridges) # for ridge plot
library(treemapify) # for tree plot
library(waffle) # for wafflet plot


# Data
property <- read_csv("data/parcels_cards_chacteristics.csv")


# ..................................................
# Factors ----
property %>% count(condition) # currently a character

property %>% 
  mutate(condition = factor(condition)) %>% # make a factor
  count(condition)

# assert the ordering of the factor levels
cond_levels <- c("Excellent", "Good", "Average", "Fair", "Poor", "Very Poor", "Unknown")

property %>% 
  mutate(condition = factor(condition, levels = cond_levels)) %>% 
  count(condition)

# forcats
# create level by frequency
property %>% 
  mutate(condition = fct_infreq(condition)) %>% 
  count(condition)

# combine small levels, fct_lump_n (min, prop, etc.)
property %>% 
  mutate(condition = fct_lump_n(condition, 6)) %>% 
  count(condition)

# recode levels, fct_collapse (also fct_relevel)
cond_levels <- c("Excellent", "Good", "Average", "Fair", "Poor", "Very Poor", "missing")

property %>% 
  mutate(condition = fct_collapse(condition,
                                missing = c("NULL", "Unknown")),
         condition = fct_explicit_na(condition, na_level = "missing"),
         condition = fct_relevel(condition, cond_levels)) %>% 
  count(condition)


# ..................................................
# ggplot2 ----

# Line examples
# How many residential properties were built each year?
#   For this one, we'll create the summary data on the fly...
property %>% 
  filter(cardtype == "R") %>% 
  group_by(year = as.numeric(yearbuilt)) %>% 
  summarize(total = n()) %>% 
  ggplot(aes(x = year, y = total)) +
  geom_line(color = "orange")


# Graph the median sale price by year of sale
#    For this one, we'll create the data frame first 
sale_by_year <- property %>% 
  mutate(lastsaleprice = as.numeric(lastsaleprice),
         sale_year = str_sub(lastsaledate1, 7, 10),
         sale_year = as.numeric(sale_year)) %>% 
  filter(cardtype == "R" & lastsaleprice != 0) %>% 
  group_by(sale_year) %>% 
  summarize(med_price = median(lastsaleprice, na.rm = TRUE))

#   Graph this


#   Then filter to 1983-2020



# Bar examples
# How many residential properties are in each magisterial district?
property %>% 
  filter(cardtype == "R") %>% 
  ggplot(aes(x = magisterialdistrict)) +
  geom_bar()

# To try:
# order the district factors by frequency
# change the color of the bars
# flip coordinate



# ..................................................
# A data cleaning chunk ----
# To make this easier to work with

# Some things I checked before making these decisions

# million_homes <- property %>%
#   filter(totalvalue > 1.0e+07) %>%
#   group_by(parcelid) %>%
#   summarize(buildings = n(),
#             cards = max(cardnum),
#             mean = mean(totalvalue),
#             sd = sd(totalvalue),
#             owner = first(owner))

# Run this
property_full <- property

var_remove <- c("visionid", "bid", "commsectnum", "status", "gpin.y", "valandmarkregister", 
                "ntnlhistoriclandmark", "ntnlreghistoricplaces", "worldheritagesite", 
                "zoningminor", "zoningother", "zmasrealtedtoproffers", "airportimpactarea",
                "daminundationzone", "entrancecorridor", "naturalresourceextractionoverlay", 
                "scenicbywaysoverlay", "scenicstreamoverlay", "steepslopesmanaged",
                "steepslopespreserved", "compplanlanduseminor", "compplanlanduseother", 
                "landuseminor", "landuseminorstructuresnumber", "landuseminordwellingunitsnumber",
                "mpo_chartarea", "trafficanalysiszone", "jurisdictionalareadesignation")

var_numeric <- c("yearbuilt", "yearremodeled", "numstories", "finsqft", "unfinishedlivingsqft",
                 "bsmtfinsqft", "bsmttotsqft", "attictotsqft", "fp_open", "fp_stacks",
                 "bedroom", "diningroom", "familyroom", "livingroom", "fullbath", "halfbath", 
                 "totalrooms", "lastsaleprice")

var_factor <- c("condition", "esdistrict", "msdistrict", "hsdistrict", "magisterialdistrict",
                "votingprecinct", "zoningprimary", "zoningsecondary", "proffered", "floodhazardoverlay",
                "compplanarea", "compplanlanduseprimary", "compplanlandusesecondary", 
                "landuseprimary", "landusesecondary", "watershed", "watersupplyprotectionarea", 
                "developmentarea", "otherruralland", "agforestdistrict", "conservationeasement", 
                "openspaceuseagreement")

property <- property_full %>% 
  select(-var_remove) %>% 
  filter(cardtype == "R",
         totalvalue < 1.0e+07) %>% 
        # these were not generally residential homes (see million_homes above)
  mutate(across(all_of(var_numeric), as.numeric),
         # apply the function as.numeric() to every column in var_numeric
         across(all_of(var_factor), as.factor),
         # apply the function as.factor() to every column in var_factor
         lastsaledate = as.Date(lastsaledate1, "%m/%d/%Y"),
         # use ?strptime to see list of formats
         last_sale_year = year(lastsaledate),
         # the `year` function comes from the lubridate package
         value_sqft = totalvalue/finsqft,
         grade_reduced = str_extract(grade, "^[^\\:]+"),
         # the initial ^ means the "start of a string";
         # the ^ in [] means "not", not the :; 
         # the + means 1 or more characters
         grade_reduced = factor(grade_reduced),
         tract = as.character(censustract),
         tract = str_remove(tract, "\\."),
         # remove the dot
         tract = paste0("0", tract),
         # add a leading 0
         tract = str_pad(tract, width = 6, side = "right", pad = "0"),
         # add 0s at end, if needed, to make tract 6 characters
         tract = ifelse(str_detect(tract, "NA"), NA_character_, tract),
         fips_tract = paste0("51003", tract),
         fips_tract = ifelse(str_detect(fips_tract, "NA"), NA_character_, fips_tract),
         fips_blkgp = paste0(fips_tract, censusblockgroup),
         fips_blkgp = ifelse(str_detect(fips_blkgp, "NA"), NA_character_, fips_blkgp),
         era = case_when(
           yearbuilt < 1950 ~ "Pre-1950",
           yearbuilt >= 1950 & yearbuilt < 2000 ~ "1950-1999",
           yearbuilt >= 2000 ~ "Post-2000",
           TRUE ~ "Unknown"
         ),
         era = factor(era, 
                      levels = c("Unknown", "Pre-1950", "1950-1999", "Post-2000"))
         )

# save this
write_csv(property, "data/property_cleaned.csv")
saveRDS(property, "data/property_cleaned.RDS")
# property <- readRDS("data/property_cleaned.RDS")


# ..................................................
# Distributions ----

p <- ggplot(property, aes(x = totalvalue))

p + geom_histogram()

# To try:
# change the bins
# change the color of the bars

p + geom_density()

# To try:
# change the bandwidth, e.g, adjust = 1/2 means use half the default bandwidth
# change the color of the line

p + geom_density(aes(fill = hsdistrict, color = hsdistrict), alpha = 1/3) 

# To try:
# limit the axis with coord_cartesian() -- zooms in without changing underlying data
# limit the axis with scale_x_continuous() -- converts values outside of range to NA


# ..................................................
# BONUS: Recreating Wilke's approach
#    (embed each subset distribution in the overall distribution)
property_hs <- property %>% 
  filter(hsdistrict != "Unassigned") # remove the unassigned properties

ggplot(property_hs, aes(x = totalvalue, y = ..count..)) +
  # specifcy y as the count so that everything remains relative to same values
  geom_density(data = select(property, -hsdistrict), fill = "grey", alpha = 1/3) +
  # plot the density of all observations from full data set; remove hsdistrict to make this work
  geom_density(aes(fill = hsdistrict), alpha = 1/3) +
  # the normal density plot for properties within each hsdistrict
  geom_vline(xintercept = median(property$totalvalue, na.rm = TRUE)) +
  # add a verticle line at the overall median
  facet_wrap(~ hsdistrict) + 
  # separate hsdistrict into facets
  coord_cartesian(xlim = c(0, 1500000), ylim = c(0, 0.09)) +
  # shrink the x axis, expand the y axis
  scale_fill_manual(
    values = c("red", "gold", "blue"),
    guide = "none"
  ) +
  # change the colors (using school spirit colors!) and turn off the legend
  labs(x = "Total Value", y = "Scaled density") +
  # add better axis labels
  theme_minimal()
  # use a different background theme

# that was hard...
# ..................................................


# Comparing distributions
# Box plots
property_hs %>% 
  filter(finsqft < 10000) %>% 
  ggplot(aes(x = hsdistrict, y = finsqft)) +
  geom_boxplot(fill = "grey90") 


# strip charts
property_hs %>% 
  filter(finsqft < 10000) %>% 
  ggplot(aes(x = hsdistrict, y = finsqft)) +
  geom_point(size = 0.25, position = "jitter") +
  stat_summary(fun = "median", geom = "point", 
               size = 3, color = "red")
  
  
# Violin plots
property_hs %>% 
  filter(finsqft < 10000) %>% 
  ggplot(aes(x = hsdistrict, y = finsqft)) +
  geom_violin(fill = "grey90") 

# To try:
# flip the coordinate
# add some color
# add the median


# Sina plots 
property_hs %>% 
  filter(finsqft < 10000) %>% 
  ggplot(aes(x = hsdistrict, y = finsqft, color = hsdistrict)) +
  geom_sina() + 
  scale_color_manual(
    values = c("red", "gold", "blue"),
    guide = "none"
  ) +
  # Albemarle is Red and Black; Monticello is Gold and Navy; Western is Blue and Gold
  labs(x = "High School District",
       y = "Finished Square Feet")

# To try:
# change the shape to . or reduce alpha or both
# put a violin underneath (before sina)


# ridge plots
property %>% 
  filter(finsqft < 5000,
         !is.na(tract)) %>% 
  ggplot(aes(x = finsqft, y = tract, finsqft)) +
  geom_density_ridges() +
  coord_cartesian(clip = "off") +
  labs(x = "Finished Square Feet",
       y = "Census Tract",
       title = "Home Sizes by Census Tract") +
  theme_ridges()

# order tracts by number of homes (e.g., fct_infreq())
# check: property %>% group_by(tract) %>% summarize(num = n()) %>% arrange(desc(num))
# order by median size of homes (e.g., fct_reorder())
# check: property %>% group_by(tract) %>% summarize(med = median(finsqft, na.rm = TRUE)) %>% arrange(desc(med))



# ..................................................
# Amounts ----
# Grouped bars
# use era built as fill factor
property %>% 
  filter(magisterialdistrict != "Unassigned") %>%
  ggplot(aes(x = fct_infreq(magisterialdistrict), fill = era)) +
  geom_bar(position = "dodge") 

# To try:
# group districts within eras instead
# instead of grouping, try faceting


# dot plots (or lollipops)
# make property by elementary district data frame

prop_by_esdistrict <- property %>% 
  group_by(esdistrict) %>% 
  summarize(med_value_sqft = median(value_sqft, na.rm = TRUE)) %>%
  mutate(hi_lo = ifelse(med_value_sqft < 196, "Low", "High"),
         diff = med_value_sqft - 196)

# dot plot
prop_by_esdistrict %>% 
  ggplot(aes(x = med_value_sqft, 
             y = fct_reorder(esdistrict, med_value_sqft))) + 
  geom_point(size = 5, color = "#0072B2") +  
  labs(title = "Does Assessed Value per Square Footage of Homes Vary by School District?",
       subtitle = "Median Assesed Value/Square Foot by Elementary School Zones", 
       x = "Median Assessed Value per Square Foot",
       y = "") +
  theme_minimal()

# lollipop version
prop_by_esdistrict %>% 
  ggplot(aes(x = med_value_sqft, 
             y = fct_reorder(esdistrict, med_value_sqft))) + 
  geom_point(size = 5, color = "#0072B2") +  
  geom_segment(aes(x=0, xend=med_value_sqft, y=esdistrict, yend=esdistrict), 
               color = "grey") + 
  labs(title = "Does Assessed Value per Square Footage of Homes Vary by School District?",
       subtitle = "Median Assesed Value/Square Foot by Elementary School Zones", 
       x = "Median Assessed Value per Square Foot",
       y = "") +
  theme_minimal()

# To try:
# add reference line for median value (196) and change the line segment
# color by hi-lo using scale_color_gradient2(low = "#d53e4f", mid = "#ffffbf", high = "#3288bd")


# ..................................................
# BONUS: Recreating Wilke's heatmap
# Create number of houses built each year by magisterial district
prop_by_year <- property %>% 
  group_by(as.numeric(yearbuilt), magisterialdistrict) %>% 
  summarize(num_built = n()) %>% 
  rename(yearbuilt = "as.numeric(yearbuilt)") %>% 
  filter(yearbuilt > 1900,
         magisterialdistrict != "Unassigned") 

# Fill in missing year/district pairs with 0
prop_by_year <- prop_by_year %>% 
  group_by(magisterialdistrict) %>% 
  complete(yearbuilt = full_seq(1901:2021, 1), 
           fill = list(num_built = 0)) 


# heatmap/tile
prop_by_year %>% 
  ggplot(aes(x = yearbuilt, y = magisterialdistrict, fill = num_built)) +
  geom_tile(color = "white", size = 0.25) +
  # color = "white" add white line separator between tiles
  scale_fill_viridis_c(
    option = "turbo", 
    name = "Number of houses built/year",
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = grid::unit(3, "in"),
      barheight = grid::unit(0.2, "in")
    )
  ) +
  # I borrowed this from Wilke -- the guide_colorbar was new to me...
  scale_x_continuous(expand = c(0, 0), name = NULL) +
  # make the plotted figure fill the full plot
  scale_y_discrete(name = "", position = "right") +
  # remove the y axis title and put labels on the right
  labs(title = "Housing Growth by Magisterial District") +
  theme(
    legend.position = "bottom",
    legend.justification = "right"
  )
  # alter the theme to control legend placement

# that was hard, too....


# ..................................................
# Proportions ----

# Pie charts and donuts -- though you probably shouldn't

# Map use codes to residence type
sfh <- c("Single Family", "Single Family-Rental")
mfh <- c("3-4 Family", "Condo-Res-TH", "Condo-Res-Garden", "Apartments", "Duplex", 
         "Doublewide", "Mobile Home", "Multi-Family", "Small Apartment")
vac <- c("Vacant (R5-R6)", "Vacant Residential Land")

# Pie and Donut data prep
prop_pie <- property %>% 
  mutate(use = case_when(
    usecode %in% sfh ~ "single-family",
    usecode %in% mfh ~ "multi-family",
    usecode %in% vac ~ "vacant-land",
    TRUE ~ "non-residence"
  ),
  use = factor(use, levels = c("single-family", 
                                 "multi-family", 
                                 "vacant-land", 
                                 "non-residence"))
  ) %>% 
  group_by(use) %>% 
  summarize(num = n())  %>% 
  arrange(num) %>% 
  # this sorts the data in the order I want it to plot
  mutate(prop = num/sum(num) * 100,
         # calculate the percent within each type
         ypos = cumsum(prop) - 0.5*prop,
         # calculate point for label position
         label = ifelse(prop > 5, as.character(use), NA_character_)
         # don't print labels if value is really small
         )
  

# Pie 
ggplot(prop_pie, aes(x = 1, y = prop, fill = use)) + 
  geom_col(color = "white") + 
  coord_polar(theta = "y") +
  geom_text(aes(y = ypos, x = 1.25, label = label), 
            color = "white", size=6) +
  guides(fill = "none") +
  theme_void()


# Donut 
ggplot(prop_pie, aes(x = 2, y = prop, fill = use)) + 
  geom_col(color = "white") + 
  coord_polar(theta = "y") +
  geom_text(aes(y = ypos, x = 2, label = label), 
            color = "white", size=6) +
  guides(fill = "none") +
  xlim(0.5, 2.5) +
  theme_void()



# Tree map
acreage <- property %>% 
  group_by(magisterialdistrict) %>% 
  summarize(acres = sum(lotsize)) %>% 
  mutate(label = paste(magisterialdistrict, scales::comma(acres), "acres", sep = "\n"))

ggplot(acreage, aes(area = acres, fill = acres, 
                    label = label)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white") +
  labs(title = "Total Acreage of Residential Homes by Magisterial District") +
  theme(legend.position = "none")


# Waffles
# https://github.com/hrbrmstr/waffle
(prop_waffle <- prop_pie %>% 
    mutate(num_scaled = round(num/100)) %>% 
    select(use, num_scaled) %>% 
    droplevels())

# using waffle (expects a named vector)
home_types <- c(345, 39, 3, 3) # create vector of scaled values
names(home_types) <- c("single family", "multi family", "vacant land", "non-res") # add names

waffle(home_types, rows = 10, size = 0.5,
       colors = c("#969696", "#009bda", "#c7d4b6", "#97b5cf"),
       legend_pos = "bottom")

# using geom_waffle (works with data frames)
ggplot(prop_waffle, aes(fill = use, num_scaled, values = num_scaled)) +
  geom_waffle(n_rows = 10, color = "white", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(values = c("#969696", "#009bda", "#c7d4b6", "#97b5cf"),
                    name = "") +
  theme_void() +
  theme(legend.position = "bottom") 


