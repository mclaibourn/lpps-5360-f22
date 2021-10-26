# Data Viz in R
# Problem Set 3 data set up


# Libraries ----
library(tidyverse)
library(WDI)


# Pull data ----
# https://data.worldbank.org/

indicators <- c("SP.DYN.LE00.IN",  # Life expectancy
                "SP.DYN.LE00.FE.IN", # Life expectancy, female
                "SP.DYN.LE00.MA.IN", # Life expectancy, male
                "EG.ELC.ACCS.ZS",  # Access to electricity
                "NY.GDP.PCAP.KD", # GDP per capita
                "EN.ATM.CO2E.PP.GD", # CO2 emissions
                "SI.POV.DDAY"     # Extreme poverty (% earning less than $2/day)
)  

wdi_raw <- WDI(country = "all", indicators, 
               extra = TRUE, start = 2008, end = 2018)
# extra = TRUE returns extra variables: region, iso3c code, incomeLevel, and others


# Prep data ----
wdi_clean <- wdi_raw %>% 
  filter(income != "Aggregates") %>% 
  select(iso2c, country, year, life_expectancy = SP.DYN.LE00.IN, 
         life_expectancy_female = SP.DYN.LE00.FE.IN,
         life_expectancy_male = SP.DYN.LE00.MA.IN,
         access_to_electricity = EG.ELC.ACCS.ZS, 
         gdp_per_cap = NY.GDP.PCAP.KD, 
         co2_emissions = EN.ATM.CO2E.PP.GD, 
         pct_extreme_poverty = SI.POV.DDAY,
         region, income)


# Save data ----

# first, check if a data subfolder exists in
# the current working directory; if not,
# create it!
if (!dir.exists("data")){
  dir.create("data")
} else {
  print("Dir already exists!")
}

# save as a CSV to call in the Rmd file
write_csv(wdi_clean, "data/wdi_clean.csv")
