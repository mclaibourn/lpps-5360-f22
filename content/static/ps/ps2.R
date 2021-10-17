# Data Viz in R
# Problem Set 2: wrangling in R, graphing in R
# Due 2021-10-26


# Be sure to use good coding practices in your problem set script!


# 1. Install/load packages ----

library(tidyverse)
# add whatever libraries you need to complete the assignment


# 2. Load and explore the data ----

# Download birth data collected by FiveThirtyEight: https://github.com/fivethirtyeight/data/tree/master/births. 
#   We'll use the Social Security Administration data from 2000-2014

births <- read_csv("https://github.com/fivethirtyeight/data/blob/master/births/US_births_2000-2014_SSA.csv?raw=true")

# Have a look at the data and get a feel for what's here (head, glimpse, summary, etc.)



# 3. Factors ----

# Create a new variable making month into a factor with labels/levels of the month names;
#   and create a new variable making date_of_month a factor (these can retain number names);
#   and create a new variable making day_of_week a facotr with labels/levels of the weekday names 
#   the github page tells us 1 is Monday, 7 is Sunday; finally, create an indicator variable
#   for weekend days.



# Look at the count of births by month and by day of week using your new variables 
#   (This is a good check on whether your factor order is what you intended)



# 4. Visualizing Distributions ----

# a. Look at the distribution of the number of births (births) by day;
#   try a histgram (choose a number of bins)
#   and a density plot
#   add a title and axis titles; feel free to make them prettier



# b. What do you notice?


# c. Look at the distribution of number of births by the day of the week
#   make any two of the following: strip chart, violin plot, sina plot
#   vary whether you put births on the x or y axis to look at this in different ways
#   make sure the days of the week are in a reasonable order (whether they are on the x or y axis)
#   try having some fun with these -- add some color, add a summary function, 
#   play with alpha, etc. 



# 5. Visualizing Amounts ----

# a. Mark a bar plot of the average number of daily births by month 
#   First, make a new dataframe that calculates the average and median
#   daily births by month. Then make a bar plot of these averages.
#   Try adding color mapped to month or to average births -- keep the one you like.
#   Be sure to add titles and appropriate axis labels (which may be no axis labels).


# b. Make a dot plot of the median daily birth rate (created above).
#   Then adapt it to a lollipop chart emphasizing the 
#   monthly deviation from the overall median (12,320). 
#   Feel free to play with color, point size, or other attributes!


  
# 6. Visualzing Proportions ----

# a. Finally, visualize the proportion of births that occur each day of the week in 2014. 
#   That is, create a data frame of the number of births in Mon, Tue, Wed, ... in 2014 and
#   graph the proportion of births for each of the seven days using a tree map, a waffle 
#   chart, OR a pie chart -- whichever ONE you prefer.

