#26/03/2024
#Workshop 10 - Data visualisation 3

install.packages('tidyverse')
install.packages('RColorBrewer')
install.packages('patchwork')

library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(dplyr)

beetles <- read.csv("dung_beetles.csv") %>%
  rename_with(tolower, everything()) %>%
  select(!starts_with("X")) %>%
  rename_with( ~ gsub("opis", "opris", .x), matches("Copis")) %>%
  pivot_longer(cols = contains('_'),
               names_to = 'spp',
               values_to = 'count') %>%
  separate_wider_delim("spp", "_",
                       names = c("genus",
                                 "species"))

#'mutate()' can be used both horizontally and vertically by grouping rows together and making summary stats for each group
first_try <- beetles %>%
  group_by(month)

?group_by
#this takes an existing tbl and converts it into a grouped table where operations are performed 'by group'

?summarize
#this creates a new data frame
#returns one row for each combination of grouping variables; if there are no grouping variables, the output will have a single row summarising all observations in the input
#it will contain one column for each grouping variable and one column for each of the summary stats that you have specified

#Group by months and count all of them up as follows:
total_pcm <- beetles %>%
  group_by(month) %>%
  summarize(total = sum(count))
#this finds the sum of the groupings using the 'count' column in 'beetles' dataset

?summarize

#Now try plotting that with ggplot as a bar plot
#You'll need to reorder the months so they're in proper order
chronological_order <- dat

total_pcm %>%
  mutate(
    month = factor(month, levels = month.name)
  ) %>% #the 'month'(s) are currently in alphabetical order because they are characters, so it needs to be changed into a factor. There is a built-in-array of month names 'month.name'
  arrange(month) %>%
  ggplot(aes(x = month, y = total)) +
  geom_col()

#[3] HEATMAPS
#These are used to look at the interaction of three variables, especially for occurrence data like the one here

#geom_raster can be used 

#Group the beetles table by genus name as well as month. Plot all of these together using geom_raster

grouping_genus_month <- beetles %>%
  group_by(genus, month) %>%
  summarise(mean_count=(mean(count)),.
            )

?summarise

?group_by
  
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

grouping_genus_month %>%
  mutate(
    month = factor(month, levels = month.name)
  ) %>%
  arrange(month) %>%
  ggplot(aes(x = month, y = genus)) +
  geom_raster(aes(fill = mean_count)) +
  theme(legend.position = "right",
        axis.title.x = element_text(),
        axis.title.y = element_text(),
        axis.text.x = element_text(angle = 45))
  
#SETTING SCALE LIMITS

#Cut the scale off at ten. You'll need to set up a scale for 'fill' with a top limit of 10. This will remove all values over ten

grouping_genus_month %>%
  mutate(
    month = factor(month, levels = month.name)
  ) %>%
  arrange(month) %>%
  ggplot(aes(x = month, y = genus)) +
  geom_raster(aes(fill = mean_count)) +
  scale_fill_continuous(limits = c(0, 10), na.value = '#56B1F7') +
  theme(legend.position = "right",
        axis.title.x = element_text(),
        axis.title.y = element_text(),
        axis.text.x = element_text(angle = 45))

#[4] AXES

guinea_world <- read.csv("number-of-reported-guinea-worm-dracunculiasis-cases.csv") %>% 
  rename(cases = Reported.cases.of.Guinea.worm.disease,
         country = Entity) %>%
  rename_with(tolower) %>% 
  filter(country == "World") %>% 
  ggplot(aes(x = year, y = cases)) +
  geom_line() +
  geom_point() +
  theme

guinea_world
