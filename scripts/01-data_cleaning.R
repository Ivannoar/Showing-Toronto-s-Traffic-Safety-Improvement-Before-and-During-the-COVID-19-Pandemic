#### Preamble ####
# Purpose: Clean the survey data downloaded from opendatatoronto
# Author: Ivan Li
# Data: 4 February 2022
# Contact: ivann.li@mail.utoronto.ca
# License: MIT


#### Workspace setup ####

library(opendatatoronto)
library(tidyverse)


### Data download ###
# From https://open.toronto.ca/dataset/police-annual-statistical-report-traffic-collisions/

# Datasets are commonly categorized into packages which contain all 
# relevant datasets of the package's topic. We will look at the entire package
# using a unique key from the opendatatoronto webpage (see above).

# get package
package <- show_package("ec53f7b2-769b-4914-91fe-a37ee27a90b3")

# get all resources for this package
resources <- list_package_resources(package)
# From this list of resources, we must obtain an unique key


# There is only one resource so get_resource() will load this by default.
# daily_collision_report <- 
#   get_resource(list_package_resources(package))


# In case of error, use directly downloaded data. The above code serves as
# what would be done if no error was present. 

daily_collision_report <-
  read.csv(file = 'inputs/data/c6cc4b41-b465-4c86-a141-96c1b1c63e33.csv')

### Cleaning data ###

daily_collision_report_clean <- 
  select(daily_collision_report, 5, 6, 7,
            8, 9, 10, 11, 12, 13, 14, 15) # Removing unneeded variables

daily_collision_report_clean <- 
  filter(daily_collision_report_clean, Year >= 2016) # Remove unneeded years (2014-2015)


#### Save data ####
View(daily_collision_report_clean)
write_csv(daily_collision_report_clean,
          file = 'outputs/paper/dcr.csv')


### Creating graphical data ###

# This is code that is also run in the R Markdown file in order to create graphs and tables.

# Generating first table, sample of dataset
daily_collision_report_clean |> 
  slice(1:5) |> 
  kable(
    caption = "First five rows of a dataset showing traffic collisions in Toronto",
    col.names = c("Month", "Day", "Year", "Hour", "Divison", "Neighbourhood ID", "Neighbourhood", "Fatalities", "Injury", "Fail to Remain", "Property Damage"),
    digits = 1,
    booktabs = TRUE, 
    linesep = ""
  ) |> kable_styling(latex_options = c("striped", "scale_down"))

# Generating first graph, number of serious collisions
# Coded with help of https://www.r-bloggers.com/2021/09/adding-text-labels-to-ggplot2-bar-chart/
seriousgraph <- filter(daily_collision_report_clean, Year > 2015 & (Fatalities >= 1 | Injury_Collisions == "YES")) |> 
  ggplot(mapping = aes(x = Year) ) +
  geom_histogram(binwidth=0.5, fill = 'grey') + ylab("Frequency") + 
  geom_text(aes(label = ..count..), vjust = 5, stat = "count", colour = "white")

seriouscollisions <- filter(daily_collision_report_clean, Year >= 2016 & (Fatalities >= 1 | Injury_Collisions == "YES"))

# Creating second graph, comparison of collisions over time. Citation for code in PDF.
# Coded with help of https://stackoverflow.com/questions/46020416/individual-events-into-frequency-per-year
sc_totals <- data.frame(table(seriouscollisions$Year, seriouscollisions$Neighbourhood))
# Finding top 10 most dangerous neighbourhoods for collisions
sc_top10 <- filter(seriouscollisions, Atom == 77| Atom == 1| Atom == 119| Atom == 137| Atom == 14| Atom == 130| Atom == 42| Atom == 76| Atom == 27| Atom == 131)
# Isolating top 10 most dangerous neighbourhoods for collisions
sc_top10 <- sc_top10 %>% group_by(Neighbourhood, Year) %>%
  summarise(collision_total = length(Neighbourhood), show_col_types = FALSE)

sc_top10_graph <- sc_top10 %>%
  group_by(Year) %>%
  ggplot(aes(x = Year, y= collision_total, color = Neighbourhood)) +
  geom_line(aes(group = Neighbourhood)) +
  labs(color = "Neighbourhoods",
       x = "Year",
       y = "Number of Serious Collisions") +
  theme_minimal()

# Generating third graph, total number of collisions yearly. Code is almost identical to first graph.
# Coded with help of https://www.r-bloggers.com/2021/09/adding-text-labels-to-ggplot2-bar-chart/
totalgraph <- daily_collision_report_clean |>
  ggplot(mapping = aes(x = Year) ) +
  geom_histogram(binwidth=0.5, fill = 'grey') + ylab("Frequency") +
  geom_text(aes(label = ..count..), vjust = 5, stat = "count", colour = "white")