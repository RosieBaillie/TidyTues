#### ---- Tidy Tuesday 27th April 2021 ---- ####
# Looking at CEO departure data from Gentry et al., (2021): 
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-27/readme.md

## Load in data 
install.packages("tidytuesdayR")

departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

# Library
#library(tidyr)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)

## -- What's the most common name for CEOs? -- ##

# Extract first name and last name.
departures <- extract(departures, exec_fullname, c("FirstName", "LastName"), "([^ ]+) (.*)")

CommonNames <- departures %>% 
  group_by(departures$FirstName) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  filter(Count >= 50)

CommonNames <- CommonNames %>% 
  rename(FirstName = `departures$FirstName`)

CommonNames <- CommonNames %>% 
  rownames_to_column("Name")

NameDotChart <- ggdotchart(
  CommonNames, x = "FirstName", y = "Count",
  add = "segments",
  sorting = "descending",
  rotate = TRUE,
  xlab = "First names",
  title = "The 'John' problem?",
  subtitle = "Most common names for S&P 1500 firm CEOs",
  caption = "Data source: Gentry et al. (2021),
  Visualised by: @Rosie_Baillie_", 
  ggtheme = theme_gray()
)

## -- How have CEO names changed over time? -- ##

# Let's get a dataframe showing CEO names by fyear.
NameTime <- departures %>% 
  select(FirstName, fyear) %>% 
  group_by(FirstName, fyear) %>% 
  summarise(n = n ())

# We're going to filter NameTime so that we only have the names which appear 
# more than 300 times: John, Robert, James, David, William, and Michael. 
PopNames <- NameTime %>% 
  filter(FirstName == "John" | FirstName == "Robert" | FirstName == "James" |
           FirstName == "David" | FirstName == "William" | FirstName == "Michael")

ggplot(data = PopNames, aes(x = fyear, y = n, group = FirstName, colour = FirstName)) +
  geom_line() +
  geom_point() + 
  labs(x = "Year", y = "Name", title = "CEO names over time",
       subtitle = "How the six most common names for CEOs of S&P companies has changed with time",
       color = "First name",  caption = "Data source: Gentry et al. (2021),
  Visualised by: @Rosie_Baillie_") +
  scale_color_brewer(palette = "Set2") + 
  theme_classic()
  
