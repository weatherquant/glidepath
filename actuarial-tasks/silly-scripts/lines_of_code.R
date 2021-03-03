# Load two packages
library(dplyr)
library(stringr)
# Count your lines of R code - Source Scripts Folder
source_scripts = list.files(path = "actuarial-tasks/source_scripts", recursive = T, full.names = T) %>%
  str_subset("[.][R]$") %>%
  sapply(function(x) x %>% readLines() %>% length()) %>%
  sum()
app_r = list.files(path = "actuarial-tasks", recursive = F, full.names = T) %>%
  str_subset("[.][R]$") %>%
  sapply(function(x) x %>% readLines() %>% length()) %>%
  sum()
print(source_scripts + app_r)