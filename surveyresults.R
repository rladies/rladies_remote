library(googlesheets)
library(tidyverse)
library(stringr)

results <- gs_title("R ladies remote") %>% gs_read


table(results$Country)
 
results = results %>% mutate(country = case_when(tolower(Country) %in% c('us','usa','united states')~'USA',
                                                 grepl('uk|united kingdom|england|scotland|gb|wales|reino unido',tolower(Country))~'UK',
                                                 grepl('new zealand|nz',tolower(Country))~'New Zealand',
                                                 grepl('germ',tolower(Country))~'Germany',
                                                 grepl('canada|canda',tolower(Country))~'Canada',
                                                 grepl('argenti',tolower(Country))~'Argentina',
                                                 grepl('netherland|holland',tolower(Country))~'Netherlands',
                                                 grepl('colombi',tolower(Country))~'Colombia',
                                                 TRUE~str_to_title(Country)))
                             
    
table(results$country)

prop.table(table(results$`What is your current level of R knowledge?`))

