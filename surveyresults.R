library(googlesheets)
library(tidyverse)
library(stringr)

results <- gs_title("R ladies remote") %>% gs_read


table(results$Country)
 
results = results %>% mutate(country = case_when((tolower(Country) %in% c('us','usa','south bend'))|grepl('united states',tolower(Country))|(Country=='Georgia'&City=='Atlanta')~'USA',
                                                 grepl('united kingdom|england|scotland|wales|reino unido',tolower(Country))|(tolower(Country)=='uk')~'UK',
                                                 grepl('new zealand|nz',tolower(Country))~'New Zealand',
                                                 grepl('germ|deutschland',tolower(Country))~'Germany',
                                                 grepl('canada|canda',tolower(Country))~'Canada',
                                                 grepl('argenti',tolower(Country))~'Argentina',
                                                 grepl('netherland|holland',tolower(Country))~'Netherlands',
                                                 grepl('colombi',tolower(Country))~'Colombia',
                                                 tolower(Country) %in% c('spain','españa')~'Spain',
                                                 tolower(Country) %in% c('italy','italia')~'Spain',
                                                 tolower(Country) %in% c('norge','norway')~'Norway',
                                                 tolower(Country) %in% c('brazil','brasil')~'Brazil',
                                                 TRUE~str_to_title(Country)))
                             
    
sort(table(results$country))

prop.table(table(results$`What is your current level of R knowledge?`))

interests = c('Learning R (for those who are new to the language)',
              'Learning new R skills, in a webinar',
              'Learning new R skills, paired with one other person or a small group',
              'Online Office hours about different topics, where you can drop in, as a question, about R, about consultant work, about a certain package, and get some help',
              'Mentoring for folks at different career stages/experience levels',
              'Talks about working remotely',
              'Talks about doing R consulting part or fulltime',
              'Talks about fighting prejudices (e.g. being a woman, being a mother, wanting to work remotely)',
              'Resume or portfolio reviews')


