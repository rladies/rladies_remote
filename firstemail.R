library(googlesheets)
library(tidyverse)
library(stringr)
library(RColorBrewer)
library(mailR)

results <- gs_title("R ladies remote") %>% gs_read

results = results %>% mutate(country = case_when((tolower(Country) %in% c('us','usa','u.s.a.','south bend'))|grepl('united states',tolower(Country))|(Country=='Georgia'&City=='Atlanta')~'USA',
                                                 grepl('united kingdom|england|scotland|wales|reino unido',tolower(Country))|(tolower(Country)=='uk')~'UK',
                                                 grepl('new zealand|nz',tolower(Country))~'New Zealand',
                                                 grepl('germ|deutschland',tolower(Country))~'Germany',
                                                 grepl('canada|canda',tolower(Country))~'Canada',
                                                 grepl('argenti',tolower(Country))~'Argentina',
                                                 grepl('netherland|holland',tolower(Country))~'Netherlands',
                                                 grepl('colombi',tolower(Country))~'Colombia',
                                                 tolower(Country) %in% c('spain','españa')~'Spain',
                                                 tolower(Country) %in% c('italy','italia')~'Italy',
                                                 tolower(Country) %in% c('norge','norway')~'Norway',
                                                 tolower(Country) %in% c('brazil','brasil')~'Brazil',
                                                 TRUE~str_to_title(Country)))



text1 = paste("Thank you for signing up to R-Ladies remote. We've had an incredible response from around the globe with",
 nrow(results),"R-Ladies responding from",  length(unique(results$country)),
"different countries and every different continent. We are excited to build a global R community together!",sep=' ')

text2 = paste("We will be launching with out first event on Saturday 5th May at 3pm Mountain Daylight time (10pm BST, 11pm Europe, 7am 6th May Sydney, Australia).",
              "We are very lucky to have an R-Ladies superstar to launch our group.", 
              '<a href="https://juliasilge.com/">Julia Silge</a> is a remote Data Scientist for StackOverflow, has written a book on Textual Analytics and created numerous teaching courses on DataCamp.',
              "Julia's talk is entitled 'Fighting the Pants Industrial Complex One Commit at a Time: Remote Working for R-Ladies'.",
              "We will send out details for accessing the talk nearer the time, but please keep the time and date free!",sep=' ')

text3 = paste("We also have an R-Ladies remote Slack Group. This will allow us to plan events, network and socialise remotely.",
              "During events we will have set social times on the Slack group to work together and meet people.",
              "To access the slack group click on ...",sep=" ")
text4 = paste("We look forward to welcoming you to our community.","The R Ladies Remote team (Abigail, Auriel and Steph)")

body <- paste(" <html>
          <body>
          <h1>Welcome to R-Ladies Remote</h1>",
          "<p>",text1,"</p><p>",text2,"</p><p>",text3,"</p><p>",text4,
        "</body>
      </html>",sep=' ')

send.mail(from = "remote@rladies.org",
          to = c("abigail.lebrecht@gmail.com","remote@rladies.org"),
          subject = "Welcome to R Ladies Remote",
          body =  body ,
          html = TRUE,
          inline = TRUE,
          smtp = list(host.name = "mail.rladies.org", port = 587, user.name = "remote@rladies.org", passwd = "****", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)