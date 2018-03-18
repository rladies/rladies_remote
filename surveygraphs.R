library(googlesheets)
library(tidyverse)
library(stringr)
library(RColorBrewer)
results <- gs_title("R ladies remote") %>% gs_read


table(results$Country)

map1 = results %>% 
  mutate(country = case_when((tolower(Country) %in% 
                    c('us','usa','u.s.a.','south bend'))|
               grepl('united states',tolower(Country))|
                   (Country=='Georgia'&City=='Atlanta')~'USA',
               grepl('united kingdom|england|scotland|wales|reino unido',
                       tolower(Country))|(tolower(Country)=='uk')~'UK',
             grepl('new zealand|nz',tolower(Country))~'New Zealand',
             grepl('germ|deutschland',tolower(Country))~'Germany',
             grepl('canada|canda',tolower(Country))~'Canada',
             grepl('argenti',tolower(Country))~'Argentina',
             grepl('netherland|holland',tolower(Country))~'Netherlands',
             grepl('colombi',tolower(Country))~'Colombia',
             tolower(Country) %in% c('spain','espaÃ±a')~'Spain',
             tolower(Country) %in% c('italy','italia')~'Italy',
             tolower(Country) %in% c('norge','norway')~'Norway',
             tolower(Country) %in% c('brazil','brasil')~'Brazil',
                 TRUE~str_to_title(Country))) %>%
  group_by(country) %>%
  summarize(count=n())

world <- map_data("world")

uniquecounts <- as.character(c(0,1,2,3,4,5,6,7,8,17,
                               20,23,25,34,162))


newdat <- full_join(world, map1, by=c("region"="country")) %>%
          mutate(count = ifelse(is.na(count),0,count),
                 count = factor(count, levels=uniquecounts))


uniquecolors <- c(NA, #0
                  "#e0ecf4","#e0ecf4","#e0ecf4", #1,2,3
                  "#bfd3e6","#bfd3e6","#bfd3e6", #4,5,6
                  "#9ebcda","#9ebcda", #7,8
                  "#8c96c6", #17
                  "#8c6bb1","#8c6bb1", #20, 23
                  "#88419d", #25
                  "#810f7c", #34
                  "#4d004b") #162


g1 <- ggplot(data=newdat, 
       aes(long, lat))+
  geom_polygon(aes(group=group, fill=count),color="black")+
  theme_bw()+
  theme(panel.background = element_rect(fill="darkgray"))+
  scale_fill_manual(breaks=uniquecounts,
                    values=uniquecolors)

results_nrow <- nrow(results)

graph2 <- results %>%
  group_by(`What is your current level of R knowledge?`) %>%
  summarise(count=n(),
            proportion=count/results_nrow) %>%
  ungroup() 

graph2$Rlevel = str_wrap(graph2$`What is your current level of R knowledge?`, width=7)

unilevels <- unique(graph2$Rlevel)[c(5,1,2,3,4)]

graph2$Rlevel <- factor(graph2$Rlevel, 
                        levels=unilevels)

purples <- brewer.pal(5,"PRGn")

g2 <- ggplot(data=graph2, 
       aes(x=Rlevel,
           y=count))+
  geom_col(aes(fill=Rlevel), color="black")+
  theme_bw()+
  scale_fill_manual(values=purples)+
  theme(legend.position="none")+
  xlab("R Experience Level")+
  ylab("Count of Respondants")


results$`Which one(s) below would you be interested in? Select all that apply` = gsub(', ([[:upper:]])','| \\U\\1',results$`Which one(s) below would you be interested in? Select all that apply`,perl=TRUE)

graph3 <- strsplit(results$`Which one(s) below would you be interested in? Select all that apply`,'|',fixed=TRUE) %>% 
        unlist() %>%  
        trimws()  %>% 
        table() %>% 
        data.frame() %>% 
        mutate(prop = Freq/nrow(results)) 

colnames(graph3)[1] <- "interest"

graph3$interest <- str_wrap(graph3$interest
                            , width=20)

purples <- brewer.pal(9,"PRGn")

a <- unique(graph3$interest)[1:4]
b <- unique(graph3$interest)[5:9]

g3a <- graph3 %>%
       filter(interest %in% a) %>%
       ggplot( aes(x=interest,
           y=Freq))+
  geom_col(aes(fill=interest), color="black")+
  theme_bw()+
  scale_fill_manual(values=purples[1:4])+
  theme(legend.position="none")+
  xlab("Interest")+
  ylab("Counts")

g3b <- graph3 %>%
  filter(interest %in% b) %>%
  ggplot( aes(x=interest,
              y=Freq))+
  geom_col(aes(fill=interest), color="black")+
  theme_bw()+
  scale_fill_manual(values=purples[5:9])+
  theme(legend.position="none")+
  xlab("Interest")+
  ylab("Counts")

g3 <- cowplot::plot_grid(g3a, g3b, ncol=1)

graph4 <- strsplit(results$`Why are you interested in R-Ladies Remote?  (select all applicable)`,',',fixed=TRUE) %>% 
  unlist() %>%  
  trimws()  %>% 
  table() %>% 
  data.frame() %>% 
  mutate(prop = Freq/nrow(results)) 

colnames(graph4)[1] <- "why"

graph4$why <- str_wrap(graph4$why
                            , width=10)

purples <- brewer.pal(6,"PRGn")

g4 <- ggplot(data=graph4, 
       aes(x=why,
           y=Freq))+
  geom_col(aes(fill=why), color="black")+
  theme_bw()+
  scale_fill_manual(values=purples)+
  theme(legend.position="none")+
  xlab("Why R-Ladies")+
  ylab("Counts")

ggsave(g1, filename="survey_g1_world_map.jpeg",
       width=15, height=10, units="cm", dpi=300)

ggsave(g2, filename="survey_g2_r_level.jpeg",
       width=15, height=10, units="cm", dpi=300)

ggsave(g3,  filename="survey_g3_what_interested_in.jpeg",
       width=20, height=15, units="cm", dpi=300)

ggsave(g4,  filename="survey_g4_why_interested_in.jpeg",
       width=15, height=10, units="cm", dpi=300)

