#################################################################################################################
## A formal test using agent-based models of the circumscription theory for the evolution of social complexity ##
## Williams and Mesoudi ##
#################################################################################################################


library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Rmisc)
library(reshape2)
library(viridis)
library(data.table)
library(zoo)
library(ggsci)
library(patchwork) 



#################################################################
#### Figure SM 8 (village.range = 1, probability.grow = 0.1) ####


###################
### Data import ###


# Read and preprocess GROUP1a
GROUP1a <- read.csv("Group_1_a.txt", header = TRUE)
GROUP1a <- as.data.table(GROUP1a)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP1a <- na.locf(GROUP1a)
GROUP1a$step2 <- rep(0:100, 100)

GROUP1a <- GROUP1a %>% 
  rename(old_step = step, step = step2)

# Read and preprocess GROUP1b
GROUP1b <- read.csv("Group_1_b.txt", header = TRUE)
GROUP1b <- as.data.table(GROUP1b)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP1b <- na.locf(GROUP1b)
GROUP1b$step2 <- rep(0:100, 100)

GROUP1b <- GROUP1b %>% 
  rename(old_step = step, step = step2)

# Combine and add geography condition
GROUP_one <- rbind(GROUP1a, GROUP1b) %>% 
  mutate(geography = case_when(
    line2 == "-18" & barren.land == "10" ~ "1.narrow valley, harsh border",
    line2 == "-18" & barren.land == "90" ~ "2.narrow valley, gentle border",
    line2 == "19" & barren.land == "10" ~ "3.wide valley, harsh border",
    line2 == "19" & barren.land == "90" ~ "4.wide valley, gentle border"
  ))


# Filter out step 0 if needed
GROUP_one<-GROUP_one %>% 
  filter(step>0)


## Group3a = random layout, green patches (82, 1599), barren land resources (10), population growth (0.1)

GROUP3a<-read.csv("Group_3_a.txt", header = TRUE)
GROUP3a<- as.data.table(GROUP3a)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP3a <- na.locf(GROUP3a)
GROUP3a$step2 <- rep(0:100,100)


GROUP3a<-GROUP3a %>% 
  rename(old_step = step) %>% 
  rename(step = step2)


## Group3b = random layout, green patches (82, 1599), barren land resources (90), population growth (0.1)

GROUP3b<-read.csv("Group_3_b.txt", header = TRUE)
GROUP3b<- as.data.table(GROUP3b)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP3b <- na.locf(GROUP3b)
GROUP3b$step2 <- rep(0:100,100)

GROUP3b<-GROUP3b %>% 
  rename(old_step = step) %>% 
  rename(step = step2)



GROUP_two <- rbind(GROUP3a, GROUP3b)  %>% 
  mutate(geography = case_when(green.patches == "82" & barren.land == "10" ~ "1.small islands, harsh border",
                               green.patches == "82" & barren.land == "90" ~ "2.small islands, gentle border",
                               green.patches == "1599" & barren.land == "10" ~ "3.large islands, harsh border",
                               green.patches == "1599" & barren.land == "90" ~ "4.large islands, gentle border"))


GROUP_two<-GROUP_two %>% 
  filter(step>0)





#### Plots to combine all parameter conditions ####

u=which(colnames(GROUP_two)%in%colnames(GROUP_one))
v=which(colnames(GROUP_one)%in%colnames(GROUP_two))
GROUP=rbind(GROUP_two[,..u],GROUP_one[,..v])


## environemtnal circumscription and hierarchy

GROUP_environ_hier <- ggplot(GROUP, aes(x=environmental.circumscription, y=average.hierarchy, color=step))+
  theme_classic()+
  geom_point()+
  theme(plot.title=element_text(hjust=0.5,
                                size=20),
        strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=20),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=20),
        axis.title = element_text(size=18),
        legend.title = element_text(size=20,
                                    hjust=0.5),
        legend.position="right",
        legend.text=element_text(size=15))+
  scale_y_log10()+
  xlim(0,1)+
  xlab("Experienced environmental circumscription")+
  ylab("Log10 mean hierarchy")+
  scale_colour_gradient(low = "#DDCC77", high = "#117733",
                        name = "Time step")

#reversed axes
# GROUP_environ_hier <- ggplot(GROUP, aes(x=average.hierarchy, y=environmental.circumscription, color=step))+
#   theme_classic()+
#   geom_point()+
#   theme(plot.title=element_text(hjust=0.5,
#                                 size=20),
#         strip.text.x = element_text(size=20),
#         strip.text.y = element_text(size=20),
#         strip.background = element_rect(fill="white"),
#         axis.text =element_text(size=20),
#         axis.title = element_text(size=18),
#         legend.title = element_text(size=20,
#                                     hjust=0.5),
#         legend.position="right",
#         legend.text=element_text(size=15))+
#   scale_y_log10()+
#   ylim(0,1)+
#   xlab("Log10 mean hierarchy")+
#   ylab("Experienced environmental circumscription")+
#   scale_colour_gradient(low = "lightgreen", high = "forestgreen",
#                         name = "Time step")

## social circumscription and hierarchy

GROUP_social_hier <- ggplot(GROUP, aes(x=social.circumscription, y=average.hierarchy, color=step))+
  theme_classic()+
  geom_point()+
  theme(plot.title=element_text(hjust=0.5,
                                size=20),
        strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=20),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=20),
        axis.title = element_text(size=18),
        legend.title = element_text(size=20,
                                    hjust=0.5),
        legend.position="right",
        legend.text=element_text(size=15))+
  scale_y_log10()+
  xlim(0,1)+
  xlab("Experienced social circumscription")+
  ylab("Log10 mean hierarchy")+
  scale_colour_gradient(low = "#DDCC77", high = "#661100",
                        name = "Time step")

#reversed axes
# GROUP_social_hier <- ggplot(GROUP, aes(x=average.hierarchy, y=social.circumscription, color=step))+
#   theme_classic()+
#   geom_point()+
#   theme(plot.title=element_text(hjust=0.5,
#                                 size=20),
#         strip.text.x = element_text(size=20),
#         strip.text.y = element_text(size=20),
#         strip.background = element_rect(fill="white"),
#         axis.text =element_text(size=20),
#         axis.title = element_text(size=18),
#         legend.title = element_text(size=20,
#                                     hjust=0.5),
#         legend.position="right",
#         legend.text=element_text(size=15))+
#   scale_y_log10()+
#   ylim(0,1)+
#   xlab("Log10 mean hierarchy")+
#   ylab("Experienced social circumscription")+
#   scale_colour_gradient(low = "darkgoldenrod1", high = "darkgoldenrod4",
#                         name = "Time step")


## environmetnal and social circumscription

GROUP_social_environ <- ggplot(GROUP, aes(x=social.circumscription, y=environmental.circumscription, color=step))+
  theme_classic()+
  geom_point()+
  theme(plot.title=element_text(hjust=0.5,
                                size=20),
        strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=20),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=20),
        axis.title = element_text(size=18),
        legend.title = element_text(size=20,
                                    hjust=0.5),
        legend.position="right",
        legend.text=element_text(size=15))+
  xlim(0,1)+
  ylim(0,1)+
  xlab("Experienced social circumscription")+
  ylab("Experienced environmental circumscription")+
  scale_colour_gradient(low = "#DDCC77", high = "#332288",
                        name = "Time step")


#reversed axes
# GROUP_social_environ <- ggplot(GROUP, aes(x=environmental.circumscription, y=social.circumscription, color=step))+
#   theme_classic()+
#   geom_point()+
#   theme(plot.title=element_text(hjust=0.5,
#                                 size=20),
#         strip.text.x = element_text(size=20),
#         strip.text.y = element_text(size=20),
#         strip.background = element_rect(fill="white"),
#         axis.text =element_text(size=20),
#         axis.title = element_text(size=18),
#         legend.title = element_text(size=20,
#                                     hjust=0.5),
#         legend.position="right",
#         legend.text=element_text(size=15))+
#   xlim(0,1)+
#   ylim(0,1)+
#   xlab("Experienced environmental circumscription")+
#   ylab("Experienced social circumscription")+
#   scale_colour_gradient(low = "plum1", high = "darkorchid4",
#                         name = "Time step")


### combine plots into one figure ###

GROUP_plot <-
  (GROUP_environ_hier | GROUP_social_hier) /
  (GROUP_social_environ)
