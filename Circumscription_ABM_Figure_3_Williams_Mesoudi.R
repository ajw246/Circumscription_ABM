
#################################################################################################################
## A formal test using agent-based models of the circumscription theory for the evolution of social complexity ##
                    ## Williams and Mesoudi (2023) Preprint, SocArXiv ##
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


safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733")



######################################
#### Figure 3 (village.range = 1) ####


###################
### Data import ###


## Group1a = concentrated layout, line2 (-18, 19), barren land resources (10), population growth (0.1)

GROUP1a<-read.csv("Group_1_a.txt", header = TRUE)
# #to make sure all runs go to 100 time steps, repeating the last value of all columns (except for step)
GROUP1a<- as.data.table(GROUP1a)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP1a <- na.locf(GROUP1a)
GROUP1a$step2 <- rep(0:100,100)


GROUP1a<-GROUP1a %>% 
  rename(old_step = step) %>% 
  rename(step = step2)


## Group 1b = concentrated layout, line2 (-18, 19), barren land resources (90), population growth (0.1)

GROUP1b<-read.csv("Group_1_b.txt", header = TRUE)
GROUP1b<- as.data.table(GROUP1b)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP1b <- na.locf(GROUP1b)
GROUP1b$step2 <- rep(0:100,100)

GROUP1b<-GROUP1b %>% 
  rename(old_step = step) %>% 
  rename(step = step2)

GROUP_one <- rbind(GROUP1a, GROUP1b)  %>% 
  mutate(geography = case_when(line2 == "-18" & barren.land == "10" ~ "1.narrow valley, harsh border",
                               line2 == "-18" & barren.land == "90" ~ "2.narrow valley, gentle border",
                               line2 == "19" & barren.land == "10" ~ "3.wide valley, harsh border",
                               line2 == "19" & barren.land == "90" ~ "4.wide valley, gentle border"))



GROUP_one_SE<- summarySE(GROUP_one, measurevar="average.hierarchy", groupvars=c("step", "geography"), na.rm=FALSE)

GROUP_one<-GROUP_one %>% 
  filter(step>0)
GROUP_one_SE<-GROUP_one_SE %>% 
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



GROUP_two_SE<- summarySE(GROUP_two, measurevar="average.hierarchy", groupvars=c("step", "geography"), na.rm=FALSE)

GROUP_two<-GROUP_two %>% 
  filter(step>0)
GROUP_two_SE<-GROUP_two_SE %>% 
  filter(step>0)




#####################################################
###### Graphs comparing formation of hierarchy ######

### Concentrated layout graph ###

GROUP_one_plot <- ggplot(GROUP_one, aes(x=step, y=average.hierarchy, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_one_SE, 
                aes( x=step, ymin=average.hierarchy-se, ymax=average.hierarchy+se, 
                    color=as.factor(geography)), 
                width=0, size=1.25)+
  geom_point(alpha = 0.03)+
  theme(plot.title=element_text(hjust=0.5,
                                size=20),
        strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=20),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=20),
        axis.title = element_text(size=18),
        legend.title = element_text(size=15,
                                    hjust=0.5),
        #legend.position="none",
        legend.justification=c(0,0.9), 
        legend.position=c(0.05,0.9),
        legend.text=element_text(size=12))+
  ylim(0,16)+
  xlab("")+
  ylab("Mean hierarchy")+
  scale_color_manual(values = safe_colorblind_palette,
                     name="Geographical conditions")+
  ggtitle("Concentrated layout")



### Randon layout graph ###


GROUP_two_plot <- ggplot(GROUP_two, aes(x=step, y=average.hierarchy, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_two_SE, 
                aes(x=step, ymin=average.hierarchy-se, ymax=average.hierarchy+se, 
                    color=as.factor(geography)), 
                width=0, size=1.25)+
  geom_point(alpha = 0.03)+
  theme(plot.title=element_text(hjust=0.5,
                                size=20),
        strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=20),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=20),
        axis.title = element_text(size=18),
        legend.title = element_text(size=15,
                                    hjust=0.5),
        #legend.position="none",
        legend.justification=c(0,0.9), 
        legend.position=c(0.05,0.9),
        legend.text=element_text(size=12))+
   ylim(0,16)+
  xlab("/n")+
  ylab("")+
  scale_color_manual(values = safe_colorblind_palette,
                     name="Geographical conditions")+
  ggtitle("Random layout")





########################################################################
###### Graphs comparing experienced environmental circumscription ######


GROUP_one_exp_environ_SE<- summarySE(GROUP_one, measurevar="environmental.circumscription", groupvars=c("step", "geography"), na.rm=FALSE)

GROUP_one_exp_environ_plot <- ggplot(GROUP_one, aes(x=step, y=environmental.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_one_exp_environ_SE, 
                aes(x=step, ymin=environmental.circumscription-se, ymax=environmental.circumscription+se, 
                    color=as.factor(geography)), 
                width=0, size=1.25)+
  geom_point(alpha = 0.03)+
  theme(plot.title=element_text(hjust=0.5,
                                size=20),
        strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=20),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=20),
        axis.title = element_text(size=18),
        legend.title = element_text(size=20,
                                    hjust=0.5),
        legend.position="none",
        legend.text=element_text(size=15))+
  ylim(0,1)+
  xlab("")+
  ylab("Experienced geographical circumscription")+
  scale_color_manual(values = safe_colorblind_palette,
                     name="Geographical conditions")


GROUP_two_exp_environ_SE<- summarySE(GROUP_two, measurevar="environmental.circumscription", groupvars=c("step", "geography"), na.rm=FALSE)

GROUP_two_exp_environ_plot <- ggplot(GROUP_two, aes(x=step, y=environmental.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_two_exp_environ_SE, 
                aes(x=step, ymin=environmental.circumscription-se, ymax=environmental.circumscription+se, 
                    color=as.factor(geography)), 
                width=0, size=1.25)+
  geom_point(alpha = 0.03)+
  theme(plot.title=element_text(hjust=0.5,
                                size=20),
        strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=20),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=20),
        axis.title = element_text(size=18),
        legend.title = element_text(size=20,
                                    hjust=0.5),
        legend.position="none",
        legend.text=element_text(size=15))+
  ylim(0,1)+
  xlab("")+
  ylab("")+
  scale_color_manual(values = safe_colorblind_palette,
                     name="Geographical conditions")





#################################################################
###### Graphs comparing experienced social circumscription ######


GROUP_one_exp_social_SE<- summarySE(GROUP_one, measurevar="social.circumscription", groupvars=c("step", "geography"), na.rm=FALSE)

GROUP_one_exp_social_plot <- ggplot(GROUP_one, aes(x=step, y=social.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_one_exp_social_SE, 
                aes(x=step, ymin=social.circumscription-se, ymax=social.circumscription+se, 
                    color=as.factor(geography)), 
                width=0, size=1.25)+
  geom_point(alpha = 0.03)+
  theme(plot.title=element_text(hjust=0.5,
                                size=20),
        strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=20),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=20),
        axis.title = element_text(size=18),
        legend.title = element_text(size=20,
                                    hjust=0.5),
        legend.position="none",
        legend.text=element_text(size=15))+
  ylim(0,1)+
  xlab("Time step")+
  ylab("Experienced social circumscription")+
  scale_color_manual(values = safe_colorblind_palette,
                     name="Geographical conditions")


GROUP_two_exp_social_SE<- summarySE(GROUP_two, measurevar="social.circumscription", groupvars=c("step", "geography"), na.rm=FALSE)

GROUP_two_exp_social_plot <- ggplot(GROUP_two, aes(x=step, y=social.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_two_exp_social_SE, 
                aes(x=step, ymin=social.circumscription-se, ymax=social.circumscription+se, 
                    color=as.factor(geography)), 
                width=0, size=1.25)+
  geom_point(alpha = 0.03)+
  theme(plot.title=element_text(hjust=0.5,
                                size=20),
        strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=20),
        strip.background = element_rect(fill="white"),
        axis.text =element_text(size=20),
        axis.title = element_text(size=18),
        legend.title = element_text(size=20,
                                    hjust=0.5),
        legend.position="none",
        legend.text=element_text(size=15))+
  ylim(0,1)+
  xlab("Time step")+
  ylab("")+
  scale_color_manual(values = safe_colorblind_palette,
                     name="Geographical conditions")



#################
### All plots ###

ALL_fig_3 <- 
  (GROUP_one_plot | GROUP_two_plot) /
  (GROUP_one_exp_environ_plot | GROUP_two_exp_environ_plot) /
  (GROUP_one_exp_social_plot | GROUP_two_exp_social_plot) +
  plot_annotation(#title = 'The effects of circumscription on hierarchy formation',
                  theme = theme(plot.title = element_text(size = 25,
                                                          hjust = 0.5)),
                  tag_levels = 'A') &
  theme(plot.tag.position = c(1, 1),
        plot.tag = element_text(size = 18))
ALL_fig_3
