
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


#################################################################
### SM Figure 7 (village.range = 10, population growth = 0.5) ###

###################
### Data import ###

## Group2c = concentrated layout, line2 (-18, 19), barren land resources (10), population growth (0.5), village.range (10)

GROUP2c<-read.csv("Group_2_c.txt", header = TRUE)
# #to make sure all runs go to 100 time steps, repeating the last value of all columns (except for step)
GROUP2c<- as.data.table(GROUP2c)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP2c <- na.locf(GROUP2c)
GROUP2c$step2 <- rep(0:100,100)


GROUP2c<-GROUP2c %>% 
  rename(old_step = step) %>% 
  rename(step = step2)


## Group 2d = concentrated layout, line2 (-18, 19), barren land resources (90), population growth (0.5), village.range (10)

GROUP2d<-read.csv("Group_2_d.txt", header = TRUE)
GROUP2d<- as.data.table(GROUP2d)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP2d <- na.locf(GROUP2d)
GROUP2d$step2 <- rep(0:100,100)

GROUP2d<-GROUP2d %>% 
  rename(old_step = step) %>% 
  rename(step = step2)

GROUP_one_3 <- rbind(GROUP2c, GROUP2d)  %>% 
  mutate(geography = case_when(line2 == "-18" & barren.land == "10" ~ "1.narrow valley, harsh border",
                               line2 == "-18" & barren.land == "90" ~ "2.narrow valley, gentle border",
                               line2 == "19" & barren.land == "10" ~ "3.wide valley, harsh border",
                               line2 == "19" & barren.land == "90" ~ "4.wide valley, gentle border"))



# Calculate percentiles for each step and geography
GROUP_one_3_percentiles <- GROUP_one_3 %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(average.hierarchy, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(average.hierarchy, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(average.hierarchy, probs = 0.75, na.rm = TRUE) )

GROUP_one_3<-GROUP_one_3 %>% 
  filter(step>0)
GROUP_one_3_percentiles <- GROUP_one_3_percentiles %>%
  filter(step > 0)


## Group4c = random layout, green patches (82, 1599), barren land resources (10), population growth (0.5), village.range (10)

GROUP4c<-read.csv("Group_4_c.txt", header = TRUE)
GROUP4c<- as.data.table(GROUP4c)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP4c <- na.locf(GROUP4c)
GROUP4c$step2 <- rep(0:100,100)


GROUP4c<-GROUP4c %>% 
  rename(old_step = step) %>% 
  rename(step = step2)


## Group4d = random layout, green patches (82, 1599), barren land resources (90), population growth (0.5), village.range (10)

GROUP4d<-read.csv("Group_4_d.txt", header = TRUE)
GROUP4d<- as.data.table(GROUP4d)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP4d <- na.locf(GROUP4d)
GROUP4d$step2 <- rep(0:100,100)

GROUP4d<-GROUP4d %>% 
  rename(old_step = step) %>% 
  rename(step = step2)



GROUP_two_3 <- rbind(GROUP4c, GROUP4d)  %>% 
  mutate(geography = case_when(green.patches == "82" & barren.land == "10" ~ "1.small islands, harsh border",
                               green.patches == "82" & barren.land == "90" ~ "2.small islands, gentle border",
                               green.patches == "1599" & barren.land == "10" ~ "3.large islands, harsh border",
                               green.patches == "1599" & barren.land == "90" ~ "4.large islands, gentle border"))



# Calculate percentiles for each step and geography
GROUP_two_3_percentiles <- GROUP_two_3 %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(average.hierarchy, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(average.hierarchy, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(average.hierarchy, probs = 0.75, na.rm = TRUE) )

GROUP_two_3<-GROUP_two_3 %>% 
  filter(step>0)
GROUP_two_3_percentiles <- GROUP_two_3_percentiles %>%
  filter(step > 0)





#####################################################
###### Graphs comparing formation of hierarchy ######

### Concentrated layout graph ###

GROUP_one_3_plot <- ggplot(GROUP_one_3, aes(x=step, y=average.hierarchy, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_one_3_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_one_3_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_one_3_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  # geom_line(data = GROUP_one_3_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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
  ylim(0,7)+
  xlab("")+
  ylab("Mean hierarchy")+
  scale_color_manual(values = safe_colorblind_palette,
                     name="Geographical conditions")+
  ggtitle("Concentrated layout")


### Randon layout graph ###


GROUP_two_3_plot <- ggplot(GROUP_two_3, aes(x=step, y=average.hierarchy, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_two_3_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_two_3_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_two_3_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  #  geom_line(data = GROUP_two_3_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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
  ylim(0,7)+
  xlab("")+
  ylab("")+
  scale_color_manual(values = safe_colorblind_palette,
                     name="Geographical conditions")+
  ggtitle("Random layout")




########################################################################
###### Graphs comparing experienced environmental circumscription ######


GROUP_one_3_exp_environ_percentiles <- GROUP_one_3 %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(environmental.circumscription, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(environmental.circumscription, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(environmental.circumscription, probs = 0.75, na.rm = TRUE) )

GROUP_one_3_exp_environ_plot <- ggplot(GROUP_one_3, aes(x=step, y=environmental.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_one_3_exp_environ_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_one_3_exp_environ_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_one_3_exp_environ_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  #  geom_line(data = GROUP_one_3_exp_environ_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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


# Calculate percentiles for each step and geography
GROUP_two_3_exp_environ_percentiles <- GROUP_two_3 %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(environmental.circumscription, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(environmental.circumscription, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(environmental.circumscription, probs = 0.75, na.rm = TRUE) )


GROUP_two_3_exp_environ_plot <- ggplot(GROUP_two_3, aes(x=step, y=environmental.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_two_3_exp_environ_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_two_3_exp_environ_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_two_3_exp_environ_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  #  geom_line(data = GROUP_two_3_exp_environ_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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



########################################################################
###### Graphs comparing experienced social circumscription ######


# Calculate percentiles for each step and geography
GROUP_one_3_exp_social_percentiles <- GROUP_one_3 %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(social.circumscription, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(social.circumscription, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(social.circumscription, probs = 0.75, na.rm = TRUE) )


GROUP_one_3_exp_social_plot <- ggplot(GROUP_one_3, aes(x=step, y=social.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_one_3_exp_social_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_one_3_exp_social_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_one_3_exp_social_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  #  geom_line(data = GROUP_one_3_exp_social_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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


# Calculate percentiles for each step and geography
GROUP_two_3_exp_social_percentiles <- GROUP_two_3 %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(social.circumscription, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(social.circumscription, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(social.circumscription, probs = 0.75, na.rm = TRUE) )


GROUP_two_3_exp_social_plot <- ggplot(GROUP_two_3, aes(x=step, y=social.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_two_3_exp_social_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_two_3_exp_social_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_two_3_exp_social_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  #  geom_line(data = GROUP_two_3_exp_social_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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

########################################################################
###### Graphs comparing population size ######


# Calculate percentiles for each step and geography
GROUP_one_3_pop_percentiles <- GROUP_one_3 %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(count.villages, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(count.villages, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(count.villages, probs = 0.75, na.rm = TRUE) )


GROUP_one_3_pop_plot <- ggplot(GROUP_one_3, aes(x=step, y=count.villages, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_one_3_pop_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_one_3_pop_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_one_3_pop_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  #  geom_line(data = GROUP_one_3_pop_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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
 # ylim(0,1)+
  xlab("Time step")+
  ylab("Population size")+
  scale_color_manual(values = safe_colorblind_palette,
                     name="Geographical conditions")


# Calculate percentiles for each step and geography
GROUP_two_3_pop_percentiles <- GROUP_two_3 %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(count.villages, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(count.villages, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(count.villages, probs = 0.75, na.rm = TRUE) )


GROUP_two_3_pop_plot <- ggplot(GROUP_two_3, aes(x=step, y=count.villages, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_two_3_pop_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_two_3_pop_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_two_3_pop_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  #  geom_line(data = GROUP_two_3_pop_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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
 # ylim(0,1)+
  xlab("Time step")+
  ylab("")+
  scale_color_manual(values = safe_colorblind_palette,
                     name="Geographical conditions")


#################
### All plots ###

ALL_fig_5 <- 
  (GROUP_one_3_plot | GROUP_two_3_plot) /
  (GROUP_one_3_exp_environ_plot | GROUP_two_3_exp_environ_plot) /
  (GROUP_one_3_exp_social_plot | GROUP_two_3_exp_social_plot) /
  (GROUP_one_3_pop_plot | GROUP_two_3_pop_plot) +
  plot_annotation(#title = 'The effects of circumscription on hierarchy formation',
    theme = theme(plot.title = element_text(size = 25,
                                            hjust = 0.5)),
    tag_levels = 'A') &
  theme(plot.tag.position = c(1, 1),
        plot.tag = element_text(size = 18))
ALL_fig_5


####################################

###  interquartile range at time step 100

GROUP_one_3_percentiles_step100 <- GROUP_one_3_percentiles %>% 
  filter(step == 100)
GROUP_two_3_percentiles_step100 <- GROUP_two_3_percentiles%>% 
  filter(step == 100)

GROUP_one_exp_environ_percentiles_step100 <- GROUP_one_exp_environ_percentiles%>% 
  filter(step == 100)
GROUP_two_exp_environ_percentiles_step100<- GROUP_two_exp_environ_percentiles%>% 
  filter(step == 100)

GROUP_one_exp_social_percentiles_step100<- GROUP_one_exp_social_percentiles%>% 
  filter(step == 100)
GROUP_two_exp_social_percentiles_step100<- GROUP_two_exp_social_percentiles%>% 
  filter(step == 100)
