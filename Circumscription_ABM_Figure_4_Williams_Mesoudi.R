
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


safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733")



####################
##### Boxplots #####



#########################
### village.range = 1 ###


# Read and preprocess GROUP1a (concentrated layout, line2 (-18, 19), barren land resources (10), population growth (0.1),, village.range (1))
GROUP1a <- read.csv("Group_1_a.txt", header = TRUE)
GROUP1a <- as.data.table(GROUP1a)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP1a <- na.locf(GROUP1a)
GROUP1a$step2 <- rep(0:100, 100)

GROUP1a <- GROUP1a %>% 
  rename(old_step = step, step = step2)

# Read and preprocess GROUP1b (concentrated layout, line2 (-18, 19), barren land resources (90), population growth (0.1), village.range (1))
GROUP1b <- read.csv("Group_1_b.txt", header = TRUE)
GROUP1b <- as.data.table(GROUP1b)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP1b <- na.locf(GROUP1b)
GROUP1b$step2 <- rep(0:100, 100)

GROUP1b <- GROUP1b %>% 
  rename(old_step = step, step = step2)

# Combine and add geography condition
GROUP_one <- rbind(GROUP1a, GROUP1b) %>% 
  mutate(geography = case_when(
    line2 == "-18" & barren.land == "10" ~ "1.narrow valley,\nharsh border",
    line2 == "-18" & barren.land == "90" ~ "2.narrow valley,\ngentle border",
    line2 == "19" & barren.land == "10" ~ "3.wide valley,\nharsh border",
    line2 == "19" & barren.land == "90" ~ "4.wide valley,\ngentle border"
  )) 

# Filter out step 0 if needed
GROUP_one<-GROUP_one %>%
  filter(step == 100)


# Plot the data including percentiles
GROUP_one_plot <- ggplot(GROUP_one, aes(x = geography, y = average.hierarchy, color = as.factor(geography), fill = as.factor(geography))) +
  theme_classic() +
  geom_boxplot(aes(x = geography, y = average.hierarchy, color = as.factor(geography), fill = as.factor(geography), alpha = 0.2)) +
  geom_point(shape =1, alpha = 1,  position=position_jitter(width=0.05,height=0)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        axis.text = element_text(size = 16, angle=0),
        axis.title = element_text(size = 18)) +
  theme(legend.position="none")+
  ylim(0, 16) +
  xlab("") +
  ylab("Mean hierarchy") +
  scale_fill_manual(values = safe_colorblind_palette, name = "Geographical conditions") +
  scale_color_manual(values = safe_colorblind_palette, name = "Geographical conditions") +
  ggtitle("Concentrated layout\nvillage.range = 1")

GROUP_one_plot






## Group3a = random layout, green patches (82, 1599), barren land resources (10), population growth (0.1)

GROUP3a<-read.csv("Group_3_a.txt", header = TRUE)
GROUP3a<- as.data.table(GROUP3a)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP3a <- na.locf(GROUP3a)
GROUP3a$step2 <- rep(0:100,100)


GROUP3a<-GROUP3a %>% 
  rename(old_step = step) %>% 
  rename(step = step2)


#### Group3b = random layout, green patches (82, 1599), barren land resources (90), population growth (0.1)####

GROUP3b<-read.csv("Group_3_b.txt", header = TRUE)
GROUP3b<- as.data.table(GROUP3b)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP3b <- na.locf(GROUP3b)
GROUP3b$step2 <- rep(0:100,100)

GROUP3b<-GROUP3b %>% 
  rename(old_step = step) %>% 
  rename(step = step2)



GROUP_two <- rbind(GROUP3a, GROUP3b)  %>% 
  mutate(geography = case_when(green.patches == "82" & barren.land == "10" ~ "1.small islands,\nharsh border",
                               green.patches == "82" & barren.land == "90" ~ "2.small islands,\ngentle border",
                               green.patches == "1599" & barren.land == "10" ~ "3.large islands,\nharsh border",
                               green.patches == "1599" & barren.land == "90" ~ "4.large islands,\ngentle border"))

GROUP_two<-GROUP_two %>% 
  filter(step==100)


GROUP_two_plot <- ggplot(GROUP_two, aes(x = geography, y = average.hierarchy, color = as.factor(geography), fill = as.factor(geography)))+
  theme_classic()+
  geom_boxplot(aes(x = geography, y = average.hierarchy, color = as.factor(geography), fill = as.factor(geography), alpha = 0.2)) +
  geom_point(shape =1, alpha = 1,  position=position_jitter(width=0.05,height=0)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        axis.text = element_text(size = 16, angle=0),
        axis.title = element_text(size = 18)) +
  theme(legend.position="none")+
  ylim(0,16)+
  xlab("")+
  ylab("")+
  scale_color_manual(values = safe_colorblind_palette, name="Geographical conditions")+
  scale_fill_manual(values = safe_colorblind_palette, name = "Geographical conditions") +
  ggtitle("Random layout\nvillage.range = 1")

GROUP_two_plot



##########################
### village.range = 10 ###

## Group1c = concentrated layout, line2 (-18, 19), barren land resources (10), population growth (0.1), village.range (10)

GROUP1c<-read.csv("Group_1_c.txt", header = TRUE)
GROUP1c<- as.data.table(GROUP1c)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP1c <- na.locf(GROUP1c)
GROUP1c$step2 <- rep(0:100,100)


GROUP1c<-GROUP1c %>% 
  rename(old_step = step) %>% 
  rename(step = step2)


#### Group 1d = concentrated layout, line2 (-18, 19), barren land resources (90), population growth (0.1), village.range (10)####

GROUP1d<-read.csv("Group_1_d.txt", header = TRUE)
GROUP1d<- as.data.table(GROUP1d)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP1d <- na.locf(GROUP1d)
GROUP1d$step2 <- rep(0:100,100)

GROUP1d<-GROUP1d %>% 
  rename(old_step = step) %>% 
  rename(step = step2)

GROUP_one_2 <- rbind(GROUP1c, GROUP1d)  %>% 
  mutate(geography = case_when(line2 == "-18" & barren.land == "10" ~ "1.narrow valley,\nharsh border",
                               line2 == "-18" & barren.land == "90" ~ "2.narrow valley,\ngentle border",
                               line2 == "19" & barren.land == "10" ~ "3.wide valley,\nharsh border",
                               line2 == "19" & barren.land == "90" ~ "4.wide valley,\ngentle border"))


GROUP_one_2<-GROUP_one_2 %>% 
  filter(step==100)



GROUP_one_2_plot <- ggplot(GROUP_one_2, aes(x = geography, y = average.hierarchy, color = as.factor(geography), fill = as.factor(geography)))+
  theme_classic()+
  geom_boxplot(aes(x = geography, y = average.hierarchy, color = as.factor(geography), fill = as.factor(geography), alpha = 0.2)) +
  geom_point(shape =1, alpha = 1,  position=position_jitter(width=0.05,height=0)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        axis.text = element_text(size = 16, angle=0),
        axis.title = element_text(size = 18)) +
  theme(legend.position="none")+
  ylim(0,16)+
  xlab("\nGeographical condition")+
  ylab("Mean hierarchy")+
  scale_color_manual(values = safe_colorblind_palette, name="Geographical conditions")+
  scale_fill_manual(values = safe_colorblind_palette, name = "Geographical conditions") +
  ggtitle("village.range = 10")




#### Group3c = random layout, green patches (82, 1599), barren land resources (10), population growth (0.1), village.range (10)####

GROUP3c<-read.csv("Group_3_c.txt", header = TRUE)
GROUP3c<- as.data.table(GROUP3c)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP3c <- na.locf(GROUP3c)
GROUP3c$step2 <- rep(0:100,100)


GROUP3c<-GROUP3c %>% 
  rename(old_step = step) %>% 
  rename(step = step2)


## Group3d = random layout, green patches (82, 1599), barren land resources (90), population growth (0.1), village.range

GROUP3d<-read.csv("Group_3_d.txt", header = TRUE)
GROUP3d<- as.data.table(GROUP3d)[, lapply(.SD, `length<-`, 101), by = run.number]
GROUP3d <- na.locf(GROUP3d)
GROUP3d$step2 <- rep(0:100,100)

GROUP3d<-GROUP3d %>% 
  rename(old_step = step) %>% 
  rename(step = step2)



GROUP_two_2 <- rbind(GROUP3c, GROUP3d)  %>% 
  mutate(geography = case_when(green.patches == "82" & barren.land == "10" ~ "1.small islands,\nharsh border",
                               green.patches == "82" & barren.land == "90" ~ "2.small islands,\ngentle border",
                               green.patches == "1599" & barren.land == "10" ~ "3.large islands,\nharsh border",
                               green.patches == "1599" & barren.land == "90" ~ "4.large islands,\ngentle border"))


GROUP_two_2<-GROUP_two_2 %>% 
  filter(step==100)




GROUP_two_2_plot <- ggplot(GROUP_two_2, aes(x = geography, y = average.hierarchy, color = as.factor(geography), fill = as.factor(geography)))+
  theme_classic()+
  geom_boxplot(aes(x = geography, y = average.hierarchy, color = as.factor(geography), fill = as.factor(geography), alpha = 0.2)) +
  geom_point(shape =1, alpha = 1,  position=position_jitter(width=0.05,height=0)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        axis.text = element_text(size = 16, angle=0),
        axis.title = element_text(size = 18)) +
  theme(legend.position="none")+
  ylim(0,16)+
  xlab("\nGeographical condition")+
  ylab("")+
  scale_color_manual(values = safe_colorblind_palette, name="Geographical conditions")+
  scale_fill_manual(values = safe_colorblind_palette, name = "Geographical conditions") +
  ggtitle("village.range = 10")



#################
### All plots ###

# ALL_fig_4 <- 
#   (GROUP_one_plot | GROUP_two_plot) /
#   (GROUP_one_2_plot | GROUP_two_2_plot) 
#    plot_annotation(
#      theme = theme(plot.title = element_text(size = 25,
#                                              hjust = 0.5)),
#     tag_levels = 'A') &
#   theme(plot.tag.position = c(1, 1),
#         plot.tag = element_text(size = 18))
# ALL_fig_4

multiplot(GROUP_one_plot, GROUP_one_2_plot, GROUP_two_plot, GROUP_two_2_plot, cols=2) 

