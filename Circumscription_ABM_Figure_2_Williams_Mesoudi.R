
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



######################################
#### Figure 2 (village.range = 1) ####


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

# Calculate percentiles for each step and geography
GROUP_one_percentiles <- GROUP_one %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(average.hierarchy, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(average.hierarchy, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(average.hierarchy, probs = 0.75, na.rm = TRUE) )

# Filter out step 0 if needed
GROUP_one<-GROUP_one %>% 
  filter(step>0)
GROUP_one_percentiles <- GROUP_one_percentiles %>%
  filter(step > 0)

# Plot the data including percentiles
GROUP_one_plot <- ggplot(GROUP_one, aes(x = step, y = average.hierarchy, color = as.factor(geography))) +
  theme_classic() +
  geom_errorbar(inherit.aes = FALSE, data = GROUP_one_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_one_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
   geom_line(data = GROUP_one_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  # geom_line(data = GROUP_one_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
  geom_point(alpha = 0.03) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_rect(fill = "white"),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 15, hjust = 0.5),
        legend.justification = c(0, 0.9), 
        legend.position = c(0.05, 0.9),
        legend.text = element_text(size = 12)) +
  ylim(0, 16) +
  xlab("") +
  ylab("Mean hierarchy") +
  scale_color_manual(values = safe_colorblind_palette, name = "Geographical conditions") +
  ggtitle("Concentrated layout")

GROUP_one_plot



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


# Calculate percentiles for each step and geography
GROUP_two_percentiles <- GROUP_two %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(average.hierarchy, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(average.hierarchy, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(average.hierarchy, probs = 0.75, na.rm = TRUE) )

GROUP_two<-GROUP_two %>% 
  filter(step>0)
GROUP_two_percentiles <- GROUP_two_percentiles %>%
  filter(step > 0)


GROUP_two_plot <- ggplot(GROUP_two, aes(x=step, y=average.hierarchy, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_two_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
 # geom_line(data = GROUP_two_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_two_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
#  geom_line(data = GROUP_two_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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
  ylab("")+
  scale_color_manual(values = safe_colorblind_palette,
                     name="Geographical conditions")+
  ggtitle("Random layout")

GROUP_two_plot





########################################################################
###### Graphs comparing experienced environmental circumscription ######


# Calculate percentiles for each step and geography
GROUP_one_exp_environ_percentiles <- GROUP_one %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(environmental.circumscription, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(environmental.circumscription, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(environmental.circumscription, probs = 0.75, na.rm = TRUE) )

GROUP_one_exp_environ_plot <- ggplot(GROUP_one, aes(x=step, y=environmental.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_one_exp_environ_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_one_exp_environ_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_one_exp_environ_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  #  geom_line(data = GROUP_one_exp_environ_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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
GROUP_two_exp_environ_percentiles <- GROUP_two %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(environmental.circumscription, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(environmental.circumscription, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(environmental.circumscription, probs = 0.75, na.rm = TRUE) )

GROUP_two_exp_environ_plot <- ggplot(GROUP_two, aes(x=step, y=environmental.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_two_exp_environ_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_two_exp_environ_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_two_exp_environ_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  #  geom_line(data = GROUP_two_exp_environ_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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


# Calculate percentiles for each step and geography
GROUP_one_exp_social_percentiles <- GROUP_one %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(social.circumscription, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(social.circumscription, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(social.circumscription, probs = 0.75, na.rm = TRUE) )

GROUP_one_exp_social_plot <- ggplot(GROUP_one, aes(x=step, y=social.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_one_exp_social_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_one_exp_social_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_one_exp_social_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  #  geom_line(data = GROUP_one_exp_social_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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
GROUP_two_exp_social_percentiles <- GROUP_two %>%
  group_by(step, geography) %>%
  summarise(
    percent25 = quantile(social.circumscription, probs = 0.25, na.rm = TRUE),
    percent50 = quantile(social.circumscription, probs = 0.5, na.rm = TRUE),
    percent75 = quantile(social.circumscription, probs = 0.75, na.rm = TRUE) )

GROUP_two_exp_social_plot <- ggplot(GROUP_two, aes(x=step, y=social.circumscription, color=as.factor(geography)))+
  theme_classic()+
  geom_errorbar(inherit.aes = FALSE, data = GROUP_two_exp_social_percentiles, 
                aes(x = step, ymin = percent25, ymax = percent75, color = as.factor(geography)), 
                width = 0, size = 1.25, alpha = 0.5) +
  # geom_line(data = GROUP_two_exp_social_percentiles, aes(x = step, y = percent25, color = as.factor(geography)), linetype = "dashed") +
  geom_line(data = GROUP_two_exp_social_percentiles, aes(x = step, y = percent50, color = as.factor(geography)), linetype = "solid") +
  #  geom_line(data = GROUP_two_exp_social_percentiles, aes(x = step, y = percent75, color = as.factor(geography)), linetype = "dashed") +
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

ALL_fig_2 <- 
  (GROUP_one_plot | GROUP_two_plot) /
  (GROUP_one_exp_environ_plot | GROUP_two_exp_environ_plot) /
  (GROUP_one_exp_social_plot | GROUP_two_exp_social_plot) +
  plot_annotation(#title = 'The effects of circumscription on hierarchy formation',
    theme = theme(plot.title = element_text(size = 25,
                                            hjust = 0.5)),
    tag_levels = 'A') &
  theme(plot.tag.position = c(1, 1),
        plot.tag = element_text(size = 18))
ALL_fig_2


####################################

###  interquartile range at time step 100

GROUP_one_percentiles_step100 <- GROUP_one_percentiles %>% 
  filter(step == 100)
GROUP_two_percentiles_step100 <- GROUP_two_percentiles%>% 
  filter(step == 100)

GROUP_one_exp_environ_percentiles_step100 <- GROUP_one_exp_environ_percentiles%>% 
  filter(step == 100)
GROUP_two_exp_environ_percentiles_step100<- GROUP_two_exp_environ_percentiles%>% 
  filter(step == 100)

GROUP_one_exp_social_percentiles_step100<- GROUP_one_exp_social_percentiles%>% 
  filter(step == 100)
GROUP_two_exp_social_percentiles_step100<- GROUP_two_exp_social_percentiles%>% 
  filter(step == 100)

