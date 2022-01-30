library(tidyverse)
library(ggplot2)
library(ggthemr)
library(cowplot)
library(reshape2)
library(patchwork)
library(ggthemes)

##################

setwd('C:/Users/cjcar/Dropbox/GeoMalaria/PARTables-Renamed')

GLENSs1 <- rbind(read.csv('Pv_stable_GL_1.csv'),read.csv('Pv_stable_GL_1_2020.csv'))
GLENSs2 <- rbind(read.csv('Pv_stable_GL_2.csv'),read.csv('Pv_stable_GL_2_2020.csv'))
GLENSs3 <- rbind(read.csv('Pv_stable_GL_3.csv'),read.csv('Pv_stable_GL_3_2020.csv'))
R8s1 <- rbind(read.csv('Pv_stable_R8_1.csv'),read.csv('Pv_stable_R8_1_2020.csv'))
R8s2 <- rbind(read.csv('Pv_stable_R8_2.csv'),read.csv('Pv_stable_R8_2_2020.csv'))
R8s3 <- rbind(read.csv('Pv_stable_R8_3.csv'),read.csv('Pv_stable_R8_3_2020.csv'))

GLENSs1$Scenario <- 'GLENS'
GLENSs1$run <- 'GLENS1'

GLENSs2$Scenario <- 'GLENS'
GLENSs2$run <- 'GLENS2'

GLENSs3$Scenario <- 'GLENS'
GLENSs3$run <- 'GLENS3'

R8s1$Scenario <- 'RCP 8.5'
R8s1$run <- 'R81'

R8s2$Scenario <- 'RCP 8.5'
R8s2$run <- 'R82'

R8s3$Scenario <- 'RCP 8.5'
R8s3$run <- 'R83'

df <- rbind(GLENSs1, GLENSs2, GLENSs3, R8s1, R8s2, R8s3)
df <- df[,-1]
df <- melt(df, id=c('years','Scenario','run'))
names(df)[names(df)=='variable'] <- 'region'
names(df)[names(df)=='value'] <- 'PAR'


level_key <- c(asiaS = 'Asia (South)',
               asiaSE = 'Asia (Southeast)',
               asiaE = 'Asia (East)',
               latamT = 'Latin America (Tropical)',
               latamC = 'Latin America (Central)',
               latamA = 'Latin America (Andean)')

df$region %>% recode(!!!level_key) -> df$region 
#df$region = factor(df$region, levels(df$region)[c(1,2,4,3)])


#ggthemr('grass') # use fresh for actual paper figure 

df$PAR <- df$PAR / 1000000

df -> vivax.glens.stable

############################

GLENSu1 <- rbind(read.csv('Pv_unstable_GL_1.csv'),read.csv('Pv_unstable_GL_1_2020.csv'))
GLENSu2 <- rbind(read.csv('Pv_unstable_GL_2.csv'),read.csv('Pv_unstable_GL_2_2020.csv'))
GLENSu3 <- rbind(read.csv('Pv_unstable_GL_3.csv'),read.csv('Pv_unstable_GL_3_2020.csv'))
R8u1 <- rbind(read.csv('Pv_unstable_R8_1.csv'),read.csv('Pv_unstable_R8_1_2020.csv'))
R8u2 <- rbind(read.csv('Pv_unstable_R8_2.csv'),read.csv('Pv_unstable_R8_2_2020.csv'))
R8u3 <- rbind(read.csv('Pv_unstable_R8_3.csv'),read.csv('Pv_unstable_R8_3_2020.csv'))

GLENSu1$Scenario <- 'GLENS'
GLENSu1$run <- 'GLENS1'

GLENSu2$Scenario <- 'GLENS'
GLENSu2$run <- 'GLENS2'

GLENSu3$Scenario <- 'GLENS'
GLENSu3$run <- 'GLENS3'

R8u1$Scenario <- 'RCP 8.5'
R8u1$run <- 'R81'

R8u2$Scenario <- 'RCP 8.5'
R8u2$run <- 'R82'

R8u3$Scenario <- 'RCP 8.5'
R8u3$run <- 'R83'

df <- rbind(GLENSu1, GLENSu2, GLENSu3, R8u1, R8u2, R8u3)
df <- df[,-1]
df <- melt(df, id=c('years','Scenario','run'))
names(df)[names(df)=='variable'] <- 'region'
names(df)[names(df)=='value'] <- 'PAR'

level_key <- c(asiaS = 'Asia (South)',
               asiaSE = 'Asia (Southeast)',
               asiaE = 'Asia (East)',
               latamT = 'Latin America (Tropical)',
               latamC = 'Latin America (Central)',
               latamA = 'Latin America (Andean)')

df$region %>% recode(!!!level_key) -> df$region 

df$PAR <- df$PAR / 1000000

df -> vivax.glens.unstable

######################################

G3s1 <- read.csv('Pv_stable_G3_1.csv')
G3s2 <- read.csv('Pv_stable_G3_2.csv')
G3s3 <- read.csv('Pv_stable_G3_3.csv')
R4s1 <- read.csv('Pv_stable_R4_1.csv')
R4s2 <- read.csv('Pv_stable_R4_2.csv')
R4s3 <- read.csv('Pv_stable_R4_3.csv')

G3s1$Scenario <- 'G3'
G3s1$run <- 'G31'

G3s2$Scenario <- 'G3'
G3s2$run <- 'G32'

G3s3$Scenario <- 'G3'
G3s3$run <- 'G33'

R4s1$Scenario <- 'RCP 4.5'
R4s1$run <- 'R41'

R4s2$Scenario <- 'RCP 4.5'
R4s2$run <- 'R42'

R4s3$Scenario <- 'RCP 4.5'
R4s3$run <- 'R43'

df <- rbind(G3s1, G3s2, G3s3, R4s1, R4s2, R4s3)
df <- df[,-1]
df <- melt(df, id=c('years','Scenario','run'))
names(df)[names(df)=='variable'] <- 'region'
names(df)[names(df)=='value'] <- 'PAR'


level_key <- c(asiaS = 'Asia (South)',
               asiaSE = 'Asia (Southeast)',
               asiaE = 'Asia (East)',
               latamT = 'Latin America (Tropical)',
               latamC = 'Latin America (Central)',
               latamA = 'Latin America (Andean)')

df$region %>% recode(!!!level_key) -> df$region 


df$PAR <- df$PAR / 1000000

df -> vivax.g3.stable

######################

G3u1 <- read.csv('Pv_unstable_G3_1.csv')
G3u2 <- read.csv('Pv_unstable_G3_2.csv')
G3u3 <- read.csv('Pv_unstable_G3_3.csv')
R4u1 <- read.csv('Pv_unstable_R4_1.csv')
R4u2 <- read.csv('Pv_unstable_R4_2.csv')
R4u3 <- read.csv('Pv_unstable_R4_3.csv')

G3u1$Scenario <- 'G3'
G3u1$run <- 'G31'

G3u2$Scenario <- 'G3'
G3u2$run <- 'G32'

G3u3$Scenario <- 'G3'
G3u3$run <- 'G33'

R4u1$Scenario <- 'RCP 4.5'
R4u1$run <- 'R41'

R4u2$Scenario <- 'RCP 4.5'
R4u2$run <- 'R42'

R4u3$Scenario <- 'RCP 4.5'
R4u3$run <- 'R43'

df2 <- rbind(G3u1, G3u2, G3u3, R4u1, R4u2, R4u3)
df2 <- df2[,-1]
df2 <- melt(df2, id=c('years','Scenario','run'))
names(df2)[names(df2)=='variable'] <- 'region'
names(df2)[names(df2)=='value'] <- 'PAR'

level_key <- c(asiaS = 'Asia (South)',
               asiaSE = 'Asia (Southeast)',
               asiaE = 'Asia (East)',
               latamT = 'Latin America (Tropical)',
               latamC = 'Latin America (Central)',
               latamA = 'Latin America (Andean)')

df2$region %>% recode(!!!level_key) -> df2$region 

df2$PAR <- df2$PAR / 1000000

df2 -> vivax.g3.unstable

######################################

GLENSs1 <- rbind(read.csv('Pf_stable_GL_1.csv'),read.csv('Pf_stable_GL_1_2020.csv'))
GLENSs2 <- rbind(read.csv('Pf_stable_GL_2.csv'),read.csv('Pf_stable_GL_2_2020.csv'))
GLENSs3 <- rbind(read.csv('Pf_stable_GL_3.csv'),read.csv('Pf_stable_GL_3_2020.csv'))
R8s1 <- rbind(read.csv('Pf_stable_R8_1.csv'),read.csv('Pf_stable_R8_1_2020.csv'))
R8s2 <- rbind(read.csv('Pf_stable_R8_2.csv'),read.csv('Pf_stable_R8_2_2020.csv'))
R8s3 <- rbind(read.csv('Pf_stable_R8_3.csv'),read.csv('Pf_stable_R8_3_2020.csv'))

GLENSs1$Scenario <- 'GLENS'
GLENSs1$run <- 'GLENS1'

GLENSs2$Scenario <- 'GLENS'
GLENSs2$run <- 'GLENS2'

GLENSs3$Scenario <- 'GLENS'
GLENSs3$run <- 'GLENS3'

R8s1$Scenario <- 'RCP 8.5'
R8s1$run <- 'R81'

R8s2$Scenario <- 'RCP 8.5'
R8s2$run <- 'R82'

R8s3$Scenario <- 'RCP 8.5'
R8s3$run <- 'R83'

df <- rbind(GLENSs1, GLENSs2, GLENSs3, R8s1, R8s2, R8s3)
df <- df[,-1]
df <- melt(df, id=c('years','Scenario','run'))
names(df)[names(df)=='variable'] <- 'region'
names(df)[names(df)=='value'] <- 'PAR'
levels(df$region) <- c('East Africa', 'Central Africa', 'Southern Africa', 'West Africa')

df$region = factor(df$region, levels(df$region)[c(1,2,4,3)])

#ggthemr('grass') # use fresh for actual paper figure 

df$PAR <- df$PAR / 1000000

df -> falciparum.glens.stable

########################

GLENSs1 <- rbind(read.csv('Pf_unstable_GL_1.csv'),read.csv('Pf_unstable_GL_1_2020.csv'))
GLENSs2 <- rbind(read.csv('Pf_unstable_GL_2.csv'),read.csv('Pf_unstable_GL_2_2020.csv'))
GLENSs3 <- rbind(read.csv('Pf_unstable_GL_3.csv'),read.csv('Pf_unstable_GL_3_2020.csv'))
R8s1 <- rbind(read.csv('Pf_unstable_R8_1.csv'),read.csv('Pf_unstable_R8_1_2020.csv'))
R8s2 <- rbind(read.csv('Pf_unstable_R8_2.csv'),read.csv('Pf_unstable_R8_2_2020.csv'))
R8s3 <- rbind(read.csv('Pf_unstable_R8_3.csv'),read.csv('Pf_unstable_R8_3_2020.csv'))

GLENSs1$Scenario <- 'GLENS'
GLENSs1$run <- 'GLENS1'

GLENSs2$Scenario <- 'GLENS'
GLENSs2$run <- 'GLENS2'

GLENSs3$Scenario <- 'GLENS'
GLENSs3$run <- 'GLENS3'

R8s1$Scenario <- 'RCP 8.5'
R8s1$run <- 'R81'

R8s2$Scenario <- 'RCP 8.5'
R8s2$run <- 'R82'

R8s3$Scenario <- 'RCP 8.5'
R8s3$run <- 'R83'

df <- rbind(GLENSs1, GLENSs2, GLENSs3, R8s1, R8s2, R8s3)
df <- df[,-1]
df <- melt(df, id=c('years','Scenario','run'))
names(df)[names(df)=='variable'] <- 'region'
names(df)[names(df)=='value'] <- 'PAR'
levels(df$region) <- c('East Africa', 'Central Africa', 'Southern Africa', 'West Africa')

df$region = factor(df$region, levels(df$region)[c(1,2,4,3)])

#ggthemr('grass') # use fresh for actual paper figure 

df$PAR <- df$PAR / 1000000

df -> falciparum.glens.unstable

####################

G3s1 <- read.csv('Pf_stable_G3_1.csv')
G3s2 <- read.csv('Pf_stable_G3_2.csv')
G3s3 <- read.csv('Pf_stable_G3_3.csv')
R4s1 <- read.csv('Pf_stable_R4_1.csv')
R4s2 <- read.csv('Pf_stable_R4_2.csv')
R4s3 <- read.csv('Pf_stable_R4_3.csv')

G3s1$Scenario <- 'G3'
G3s1$run <- 'G31'

G3s2$Scenario <- 'G3'
G3s2$run <- 'G32'

G3s3$Scenario <- 'G3'
G3s3$run <- 'G33'

R4s1$Scenario <- 'RCP 4.5'
R4s1$run <- 'R41'

R4s2$Scenario <- 'RCP 4.5'
R4s2$run <- 'R42'

R4s3$Scenario <- 'RCP 4.5'
R4s3$run <- 'R43'

df <- rbind(G3s1, G3s2, G3s3, R4s1, R4s2, R4s3)
df <- df[,-1]
df <- melt(df, id=c('years','Scenario','run'))
names(df)[names(df)=='variable'] <- 'region'
names(df)[names(df)=='value'] <- 'PAR'
levels(df$region) <- c('East Africa', 'Central Africa', 'Southern Africa', 'West Africa')

df$region = factor(df$region, levels(df$region)[c(1,2,4,3)])
df$PAR <- df$PAR / 1000000

df -> falciparum.g3.stable

######################

G3u1 <- read.csv('Pf_unstable_G3_1.csv')
G3u2 <- read.csv('Pf_unstable_G3_2.csv')
G3u3 <- read.csv('Pf_unstable_G3_3.csv')
R4u1 <- read.csv('Pf_unstable_R4_1.csv')
R4u2 <- read.csv('Pf_unstable_R4_2.csv')
R4u3 <- read.csv('Pf_unstable_R4_3.csv')

G3u1$Scenario <- 'G3'
G3u1$run <- 'G31'

G3u2$Scenario <- 'G3'
G3u2$run <- 'G32'

G3u3$Scenario <- 'G3'
G3u3$run <- 'G33'

R4u1$Scenario <- 'RCP 4.5'
R4u1$run <- 'R41'

R4u2$Scenario <- 'RCP 4.5'
R4u2$run <- 'R42'

R4u3$Scenario <- 'RCP 4.5'
R4u3$run <- 'R43'

df2 <- rbind(G3u1, G3u2, G3u3, R4u1, R4u2, R4u3)
df2 <- df2[,-1]
df2 <- melt(df2, id=c('years','Scenario','run'))
names(df2)[names(df2)=='variable'] <- 'region'
names(df2)[names(df2)=='value'] <- 'PAR'
levels(df2$region) <- c('East Africa', 'Central Africa', 'Southern Africa', 'West Africa')

df2$region = factor(df2$region, levels(df2$region)[c(1,2,4,3)])
df2$PAR <- df2$PAR / 1000000

df2 -> falciparum.g3.unstable

###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

head(vivax.g3.stable)

vivax.g3.stable$malaria <- 'vivax'
vivax.g3.unstable$malaria <- 'vivax'
vivax.glens.stable$malaria <- 'vivax'
vivax.glens.unstable$malaria <- 'vivax'
falciparum.g3.stable$malaria <- 'falciparum'
falciparum.g3.unstable$malaria <- 'falciparum'
falciparum.glens.stable$malaria <- 'falciparum'
falciparum.glens.unstable$malaria <- 'falciparum'

vivax.g3.stable$risk <- 'stable'
vivax.glens.stable$risk <- 'stable'
falciparum.g3.stable$risk <- 'stable'
falciparum.glens.stable$risk <- 'stable'
vivax.g3.unstable$risk <- 'unstable'
vivax.glens.unstable$risk <- 'unstable'
falciparum.g3.unstable$risk <- 'unstable'
falciparum.glens.unstable$risk <- 'unstable'

rbind(vivax.g3.stable,
      vivax.g3.unstable,
      vivax.glens.stable,
      vivax.glens.unstable,
      falciparum.g3.stable,
      falciparum.g3.unstable,
      falciparum.glens.stable,
      falciparum.glens.unstable) %>% as_tibble -> all.malaria

all.malaria
unique(all.malaria$Scenario)

level_key <- c(G3 = 'RCP 4.5',
               `RCP 4.5`='RCP 4.5',
               GLENS = 'RCP 8.5',
               `RCP 8.5` = 'RCP 8.5')

level_key2 <- c(G3 = 'SRM',
               `RCP 4.5`='No SRM',
               GLENS = 'SRM',
               `RCP 8.5` = 'No SRM')

library(extrafont)
#font_import() # import all your fonts
#fonts() #get a list of fonts
loadfonts(device="win")


library(magrittr)
all.malaria %<>% filter(years <= 2070)

### VERSION 1
# 
# all.malaria %>% 
#   filter(risk=='stable') %>%
#   group_by(Scenario, years, run) %>%
#   summarize(PAR = sum(PAR)/1000) %>%
#   ungroup() %>%
#   group_by(Scenario, years) %>%
#   summarize(PAR = mean(PAR)) %>%
#  # filter(years > 2020) %>%
#   mutate(Baseline = recode(Scenario, !!!level_key),
#          SRM = recode(Scenario, !!!level_key2)) %>% 
#   ggplot() + 
#   facet_wrap(~ Baseline) + theme_few() + xlab(NULL) + 
#   theme(text = element_text(family = 'Roboto'),
#         axis.text.x = element_text(hjust = 1, vjust = 1, angle=45, size = 10),
#         axis.title.y = element_text(vjust = 5),
#         legend.text = element_text(size = 10),
#         strip.text = element_text(size = 13),
#         plot.margin = margin(0.75, 0.75, 0.75, 0.75, "cm"),
#         legend.position = c(0.11,0.88),
#         legend.background = element_blank(),
#         panel.grid.major.y = element_line(color='light grey')) + 
#   ylab('Populations at highest risk (bil.)') + 
#   labs(color=NULL) + scale_color_few() + 
#   geom_line(aes(x=years, y=PAR, color=SRM), lwd=0.6) + 
#   geom_point(aes(x=years, y=PAR, color=SRM), cex=1.6) 



### VERSION 1B


all.malaria %>% 
  filter(risk=='stable') %>%
  group_by(Scenario, years, run) %>%
  summarize(PAR = sum(PAR)/1000) %>%
  ungroup() %>%
  #group_by(Scenario, years) %>%
  #summarize(PAR = mean(PAR/1000)) %>%
  #filter(years > 2020) %>%
  mutate(Baseline = recode(Scenario, !!!level_key),
         SRM = recode(Scenario, !!!level_key2)) %>% 
  ggplot(aes(group=run)) + 
  facet_wrap(~ Baseline) + theme_few() + xlab(NULL) + 
  theme(text = element_text(family = 'Roboto'),
        axis.text.x = element_text(hjust = 1, vjust = 1, angle=45, size = 10),
        axis.title.y = element_text(vjust = 5),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 13),
        plot.margin = margin(0.75, 0.75, 0.75, 0.75, "cm"),
        legend.position = c(0.11,0.88),
        legend.background = element_blank(),
        panel.grid.major.y = element_line(color='light grey')) + 
  ylab('Populations at highest risk (bil.)') + 
  labs(color=NULL) + scale_color_few() + 
  geom_line(aes(x=years, y=PAR, color=SRM), lwd=0.6) + 
  geom_point(aes(x=years, y=PAR, color=SRM), cex=1.6) 


#########################################################

# ED Fig 1

all.malaria %>% filter(malaria=='falciparum',
                       risk == 'stable',
                       Scenario %in% c('G3','RCP 4.5')) %>%
  ggplot(aes(x=years, y=PAR, group=run, color=Scenario)) + 
  geom_line() + geom_point() + facet_wrap( ~ region) + #, scales='free'
  theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
        panel.grid.major = element_line(colour='lightgrey'),
        panel.grid.minor = element_line(colour='lightgrey')) + 
  ylab(NULL) + xlab(NULL) + 
  legend_bottom() + xlim(2015, 2075) + 
  labs(title = 'Stable risk', subtitle = '(6+ months)') -> g1

all.malaria %>% filter(malaria=='falciparum',
                       risk == 'unstable',
                       Scenario %in% c('G3','RCP 4.5')) %>%
  ggplot(aes(x=years, y=PAR, group=run, color=Scenario)) + 
  geom_line() + geom_point() + facet_wrap( ~ region) + #, scales='free'
  theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
        panel.grid.major = element_line(colour='lightgrey'),
        panel.grid.minor = element_line(colour='lightgrey')) + 
  ylab(NULL) + xlab(NULL) + 
  legend_bottom() + xlim(2015, 2075) + 
  labs(title = 'Unstable risk', subtitle = '(1-5 months)') -> g2

g1 + g2
  
# ED Fig 2

all.malaria %>% filter(malaria=='falciparum',
                       risk == 'stable',
                       Scenario %in% c('GLENS','RCP 8.5')) %>%
  ggplot(aes(x=years, y=PAR, group=run, color=Scenario)) + 
  geom_line() + geom_point() + facet_wrap( ~ region) + #, scales='free'
  theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
        panel.grid.major = element_line(colour='lightgrey'),
        panel.grid.minor = element_line(colour='lightgrey')) + 
  ylab(NULL) + xlab(NULL) + 
  legend_bottom() + xlim(2015, 2075) + 
  labs(title = 'Stable risk', subtitle = '(6+ months)') -> g1

all.malaria %>% filter(malaria=='falciparum',
                       risk == 'unstable',
                       Scenario %in% c('GLENS','RCP 8.5')) %>%
  ggplot(aes(x=years, y=PAR, group=run, color=Scenario)) + 
  geom_line() + geom_point() + facet_wrap( ~ region) + #, scales='free'
  theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
        panel.grid.major = element_line(colour='lightgrey'),
        panel.grid.minor = element_line(colour='lightgrey')) + 
  ylab(NULL) + xlab(NULL) + 
  legend_bottom() + xlim(2015, 2075) + 
  labs(title = 'Unstable risk', subtitle = '(1-5 months)') -> g2

g1 + g2

# ED Fig 3

all.malaria %>% filter(malaria=='vivax',
                       risk == 'stable',
                       Scenario %in% c('G3','RCP 4.5')) %>%
  ggplot(aes(x=years, y=PAR, group=run, color=Scenario)) + 
  geom_line() + geom_point() + facet_wrap( ~ region, nrow = 3) + #, scales='free'
  theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
        panel.grid.major = element_line(colour='lightgrey'),
        panel.grid.minor = element_line(colour='lightgrey')) + 
  ylab(NULL) + xlab(NULL) + 
  legend_bottom() + xlim(2015, 2075) + 
  labs(title = 'Stable risk', subtitle = '(6+ months)') -> g1

all.malaria %>% filter(malaria=='vivax',
                       risk == 'unstable',
                       Scenario %in% c('G3','RCP 4.5')) %>%
  ggplot(aes(x=years, y=PAR, group=run, color=Scenario)) + 
  geom_line() + geom_point() + facet_wrap( ~ region, nrow = 3) + #, scales='free'
  theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
        panel.grid.major = element_line(colour='lightgrey'),
        panel.grid.minor = element_line(colour='lightgrey')) + 
  ylab(NULL) + xlab(NULL) + 
  legend_bottom() + xlim(2015, 2075) + 
  labs(title = 'Unstable risk', subtitle = '(1-5 months)') -> g2

g1 + g2

# ED Fig 4

all.malaria %>% filter(malaria=='vivax',
                       risk == 'stable',
                       Scenario %in% c('GLENS','RCP 8.5')) %>%
  ggplot(aes(x=years, y=PAR, group=run, color=Scenario)) + 
  geom_line() + geom_point() + facet_wrap( ~ region, nrow = 3) + #, scales='free'
  theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
        panel.grid.major = element_line(colour='lightgrey'),
        panel.grid.minor = element_line(colour='lightgrey')) + 
  ylab(NULL) + xlab(NULL) + 
  legend_bottom() + xlim(2015, 2075) + 
  labs(title = 'Stable risk', subtitle = '(6+ months)') -> g1

all.malaria %>% filter(malaria=='vivax',
                       risk == 'unstable',
                       Scenario %in% c('GLENS','RCP 8.5')) %>%
  ggplot(aes(x=years, y=PAR, group=run, color=Scenario)) + 
  geom_line() + geom_point() + facet_wrap( ~ region, nrow = 3) + #, scales='free'
  theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
        panel.grid.major = element_line(colour='lightgrey'),
        panel.grid.minor = element_line(colour='lightgrey')) + 
  ylab(NULL) + xlab(NULL) + 
  legend_bottom() + xlim(2015, 2075) + 
  labs(title = 'Unstable risk', subtitle = '(1-5 months)') -> g2

g1 + g2
