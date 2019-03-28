






## Sweet 16 Blog post visuals


###############################
#####   Load  libraries   ##### 
###############################
library(ggjoy)
library(data.table)
library(tidyverse)
library(magrittr)
library(d3heatmap)
library(gplots)





###############################
#####   Load  Datafiles   ##### 
###############################

# game by game data
sweet16 <- fread("Sweet16.csv")

# Ken Pom data
kenpom <- fread('kenpom16.csv')




###############################
#####   Ken Pom Ranking   ##### 
###############################


# change to dataframe and then matrix to rename rows
kenpomMatrix <- kenpom %>% data.frame()
row.names(kenpomMatrix) <- kenpomMatrix$Team

# get rankings for key metrics
kenpomRank <- kenpomMatrix %>% select(AdjORank, AdjDRank, AdjTRank, LuckRank, SoSAdjEM) %>%
  rename(Offense = AdjORank,
         Defense = AdjDRank,
         Pace = AdjTRank,
         Luck = LuckRank,
         SoS = SoSAdjEM)

# interactive heatmap with 3 clusters
d3heatmap(kenpomRank, scale = "column", dendrogram = "row", k_row = 3, colors = "Blues")





##############################
#####   Season Metrics   ##### 
##############################


# Compare distribution of points per possession
ggplot(sweet16, aes(x = PPP, y = TeamName, fill = TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values = rep(c('gray', 'lightblue'), length(sweet16$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Points per Possession (PPP)", y = "")


# Net ratings
ggplot(sweet16, aes(x = Net_rating, y = TeamName, fill=TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values=rep(c('gray', 'lightblue'), length(sweet16$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Net Rating (Offensive Rating - Defensive Rating)" ,y="")


# Free Throw Frequency
ggplot(sweet16, aes(x = FTFreq, y = TeamName, fill=TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values=rep(c('gray', 'lightblue'), length(sweet16$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Free Throw Frequency (Free throw attempts per possession)" ,y="")


# Assist Ratio
ggplot(sweet16, aes(x = AstRatio, y = TeamName, fill=TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values=rep(c('gray', 'lightblue'), length(sweet16$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Assist Ratio (Assists per made fielg goal)" ,y="")


# Pace
ggplot(sweet16, aes(x = Pace, y = TeamName, fill=TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values=rep(c('gray', 'lightblue'), length(sweet16$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Pace" ,y="")

# Turnovers
ggplot(sweet16, aes(x = TO, y = TeamName, fill=TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values=rep(c('gray', 'lightblue'), length(sweet16$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Turnovers" ,y="")


# Turnovers
ggplot(sweet16, aes(x = FG3Freq, y = TeamName, fill=TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values=rep(c('gray', 'lightblue'), length(sweet16$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Three Point Frequency (3FGA/FGA)" ,y="")







#################################
#####   Sweet 16 Matchups   ##### 
#################################





#######################################
#####   Michigan vs. Texas Tech   ##### 
#######################################

# Michigan vs. Texas Tech (Mich -1.5)
mich_color <- "#FFCB05"
tech_color <- "#CC0000"
Mich.TT <- sweets %>% filter(TeamName %in% c("Michigan", "Texas Tech")) %>%
  select(TeamName, Net_rating, Off_rating, Def_rating, Pace, TrueShot, PPP, FG3Freq, FTFreq, AstRatio, TO, STLperc, Blkperc, Rebperc, OffRebperc) %>%
  rename(Overall = Net_rating,
         Offense = Off_rating,
         Defense = Def_rating,
         Points = PPP,
         Three.Frequency = FG3Freq,
         FreeThrow.Frequency = FTFreq,
         Assist.Ratio = AstRatio,
         Turnovers = TO,
         Steals = STLperc,
         Blocks = Blkperc,
         Rebounds = Rebperc,
         Offensive.Rebounds = OffRebperc) %>%
  melt(id.vars = "TeamName")


# Compare distribution of metrics for the matchup
ggplot(Mich.TT, aes(x = value, y = variable, fill = TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values = rep(c(mich_color, tech_color), length(Mich.TT$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Michigan vs. Texas Tech (Mich -1.5)", y = "")



######################################
#####    Tennessee vs. Purdue    ##### 
######################################

# Tennessee vs. Purdue (Tenn -1.5)
tenn_color <- "#FF8200"
pur_color <- "#CEB888"
Tenn.Pur <- sweets %>% filter(TeamName %in% c("Tennessee", "Purdue")) %>%
  select(TeamName, Net_rating, Off_rating, Def_rating, Pace, TrueShot, PPP, FG3Freq, FTFreq, AstRatio, TO, STLperc, Blkperc, Rebperc, OffRebperc) %>%
  rename(Overall = Net_rating,
         Offense = Off_rating,
         Defense = Def_rating,
         Points = PPP,
         Three.Frequency = FG3Freq,
         FreeThrow.Frequency = FTFreq,
         Assist.Ratio = AstRatio,
         Turnovers = TO,
         Steals = STLperc,
         Blocks = Blkperc,
         Rebounds = Rebperc,
         Offensive.Rebounds = OffRebperc) %>%
  melt(id.vars = "TeamName")


# Compare distribution of metrics for the matchup
ggplot(Tenn.Pur, aes(x = value, y = variable, fill = TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values = rep(c(pur_color, tenn_color), length(Tenn.Pur$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Tennessee vs. Purdue (Tenn -1.5)", y = "")





######################################
#####     Virgina vs. Oregon     ##### 
######################################

# Virgina vs. Oregon (Virginia -8.5)
vir_color <- "#F84C1E"
org_color <- "#154733"
Vir.Org <- sweets %>% filter(TeamName %in% c("Virginia", "Oregon")) %>%
  select(TeamName, Net_rating, Off_rating, Def_rating, Pace, TrueShot, PPP, FG3Freq, FTFreq, AstRatio, TO, STLperc, Blkperc, Rebperc, OffRebperc) %>%
  rename(Overall = Net_rating,
         Offense = Off_rating,
         Defense = Def_rating,
         Points = PPP,
         Three.Frequency = FG3Freq,
         FreeThrow.Frequency = FTFreq,
         Assist.Ratio = AstRatio,
         Turnovers = TO,
         Steals = STLperc,
         Blocks = Blkperc,
         Rebounds = Rebperc,
         Offensive.Rebounds = OffRebperc) %>%
  melt(id.vars = "TeamName")


# Compare distribution of metrics for the matchup
ggplot(Vir.Org, aes(x = value, y = variable, fill = TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values = rep(c(org_color, vir_color), length(Vir.Org$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Virgina vs. Oregon (Virginia -8.5)", y = "")




######################################
#####   Gonzaga vs. Florida St   ##### 
######################################


# Gonzaga vs. Florida St (Zaga -7.5)
zag_color <- "#041E42"
fsu_color <- "#782F40"
Zag.Fsu <- sweets %>% filter(TeamName %in% c("Gonzaga", "Florida St")) %>%
  select(TeamName, Net_rating, Off_rating, Def_rating, Pace, TrueShot, PPP, FG3Freq, FTFreq, AstRatio, TO, STLperc, Blkperc, Rebperc, OffRebperc) %>%
  rename(Overall = Net_rating,
         Offense = Off_rating,
         Defense = Def_rating,
         Points = PPP,
         Three.Frequency = FG3Freq,
         FreeThrow.Frequency = FTFreq,
         Assist.Ratio = AstRatio,
         Turnovers = TO,
         Steals = STLperc,
         Blocks = Blkperc,
         Rebounds = Rebperc,
         Offensive.Rebounds = OffRebperc) %>%
  melt(id.vars = "TeamName")


# Compare distribution of metrics for the matchup
ggplot(Zag.Fsu, aes(x = value, y = variable, fill = TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values = rep(c(fsu_color, zag_color), length(Zag.Fsu$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Gonzaga vs. Florida St (Zags -7.5)", y = "")







######################################
#####   Duke vs. Virginia Tech   ##### 
######################################

# Duke vs. Virginia Tech (Duke -7)
duke_color <- "#003087"
vt_color <- "#630031"
Duk.VT <- sweets %>% filter(TeamName %in% c("Duke", "Virginia Tech")) %>%
  select(TeamName, Net_rating, Off_rating, Def_rating, Pace, TrueShot, PPP, FG3Freq, FTFreq, AstRatio, TO, STLperc, Blkperc, Rebperc, OffRebperc) %>%
  rename(Overall = Net_rating,
         Offense = Off_rating,
         Defense = Def_rating,
         Points = PPP,
         Three.Frequency = FG3Freq,
         FreeThrow.Frequency = FTFreq,
         Assist.Ratio = AstRatio,
         Turnovers = TO,
         Steals = STLperc,
         Blocks = Blkperc,
         Rebounds = Rebperc,
         Offensive.Rebounds = OffRebperc) %>%
  melt(id.vars = "TeamName")


# Compare distribution of metrics for the matchup
ggplot(Duk.VT, aes(x = value, y = variable, fill = TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values = rep(c(duke_color, vt_color), length(Duk.VT$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Duke vs. Virginia Tech (Duke -7)", y = "")






######################################
#####    Kentucky vs. Houston    ##### 
######################################


# Kentucky vs. Houston (Kentucky -3)
ken_color <- "#0033A0"
hou_color <- "#C8102E"
Ken.Hou <- sweets %>% filter(TeamName %in% c("Kentucky", "Houston")) %>%
  select(TeamName, Net_rating, Off_rating, Def_rating, Pace, TrueShot, PPP, FG3Freq, FTFreq, AstRatio, TO, STLperc, Blkperc, Rebperc, OffRebperc) %>%
  rename(Overall = Net_rating,
         Offense = Off_rating,
         Defense = Def_rating,
         Points = PPP,
         Three.Frequency = FG3Freq,
         FreeThrow.Frequency = FTFreq,
         Assist.Ratio = AstRatio,
         Turnovers = TO,
         Steals = STLperc,
         Blocks = Blkperc,
         Rebounds = Rebperc,
         Offensive.Rebounds = OffRebperc) %>%
  melt(id.vars = "TeamName")


# Compare distribution of metrics for the matchup
ggplot(Ken.Hou, aes(x = value, y = variable, fill = TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values = rep(c(hou_color, ken_color), length(Ken.Hou$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Kentucky vs. Houston (Kentucky -3)", y = "")





#######################################
#####  North Carolina vs. Auburn  ##### 
#######################################


# North Carolina vs. Auburn (North Carolina -5)
car_color <- "#7BAFD4"
aub_color <- "#E87722"
Car.Aub <- sweets %>% filter(TeamName %in% c("North Carolina", "Auburn")) %>%
  select(TeamName, Net_rating, Off_rating, Def_rating, Pace, TrueShot, PPP, FG3Freq, FTFreq, AstRatio, TO, STLperc, Blkperc, Rebperc, OffRebperc) %>%
  rename(Overall = Net_rating,
         Offense = Off_rating,
         Defense = Def_rating,
         Points = PPP,
         Three.Frequency = FG3Freq,
         FreeThrow.Frequency = FTFreq,
         Assist.Ratio = AstRatio,
         Turnovers = TO,
         Steals = STLperc,
         Blocks = Blkperc,
         Rebounds = Rebperc,
         Offensive.Rebounds = OffRebperc) %>%
  melt(id.vars = "TeamName")


# Compare distribution of metrics for the matchup
ggplot(Car.Aub, aes(x = value, y = variable, fill = TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values = rep(c(aub_color, car_color), length(Car.Aub$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="North Carolina vs. Auburn (North Carolina -5)", y = "")






#######################################
#####     Michigan St vs. LSU     ##### 
#######################################

# Michigan St vs. LSU (MSU -6)
msu_color <- "#18453B"
lsu_color <- "#461D7C"
Msu.Lsu <- sweets %>% filter(TeamName %in% c("Michigan St", "LSU")) %>%
  select(TeamName, Net_rating, Off_rating, Def_rating, Pace, TrueShot, PPP, FG3Freq, FTFreq, AstRatio, TO, STLperc, Blkperc, Rebperc, OffRebperc) %>%
  rename(Overall = Net_rating,
         Offense = Off_rating,
         Defense = Def_rating,
         Points = PPP,
         Three.Frequency = FG3Freq,
         FreeThrow.Frequency = FTFreq,
         Assist.Ratio = AstRatio,
         Turnovers = TO,
         Steals = STLperc,
         Blocks = Blkperc,
         Rebounds = Rebperc,
         Offensive.Rebounds = OffRebperc) %>%
  melt(id.vars = "TeamName")


# Compare distribution of metrics for the matchup
ggplot(Msu.Lsu, aes(x = value, y = variable, fill = TeamName)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values = rep(c(lsu_color, msu_color), length(Msu.Lsu$TeamName)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Michigan St vs. LSU (MSU -6)", y = "")

