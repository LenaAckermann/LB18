##############################
### Auswertung LB 18 Teams ###
##############################

# Lena Ackermann
# 12.01.2019
# Survey created with formr

library(ggplot2)
library(tidyr)

# set wd
# setwd("C:/Users/lackerm2/Desktop/LB18")

df <- read.csv("lb18teams.csv", header=T, sep = ";")
df <- na.omit(df)
df[,5:25]<- lapply(df[,5:15], as.factor)

longdf <- gather(df, food, response, Qualitaet_Essen:Preis_Essen)
longdf$response <- as.factor(longdf$response)

longdf2 <- gather(df, friday, response, Musik_Fr:Publikum_Fr)
longdf2$response <- as.factor(longdf2$response)

longdf3 <- gather(df, saturday, response, Musik_Sa:Publikum_Sa)
longdf3$response <- as.factor(longdf3$response)

# Geschlecht
ggplot(df, aes(x=Geschlecht, fill = Geschlecht, color = Geschlecht)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "weiblich", "2" = "männlich")) +
  scale_color_manual(values = c("#AC1B3E","#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#FFC652"))+
  labs(y="Anzahl", x="Geschlecht") +
  ggtitle("Geschlecht") 
ggsave("gender.png")

# Turnier gesamt
levels(df$Insgesamt) <- c(levels(df$Insgesamt), "1", "2")
df$Insgesamt <- factor(df$Insgesamt, levels = c("1", "2", "3", "4"))
ggplot(df, aes(x=Insgesamt, fill = Insgesamt, color = Insgesamt)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "1 (schlecht)", "2" = "2", "3" = "3", "4" = "4 (hervorragend)")) +
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Wie hat euch das Turnier insgesamt gefallen?") 
ggsave("turnier_gesamt.png")


# Teamfee
levels(df$Teamfee_d) <- c(levels(df$Teamfee_d), "1")
df$Teamfee_d <- factor(df$Teamfee_d, levels = c("1", "2", "3"))
ggplot(df, aes(x=Teamfee_d, fill = Teamfee_d, color = Teamfee_d)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "zu niedrig", "2" = "genau richtig", "3" = "zu hoch")) +
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Wie fandet ihr die Meldegebühr?") 
ggsave("teamfee.png")


# Anzahl Teams
ggplot(df, aes(x=Anzahl_Teams, fill = Anzahl_Teams, color = Anzahl_Teams)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "zu wenige", "2" = "genau richtig", "3" = "zu viele")) +
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Wie fandet ihr die Anzahl der Teams?") 
ggsave("anzahl_teams.png")


# Spielplan
ggplot(df, aes(x=Spielplan, fill = Spielplan, color = Spielplan)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "1 (schlecht)", "2" = "2", "3" = "3", "4" = "4 (hervorragend)")) +
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Der digitale Spielplan war...") 
ggsave("spielplan.png")


# Zeit pro Spiel
levels(df$Zeit_Spiel) <- c(levels(df$Zeit_Spiel), "3")
df$Zeit_Spiel <- factor(df$Zeit_Spiel, levels = c("1", "2", "3"))
ggplot(df, aes(x=Zeit_Spiel, fill = Zeit_Spiel, color = Zeit_Spiel)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "zu kurz", "2" = "genau richtig", "3" = "zu lang")) +
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Wie fandet ihr die Zeit pro Spiel?") 
ggsave("zeit_spiel.png")


# Anzahl Spiele
levels(df$Anzahl_Spiel) <- c(levels(df$Anzahl_Spiel), "3")
df$Anzahl_Spiel <- factor(df$Anzahl_Spiel, levels = c("1", "2", "3"))
ggplot(df, aes(x=Anzahl_Spiel, fill = Anzahl_Spiel, color = Anzahl_Spiel)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "zu wenige", "2" = "genau richtig", "3" = "zu viele")) +
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Wie fandet ihr die Anzahl der Spiele?") 
ggsave("anzahl_spiele.png")


# Essen
levels(longdf$response) <- c(levels(longdf$response), "1", "2")
longdf$response <- factor(longdf$response, levels = c("1", "2", "3", "4"))

#ggplot(longdf, aes(x=response, fill = food, color = food)) + 
 # theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  #geom_bar(stat="count", position=position_dodge(width=0.9), alpha=0.8) +
  #scale_x_discrete(drop = F, 
   #                labels=c("1" = "1 (schlecht)", "2" = "2", "3" = "3", "4" = "4 (hervorragend)")) +
  #scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  #scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  #labs(y="Anzahl", x="") +
  #ggtitle("Essen") 

ggplot(longdf, aes(x=food, fill = response, color = response)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Essen") 
ggsave("essen.png")



# Party Freitag
ggplot(longdf2, aes(x=friday, fill = response, color = response)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", width = 0.8, position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F)+
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"),
                     labels=c("1 (schlecht)", "2", "3", "4 (hervorragend)"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"),
                    labels=c("1 (schlecht)", "2", "3", "4 (hervorragend)"))+
  labs(y="Anzahl", x="") +
  ggtitle("Party Freitag")
ggsave("party_fr.png")



# Party Samstag
ggplot(longdf3, aes(x=saturday, fill = response, color = response)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", width = 0.8, position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F)+
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"),
                     labels=c("1 (schlecht)", "2", "3", "4 (hervorragend)"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"),
                    labels=c("1 (schlecht)", "2", "3", "4 (hervorragend)"))+
  labs(y="Anzahl", x="") +
  ggtitle("Party Samstag") 
ggsave("party_sa.png")


# LB 2019?
levels(df$LB19d) <- c(levels(df$LB19d), "3")
df$LB19d <- factor(df$LB19d, levels = c("1", "2", "3"))
ggplot(df, aes(x=LB19d, fill = LB19d, color = LB19d)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "Auf jeden Fall", "2" = "Mal sehen", "3" = "Nein Danke")) +
  scale_y_continuous(breaks = seq(0,9,by = 2))+
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Seid ihr 2019 wieder am Start?") 
ggsave("lb2019.png")


