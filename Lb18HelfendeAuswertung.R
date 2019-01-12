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

dfh <- read.csv("lb18helfende.csv", header=T, sep = ";")
dfh[,5:24]<- lapply(dfh[,5:24], as.factor)

longdfh2 <- gather(dfh, friday, response, Location_Fr:Publikum_Fr)
longdfh2$response <- as.factor(longdfh2$response)

longdfh3 <- gather(dfh, saturday, response, Musik_Sa:Publikum_Sa)
longdfh3$response <- as.factor(longdfh3$response)

# Geschlecht
ggplot(dfh, aes(x=Gender, fill = Gender, color = Gender)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "weiblich", "2" = "männlich", "3" = "keine Angabe")) +
  scale_y_continuous(breaks = seq(0,10,by = 2))+
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Geschlecht") 
ggsave("gender_helfende.png")

# Workload
levels(dfh$Arbeit) <- c(levels(dfh$Arbeit), "1")
dfh$Arbeit <- factor(dfh$Arbeit, levels = c("1", "2", "3", "4"))
ggplot(dfh, aes(x=Arbeit, fill = Arbeit, color = Arbeit)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "1 (unterfordert)", "2" = "2", "3" = "3", "4" = "4 (überlastet)")) +
  scale_y_continuous(breaks = seq(0,10,by = 2))+
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Wie war der Workload?") 
ggsave("workload.png")

# Organisation
my.labels <- c("1 (Ich war \n total verloren)", "2", "3", "4 (Ich wusste \n immer, wo ich sein muss)")

levels(dfh$Orga) <- c(levels(dfh$Orga), "1", "2")
dfh$Orga <- factor(dfh$Orga, levels = c("1", "2", "3", "4"))
ggplot(dfh, aes(x=Orga, fill = Orga, color = Orga)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=my.labels) +
  scale_y_continuous(breaks = seq(0,12,by = 2))+
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Wie war der Workload?") 
ggsave("orga.png")

# LaBot
levels(dfh$LaBot) <- c(levels(dfh$LaBot), "1", "2")
dfh$LaBot <- factor(dfh$LaBot, levels = c("1", "2", "3", "4"))
ggplot(dfh, aes(x=LaBot, fill = LaBot, color = LaBot)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "1 (gar nicht)", "2" = "2", "3" = "3", "4" = "4 (super hilfreich)")) +
  scale_y_continuous(breaks = seq(0,16,by = 2))+
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#FFC652"))+
  labs(y="Anzahl", x="") +
  ggtitle("Wie hilfreich war der LaBot?") 
ggsave("labot.png")

# Party Freitag
levels(longdfh2$response) <- c(levels(longdfh2$response), "1", "2")
longdfh2$response <- factor(longdfh2$response, levels = c("1", "2", "3", "4", "5"))
ggplot(longdfh2, aes(x=friday, fill = response, color = response)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", width = 0.8, position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F)+
  scale_y_continuous(breaks = seq(0,10,by = 2))+
  scale_color_manual(values = c("#AC1B3E","#C75445", "#808080"),
                     labels=c("3", "4 (hervorragend)", "War nicht da"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#808080"),
                    labels=c("3", "4 (hervorragend)", "War nicht da"))+
  labs(y="Anzahl", x="") +
  ggtitle("Party Freitag (Helfende)")
ggsave("party_fr_helfende.png")

# Party Samstag
levels(longdfh3$response) <- c(levels(longdfh3$response), "1")
longdfh3$response <- factor(longdfh3$response, levels = c("1", "2", "3", "4", "5"))
ggplot(longdfh3, aes(x=saturday, fill = response, color = response)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", width = 0.8, position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F)+
  scale_y_continuous(breaks = seq(0,10,by = 2))+
  scale_color_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#808080"),
                     labels=c("2","3", "4 (hervorragend)", "War nicht da"))+
  scale_fill_manual(values = c("#AC1B3E","#C75445", "#E38D4B", "#808080"),
                    labels=c("2","3", "4 (hervorragend)", "War nicht da"))+
  labs(y="Anzahl", x="") +
  ggtitle("Party Samstag (Helfende)")
ggsave("party_sa_helfende.png")

# LB 2019
levels(dfh$Work_2019) <- c(levels(dfh$Work_2019), "1")
dfh$Work_2019 <- factor(dfh$Work_2019, levels = c("1", "2", "3", "4"))
ggplot(dfh, aes(x=Work_2019, fill = Work_2019, color = Work_2019)) + 
  theme_classic() + theme(text= element_text(size=22)) + theme(legend.title=element_blank()) +
  geom_bar(stat="count", position="dodge", alpha=0.8) +
  scale_x_discrete(drop = F, 
                   labels=c("1" = "Stärker", "2" = "Wie 2018", "3" = "Weniger", "4" = "Nicht mehr in Gö")) +
  scale_y_continuous(breaks = seq(0,12,by = 2))+
  scale_color_manual(values = c("#AC1B3E", "#E38D4B", "#808080"))+
  scale_fill_manual(values = c("#AC1B3E", "#E38D4B", "#808080"))+
  labs(y="Anzahl", x="") +
  ggtitle("Wie sehr willst du dich 2019 einbringen?") 
ggsave("lb19.png")

