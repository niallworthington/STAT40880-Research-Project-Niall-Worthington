
scipen=2

teamsGB = c('Bayern Munich','Borussia Dortmund','Bayern Leverkusen','Bayern Munich','Borussia Dortmund','Bayern Leverkusen')
prop2000GB = c(13/13,8/13,9/13,14/14,13/14,12/14)
prop2000GB = round(prop2000GB,digits = 3)


eraGB = c(1,1,1,2,2,2)


clusterGB = data.frame(teamsGB,prop2000GB,eraGB,round(prop2000GB,digits = 2))


cluster2GB  = clusterGB[4:6,]

teams2GB = teamsGB[4:6]
prop20002GB = prop2000GB[4:6]


ggplot(data=cluster2GB,aes(x=teams2GB,y=prop20002GB))+
  geom_bar(stat='identity',fill=c('red','yellow','red4'))+
  ylim(0,1)+
  labs(x = "Propotion of Time in Top Cluser Post 2008", y = "Frequency\n", title = "\n German Bundesliga \n2008-2022 \n") +
  geom_text(aes(label = prop20002GB), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10),
        legend.title = element_text(face="bold", size = 10))





cluster3GB  = clusterGB[1:3,]

teams3GB = teamsGB[1:3]
prop20003GB = prop2000GB[1:3]


ggplot(data=cluster3GB,aes(x=teams3GB,y=prop20003GB))+
  geom_bar(stat='identity',fill=c('red','yellow','red4'))+
  ylim(0,1)+
  labs(x = "Propotion of Time in Top Cluser Pre 2008", y = "Frequency\n", title = "\n German Bundesliga \n1995-2008 \n") +
  geom_text(aes(label = prop20003GB), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10),
        legend.title = element_text(face="bold", size = 10))


length(LIGUE1Points)
