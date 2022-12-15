
scipen=2

teamsFL1 = c('Paris Saint-Germain','Marseille','Olympique Lyon','LOSC Lille','Paris Saint-Germain','Marseille','Olympique Lyon','LOSC Lille')
prop2000FL1 = c(9/14,7/14,13/14,5/14,12/13,9/13,13/13,10/13)
prop2000FL1 = round(prop2000FL1,digits = 3)


eraFL1 = c(1,1,1,1,2,2,2,2)


clusterFL1 = data.frame(teamsFL1,prop2000FL1,eraFL1,round(prop2000FL1,digits = 2))


cluster2FL1  = clusterFL1[5:8,]

teams2FL1 = teamsFL1[5:8]
prop20002FL1 = prop2000FL1[5:8]


ggplot(data=cluster2FL1,aes(x=teams2FL1,y=prop20002FL1))+
  geom_bar(stat='identity',fill=c('darkblue','lightblue','white','red'))+
  ylim(0,1)+
  labs(x = "Propotion of Time in Top Cluser Post 2008", y = "Frequency\n", title = "\n French Ligue 1\n2008-2022 \n") +
  geom_text(aes(label = prop20002FL1), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10),
        legend.title = element_text(face="bold", size = 10))





cluster3FL1  = clusterFL1[1:4,]

teams3FL1 = teamsFL1[1:4]
prop20003FL1 = prop2000FL1[1:4]


ggplot(data=cluster3FL1,aes(x=teams3FL1,y=prop20003FL1))+
  geom_bar(stat='identity',fill=c('darkblue','lightblue','white','red'))+
  ylim(0,1)+
  labs(x = "Propotion of Time in Top Cluser Pre 2008", y = "Frequency\n", title = "\n French Ligue 1\n1994-2008 \n") +
  geom_text(aes(label = prop20003FL1), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10),
        legend.title = element_text(face="bold", size = 10))

