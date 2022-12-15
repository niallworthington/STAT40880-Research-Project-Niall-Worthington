
scipen=2

teamsSLL = c('Real Madrid','Barcelona','Atletico Madrid','Sevilla','Real Madrid','Barcelona','Atletico Madrid','Sevilla')
prop2000SLL = c(14/14,14/14,4/14,6/14,15/15,15/15,10/15,9/15)
prop2000SLL = round(prop2000SLL,digits = 3)


eraSLL = c(1,1,1,1,2,2,2,2)
prop2000SLL = round(prop2000SLL,digits = 2)

clusterSLL = data.frame(teamsSLL,prop2000SLL,eraSLL,round(prop2000SLL,digits = 3))


cluster2SLL  = clusterSLL[5:8,]

teams2SLL = teamsSLL[5:8]
prop20002SLL = prop2000SLL[5:8]


ggplot(data=cluster2SLL,aes(x=teams2SLL,y=prop20002SLL))+
  geom_bar(stat='identity',fill=c('white','darkblue','red1','firebrick'))+
  ylim(0,1)+
  labs(x = "Propotion of Time in Top Cluser Post 2007", y = "Frequency\n", title = "\n Spanish La Liga \n 2007-2022 \n") +
  geom_text(aes(label = prop20002SLL), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10),
        legend.title = element_text(face="bold", size = 10))





cluster3SLL  = clusterSLL[1:4,]

teams3SLL = teamsSLL[1:4]
prop20003SLL = prop2000SLL[1:4]


ggplot(data=cluster3SLL,aes(x=teams3SLL,y=prop20003SLL))+
  geom_bar(stat='identity',fill=c('white','darkblue','red1','firebrick'))+
  ylim(0,1)+
  labs(x = "Propotion of Time in Top Cluser Pre 2007", y = "Frequency\n", title = "\n Spanish La Liga \n1993-2007 \n") +
  geom_text(aes(label = prop20003SLL), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10),
        legend.title = element_text(face="bold", size = 10))







