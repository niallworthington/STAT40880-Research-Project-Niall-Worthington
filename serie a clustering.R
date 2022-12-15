
scipen=2

teamsISA = c('Juventus','Inter Milan', 'AC Milan','Napoli','Lazio','Roma','Juventus','Inter Milan', 'AC Milan','Napoli','Lazio','Roma')
prop2000ISA = c(12/14,12/14,11/14,1/14,11/14,12/14,13/14,12/14,12/14,13/14,10/14,14/14)
prop2000ISA = round(prop2000ISA,digits = 3)


eraISA = c(1,1,1,1,1,1,2,2,2,2,2,2)


clusterISA = data.frame(teamsISA,prop2000ISA,eraISA,round(prop2000ISA,digits = 2))


cluster2ISA  = clusterISA[7:12,]

teams2ISA = teamsISA[7:12]
prop20002ISA = prop2000ISA[7:12]


ggplot(data=cluster2ISA,aes(x=teams2ISA,y=prop20002ISA))+
  geom_bar(stat='identity',fill=c('white','darkblue','red','blue','lightblue','orange'))+
  ylim(0,1)+
  labs(x = "Propotion of Time in Top Cluser Post 2008", y = "Frequency\n", title = "\n Italian Serie A\n2008-2022 \n") +
  geom_text(aes(label = prop20002ISA), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10),
        legend.title = element_text(face="bold", size = 10))





cluster3ISA  = clusterISA[1:6,]

teams3ISA = teamsISA[1:6]
prop20003ISA = prop2000ISA[1:6]


ggplot(data=cluster3ISA,aes(x=teams3ISA,y=prop20003ISA))+
  geom_bar(stat='identity',fill=c('white','darkblue','red','blue','lightblue','orange'))+
  ylim(0,1)+
  labs(x = "Propotion of Time in Top Cluser Pre 2008", y = "Frequency\n", title = "\n Italian Serie A\n1995-2008 \n") +
  geom_text(aes(label = prop20003ISA), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10),
        legend.title = element_text(face="bold", size = 10))

