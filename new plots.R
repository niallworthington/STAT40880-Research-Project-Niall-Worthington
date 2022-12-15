for (i in 1:1){
  fit = cpt.mean(na.omit(EPLPoints[,i]))
  cptsEPL[i] = cpts(fit)
  ints = param.est(fit)$mean
  emptyEPL[i] = ints[1]-ints[2]
  MagnitudeINTSEPL[i] = emptyEPL[i]
  
  
  
  a = ggplot(data = data.frame(seq(1:length(na.omit(EPLPoints[,i]))),na.omit(EPLPoints[,i])),
             aes(x =seq(1:length(na.omit(EPLPoints[,i]))),y = na.omit(EPLPoints[,i])))+ 
    geom_line() + 
    
    ylim(20,102)+
    xlim(1,22)+
    geom_segment(x = 1, xend=length(EPLPoints[1:cptsEPL[i],1]) ,y =ints[1] ,yend = ints[1],color='red')+
    geom_segment(x = length(EPLPoints[1:cptsEPL[i],1]), xend=22 ,y =ints[2] ,yend = ints[2],color='red')+
    geom_vline(xintercept = cptsEPL[i],linetype='dashed',size = 1,color='blue')+
    ggtitle(dates[i],'English Premier League')+  
    labs(x='League Position',y = 'Points Total')
  #ggsave(a,filename=paste("PremierLeagueSeason",i,".png",sep=""))
  
  print(ggplot(data = data.frame(seq(1:length(na.omit(EPLPoints[,i]))),na.omit(EPLPoints[,i])),
               aes(x =seq(1:length(na.omit(EPLPoints[,i]))),y = na.omit(EPLPoints[,i])))+ 
          geom_line() + 
          
          ylim(20,102)+
          xlim(1,22)+
          geom_segment(x = 1, xend=length(EPLPoints[1:cptsEPL[i],1]) ,y =ints[1] ,yend = ints[1],color='red')+
          geom_segment(x = length(EPLPoints[1:cptsEPL[i],1]), xend=22 ,y =ints[2] ,yend = ints[2],color='red')+
          geom_vline(xintercept = cptsEPL[i],linetype='dashed',size = 1,color='blue')+
          ggtitle(dates[i],'English Premier League')+  
          labs(x='League Position',y = 'Points Total'))
}


EPLMAGPLOT2= data.frame(MAG = c(MagnitudeINTSEPL),dates =c(dates),numeric = c(numeric))
EPLMAGPLOT2 = EPLMAGPLOT2[-c(39,40),]


ggplot(data = EPLMAGPLOT2,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Points Difference between Cluster Means as Time Progresses - English Premier League
          COVID 19 Seasons Removed')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,39),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","21/22"))



















EPLCPPLOT= data.frame(MAG = c(cptsEPL),dates =c(dates),numeric = c(numeric))
  
ggplot(data = EPLCPPLOT,aes(x = numeric,y = MAG))+
      geom_point()+
      ggtitle('Number of Teams in the Top Cluster as Time Progresses - English Premier League')+  
      labs(x='Year',y = 'Difference')+
      stat_summary(fun.data= mean_cl_normal) + 
      scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                         labels=c("85/86", "90/91","95/96","00/01","05/06",
                                  "10/11","15/16","20/21"))+
      scale_y_continuous(breaks =c(2,4,6,8,10,12,14))




#remove covid season

EPLCPPLOT2= data.frame(MAG = c(cptsEPL),dates =c(dates),numeric = c(numeric))
EPLCPPLOT2 = EPLCPPLOT2[-c(39,40),]



ggplot(data = EPLCPPLOT2,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Number of Teams in the Top Cluster as Time Progresses - English Premier League
          (COVID-19 Affected Seasons Removed)')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,41),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","21/22"))















EPLHHI2 = EPLHHI
EPLHHI2 = c(EPLHHI[1:38],EPLHHI[41])
EPLPLOT2 = data.frame(EPLHHI2,numericEPLHHI)

ggplot(EPLPLOT2,aes(x=numericEPLHHI,y=EPLHHI2))+geom_point(size=3)+
  ggtitle("Herfindahl-Hirschman Index Per Season - English Premier League
          Covid 19 Seasons Removed") +
  labs(x="Season", y="HHI")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,39),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","20/21")) +
  ylim(1.02,1.16)+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')











EPLCPPLOT3= data.frame(MAG = c(cptsEPL),dates =c(dates),numeric = c(numeric))
EPLCPPLOT3 = EPLCPPLOT3[1:20,]

ggplot(data = EPLCPPLOT3,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Number of Teams in the Top Cluster as Time Progresses - English Premier League
          1982-2000')+  
  labs(x='Year',y = 'Number of Teams in Top Cluster')+

  ylim(3,15)+
  scale_x_continuous(breaks=c(5,10,15,20),
                     labels=c("85/86", "90/91","95/96","00/01"))+
  geom_segment(x = 1, xend=20 ,y =6 ,yend = 6,color='red')



EPLCPPLOT4= data.frame(MAG = c(cptsEPL),dates =c(dates),numeric = c(numeric))
EPLCPPLOT4 = EPLCPPLOT4[21:41,]

ggplot(data = EPLCPPLOT4,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Number of Teams in the Top Cluster as Time Progresses - English Premier League
          2001-2022')+  
  labs(x='Year',y = 'Number of Teams in Top Cluster')+
  ylim(3,15)+
  scale_x_continuous(breaks=c(25,30,35,40),
                     labels=c("05/06",
                              "10/11","15/16","20/21"))+
  geom_segment(x = 21, xend=41 ,y =6 ,yend = 6,color='red')











scipen=2

teams = c('Manchester United','Liverpool','Arsenal','Manchester City','Chelsea','Tottenham',
          'Manchester United','Liverpool','Arsenal','Manchester City','Chelsea','Tottenham')
prop2000 = c(17/19,16/19,16/19,2/19,7/19,10/19,21/22,19/22,22/22,14/22,21/22,14/22)

prop2000 = round(prop2000,digits=2)


era = c(1,1,1,1,1,1,2,2,2,2,2,2)

  
cluster = data.frame(teams,prop2000,era,round(prop2000,digits = 2))
  
  
  
  
  
  
  ggplot(cluster, aes(x=teams, y=prop2000, fill=era)) + 
    geom_bar(stat="identity", width=0.7,position=position_dodge(width=1))

  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
scipen =2
  
  
ggplot(data=cluster[1:6,],aes(x=teams[1:6],y=prop2000[1:6]))+
  geom_bar(stat='identity',fill=c('red1','red4','firebrick1','lightblue','blue','white'))+
  ylim(0,1)+
  labs(x = "Propotion of Time in Top Cluser Pre 2000", y = "Frequency\n", title = "English Premier League\n\n 1981-2000 \n") +
  geom_text(aes(label = prop2000[1:6]), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10),
        legend.title = element_text(face="bold", size = 10))




prop2000[7:12]
teams[7:12]
cluster[7:12,]

cluster2  = cluster[7:12,]

teams2 = teams[7:12]
prop20002 = prop2000[7:12]


ggplot(data=cluster2,aes(x=teams2,y=prop20002))+
  geom_bar(stat='identity',fill=c('red1','red4','firebrick1','lightblue','blue','white'))+
  ylim(0,1)+
  labs(x = "Propotion of Time in Top Cluser Post 2000", y = "Frequency\n", title = "\n English Premier League\n2000-2022 \n") +
  geom_text(aes(label = prop20002), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10),
        legend.title = element_text(face="bold", size = 10))
