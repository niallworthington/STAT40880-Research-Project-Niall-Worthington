SLLCPPLOT= data.frame(MAG = c(cptsSLL),dates =c(dates[13:41]),numeric = c(numeric[1:29]))

ggplot(data = SLLCPPLOT,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Number of Teams in the Top Cluster as Time Progresses - Spannish La Liga')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) +
  scale_x_continuous(breaks=c(5,10,15,20,25,29),
                     labels=c("95/96","00/01","05/06",
                              "10/11","15/16","21/22"))+
  scale_y_continuous(breaks =c(2,4,6,8,10,12,14))

GBCPPLOT= data.frame(MAG = c(cptsGB),dates =c(dates[15:41]),numeric = c(numeric[1:27]))

ggplot(data = GBCPPLOT,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Number of Teams in the Top Cluster as Time Progresses - German Bundesliga')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 
  scale_x_continuous(breaks=c(5,10,15,20,25),
                     labels=c("99/00", "04/05","09/10","14/15","19/20"))+
  scale_y_continuous(breaks =c(2,4,6,8,10,12,14))



ISACPPLOT= data.frame(MAG = c(cptsISA),dates =c(dates[14:41]),numeric = c(numeric[1:28]))

ggplot(data = ISACPPLOT,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Number of Teams in the Top Cluster as Time Progresses- Italian Serie A')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 

  scale_x_continuous(breaks=c(5,10,15,20,25),
                     labels=c("98/99", "03/04","08/09","13/14","18/19"))+
  scale_y_continuous(breaks =c(2,4,6,8,10,12,14))







FL1CPPLOT= data.frame(MAG = c(cptsFL1),dates =c(dates2[14:40]),numeric2 = c(numeric2[1:27]))

ggplot(data = FL1CPPLOT,aes(x = numeric2,y = MAG))+
  geom_point()+
  ggtitle('Number of Teams in the Top Cluster as Time Progresses- French Ligue 1')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 
  
  scale_x_continuous(breaks=c(5,10,15,20,25),
                     labels=c("98/99", "03/04","08/09","13/14","18/19"))+
  scale_y_continuous(breaks =c(2,4,6,8,10,12,14))

