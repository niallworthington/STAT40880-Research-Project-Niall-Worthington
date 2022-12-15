# EPL

EPLMAGPLOT= data.frame(MAG = c(MagnitudeINTSEPL),dates =c(dates),numeric = c(numeric))

ggplot(data = EPLMAGPLOT,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Points Difference between Cluster Means as Time Progresses - English Premier League')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","20/21"))
EPLMAGPLOT2= data.frame(MAG = c(MagnitudeINTSEPL),dates =c(dates),numeric = c(numeric))

EPLMAGPLOT2 = EPLMAGPLOT2[-c(39,40),]

ggplot(data = EPLMAGPLOT2,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Points Difference between Cluster Means as Time Progresses - English Premier League
          (COVID 19 AFFECTED SEASONS REMOVED')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')+
  annotate('text', x = 33, y = 18, label = 'Slope =')+
  annotate('text', x = 38, y = 18, label =coef(lm(EPLMAGPLOT2$MAG~EPLMAGPLOT2$numeric))[2])+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","20/21"))



qqplot(numeric,MagnitudeINTSEPL)
hist(MagnitudeINTSEPL)
shapiro.test(MagnitudeINTSEPL)
#p-value greater than 0.05 -> data is not signficiantly different from a normal distribution


#regression assumptions
#1-linearity yes
#2 - homoscedasticity
# 3 - independence yes
#normality - yes

#SLL


SLLMAGPLOT= data.frame(MAG = c(MagnitudeINTSSLL),dates =c(dates[13:41]),numeric = c(numeric[1:29]))

ggplot(data = SLLMAGPLOT,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Points Difference between Cluster Means as Time Progresses- Spannish La Liga')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')+
  scale_x_continuous(breaks=c(5,10,15,20,25),
                     labels=c("97/98", "02/03","07/08","12/13","17/18"))



# GB


GBMAGPLOT= data.frame(MAG = c(MagnitudeINTSGB),dates =c(dates[15:41]),numeric = c(numeric[1:27]))

ggplot(data = GBMAGPLOT,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Points Difference between Cluster Means as Time Progresses- German Bundesliga')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')+
  scale_x_continuous(breaks=c(5,10,15,20,25),
                     labels=c("99/00", "04/05","09/10","14/15","19/20"))




# ISA


ISAMAGPLOT= data.frame(MAG = c(MagnitudeINTSISA),dates =c(dates[14:41]),numeric = c(numeric[1:28]))

ggplot(data = ISAMAGPLOT,aes(x = numeric,y = MAG))+
  geom_point()+
  ggtitle('Points Difference between Cluster Means as Time Progresses- Italian Serie A')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')+
  scale_x_continuous(breaks=c(5,10,15,20,25),
                     labels=c("98/99", "03/04","08/09","13/14","18/19"))



#FL1


FL1MAGPLOT= data.frame(MAG = c(MagnitudeINTSFL1),dates =c(dates2[14:40]),numeric2 = c(numeric2[1:27]))

ggplot(data = FL1MAGPLOT,aes(x = numeric2,y = MAG))+
  geom_point()+
  ggtitle('Points Difference between Cluster Means as Time Progresses - French Ligue 1')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')+
  scale_x_continuous(breaks=c(5,10,15,20,25),
                     labels=c("98/99", "03/04","08/09","13/14","18/19"))




FL1MAGPLOT= data.frame(MAG = c(MagnitudeINTSFL1),dates =c(dates2[14:40]),numeric2 = c(numeric2[1:27]))

ggplot(data = FL1MAGPLOT,aes(x = numeric2,y = MAG))+
  geom_point()+
  ggtitle('Points Difference between Cluster Means as Time Progresses - French Ligue 1')+  
  labs(x='Year',y = 'Difference')+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')+
  annotate('text', x = 22, y = 15, label = 'Slope =')+
  annotate('text', x = 25.2, y = 15, label =  coef(lm(FL1MAGPLOT$MAG~FL1MAGPLOT$numeric2))[2])+
  scale_x_continuous(breaks=c(5,10,15,20,25),
                     labels=c("98/99", "03/04","08/09","13/14","18/19"))



