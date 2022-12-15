#CHANGE POINT PLOTTING

cptsEPL = c()
emptyEPL = c()
storeplots = c()
MagnitudeINTSEPL = c()


for (i in 1:41){
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


#LA LIGA
#remove first 12 seasons - 2 point wins
LALIGAPoints = LALIGAPoints[,13:41]


cptsSLL = c()
emptySLL = c()
MagnitudeINTSSLL = c()

for (i in 1:29){
  fit = cpt.mean(na.omit(LALIGAPoints[,i]))
  cptsSLL[i] = cpts(fit)
  ints = param.est(fit)$mean
  emptySLL[i] = ints[1]-ints[2]
  MagnitudeINTSSLL[i] = emptySLL[i]
  
  
  
  a = ggplot(data = data.frame(seq(1:length(na.omit(LALIGAPoints[,i]))),na.omit(LALIGAPoints[,i])),
             aes(x =seq(1:length(na.omit(LALIGAPoints[,i]))),y = na.omit(LALIGAPoints[,i])))+ 
    geom_line() + 
    
    ylim(12,95)+
    xlim(1,22)+
    geom_segment(x = 1, xend=length(LALIGAPoints[1:cptsSLL[i],1]) ,y =ints[1] ,yend = ints[1],color='red')+
    geom_segment(x = length(LALIGAPoints[1:cptsSLL[i],1]), xend=22 ,y =ints[2] ,yend = ints[2],color='red')+
    geom_vline(xintercept = cptsSLL[i],linetype='dashed',size = 1,color='blue')+
    ggtitle(dates[i+12], 'Spannish La Liga')+  
    labs(x='League Position',y = 'Points Total')
  #ggsave(a,filename=paste("SpannishLaLigaSeason",i,".png",sep=""))
   print(ggplot(data = data.frame(seq(1:length(na.omit(LALIGAPoints[,i]))),na.omit(LALIGAPoints[,i])),
             aes(x =seq(1:length(na.omit(LALIGAPoints[,i]))),y = na.omit(LALIGAPoints[,i])))+ 
    geom_line() + 
    
    ylim(12,95)+
    xlim(1,22)+
    geom_segment(x = 1, xend=length(LALIGAPoints[1:cptsSLL[i],1]) ,y =ints[1] ,yend = ints[1],color='red')+
    geom_segment(x = length(LALIGAPoints[1:cptsSLL[i],1]), xend=22 ,y =ints[2] ,yend = ints[2],color='red')+
    geom_vline(xintercept = cptsSLL[i],linetype='dashed',size = 1,color='blue')+
    ggtitle(dates[i+12],'Spannish La Liga')+  
    labs(x='League Position',y = 'Points Total'))
  
}



# BUNDESLIGA
#remove first 14 seasons - 2 point wins
BUNDESLIGAPoints = BUNDESLIGAPoints[,15:41]

cptsGB = c()
emptyGB = c()
MagnitudeINTSGB = c()
for (i in 1:27){
  fit = cpt.mean(na.omit(BUNDESLIGAPoints[,i]))
  cptsGB[i] = cpts(fit)
  ints = param.est(fit)$mean
  emptyGB[i] = ints[1]-ints[2]
  MagnitudeINTSGB[i] = emptyGB[i]
  
  
  
  a = ggplot(data = data.frame(seq(1:length(na.omit(BUNDESLIGAPoints[,i]))),na.omit(BUNDESLIGAPoints[,i])),
             aes(x =seq(1:length(na.omit(BUNDESLIGAPoints[,i]))),y = na.omit(BUNDESLIGAPoints[,i])))+ 
    geom_line() + 
    
    ylim(12,95)+
    xlim(1,18)+
    geom_segment(x = 1, xend=length(BUNDESLIGAPoints[1:cptsGB[i],1]) ,y =ints[1] ,yend = ints[1],color='red')+
    geom_segment(x = length(BUNDESLIGAPoints[1:cptsGB[i],1]), xend=18,y =ints[2] ,yend = ints[2],color='red')+
    geom_vline(xintercept = cptsGB[i],linetype='dashed',size = 1,color='blue')+
    ggtitle(dates[i+14],'German Bundesliga')+  
    labs(x='League Position',y = 'Points Total')
  #ggsave(a,filename=paste("GermanBUNDESLIGASeason",i,".png",sep=""))
  print(ggplot(data = data.frame(seq(1:length(na.omit(BUNDESLIGAPoints[,i]))),na.omit(BUNDESLIGAPoints[,i])),
               aes(x =seq(1:length(na.omit(BUNDESLIGAPoints[,i]))),y = na.omit(BUNDESLIGAPoints[,i])))+ 
          geom_line() + 
          
          ylim(12,95)+
          xlim(1,18)+
          geom_segment(x = 1, xend=length(BUNDESLIGAPoints[1:cptsGB[i],1]) ,y =ints[1] ,yend = ints[1],color='red')+
          geom_segment(x =length(BUNDESLIGAPoints[1:cptsGB[i],1]), xend=18 ,y =ints[2] ,yend = ints[2],color='red')+
          geom_vline(xintercept = cptsGB[i],linetype='dashed',size = 1,color='blue')+
          ggtitle(dates[i+14],'German Bundesliga')+  
          labs(x='League Position',y = 'Points Total'))
  
}






# Serie A
#remove first 13 seasons - 2 point wins
SERIEAPoints = SERIEAPoints[,14:41]



cptsISA = c()
emptyISA = c()
MagnitudeINTSISA =c()
for (i in 1:28){
  fit = cpt.mean(na.omit(SERIEAPoints[,i]))
  cptsISA[i] = cpts(fit)
  ints = param.est(fit)$mean
  emptyISA[i] = ints[1]-ints[2]
  MagnitudeINTSISA[i] = emptyISA[i]
  
  
  a = ggplot(data = data.frame(seq(1:length(na.omit(SERIEAPoints[,i]))),na.omit(SERIEAPoints[,i])),
             aes(x =seq(1:length(na.omit(SERIEAPoints[,i]))),y = na.omit(SERIEAPoints[,i])))+ 
    geom_line() + 
    
    ylim(9,105)+
    xlim(1,22)+
    geom_segment(x = 1, xend=length(SERIEAPoints[1:cptsISA[i],1]) ,y =ints[1] ,yend = ints[1],color='red')+
    geom_segment(x = length(SERIEAPoints[1:cptsISA[i],1]), xend=22 ,y =ints[2] ,yend = ints[2],color='red')+
    geom_vline(xintercept = cptsISA[i],linetype='dashed',size = 1,color='blue')+
    ggtitle(dates[i+13],'Italian Serie A')+  
    labs(x='League Position',y = 'Points Total')
  #ggsave(a,filename=paste("iTALIANSERIEASeason",i,".png",sep=""))
  print(ggplot(data = data.frame(seq(1:length(na.omit(SERIEAPoints[,i]))),na.omit(SERIEAPoints[,i])),
               aes(x =seq(1:length(na.omit(SERIEAPoints[,i]))),y = na.omit(SERIEAPoints[,i])))+ 
          geom_line() + 
          
          ylim(12,95)+
          xlim(1,22)+
          geom_segment(x = 1, xend=length(SERIEAPoints[1:cptsISA[i],1]) ,y =ints[1] ,yend = ints[1],color='red')+
          geom_segment(x =length(SERIEAPoints[1:cptsISA[i],1]), xend=22 ,y =ints[2] ,yend = ints[2],color='red')+
          geom_vline(xintercept = cptsISA[i],linetype='dashed',size = 1,color='blue')+
          ggtitle(dates[i+13],'Italian Serie A')+  
          labs(x='League Position',y = 'Points Total'))
  
}


# LIGUE 1
#remove first 13 seasons - 2 point wins
LIGUE1Points = LIGUE1Points[,14:40]



cptsFL1 = c()
emptyFL1 = c()
MagnitudeINTSFL1 = c()

for (i in 1:27){
  fit = cpt.mean(na.omit(LIGUE1Points[,i]))
  cptsFL1[i] = cpts(fit)
  ints = param.est(fit)$mean
  emptyFL1[i] = ints[1]-ints[2]
  MagnitudeINTSFL1[i] = emptyFL1[i]
  
  
  
  a = ggplot(data = data.frame(seq(1:length(na.omit(LIGUE1Points[,i]))),na.omit(LIGUE1Points[,i])),
             aes(x =seq(1:length(na.omit(LIGUE1Points[,i]))),y = na.omit(LIGUE1Points[,i])))+ 
    geom_line() + 
    
    ylim(9,102)+
    xlim(1,22)+
    geom_segment(x = 1, xend=length(LIGUE1Points[1:cptsFL1[i],1]) ,y =ints[1] ,yend = ints[1],color='red')+
    geom_segment(x = length(LIGUE1Points[1:cptsFL1[i],1]), xend=22 ,y =ints[2] ,yend = ints[2],color='red')+
    geom_vline(xintercept = cptsFL1[i],linetype='dashed',size = 1,color='blue')+
    ggtitle(dates2[i+13],'French Ligue 1')+  
    labs(x='League Position',y = 'Points Total')
  #ggsave(a,filename=paste("frenchLIGUE1Season",i,".png",sep=""))
  print(ggplot(data = data.frame(seq(1:length(na.omit(LIGUE1Points[,i]))),na.omit(LIGUE1Points[,i])),
               aes(x =seq(1:length(na.omit(LIGUE1Points[,i]))),y = na.omit(LIGUE1Points[,i])))+ 
          geom_line() + 
          
          ylim(8,105)+
          xlim(1,22)+
          geom_segment(x = 1, xend=length(LIGUE1Points[1:cptsFL1[i],1]) ,y =ints[1] ,yend = ints[1],color='red')+
          geom_segment(x = length(LIGUE1Points[1:cptsFL1[i],1]), xend=22,y =ints[2] ,yend = ints[2],color='red')+
          geom_vline(xintercept = cptsFL1[i],linetype='dashed',size = 1,color='blue')+
          ggtitle(dates2[i+13],'French Ligue 1')+  
          labs(x='League Position',y = 'Points Total'))
  
}



