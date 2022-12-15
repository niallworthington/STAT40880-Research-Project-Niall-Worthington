#DATA IMPORT

# Importing points totals for each league
BUNDESLIGAPoints = read.csv('BUNDESLIGA CSV.csv')
BUNDESLIGAPoints = rev(BUNDESLIGAPoints[1:18,])
LALIGAPoints = read.csv('LA LIGA CSV .csv')
LALIGAPoints = rev(LALIGAPoints[1:22,])
EPLPoints = read.csv('EPL CSV .csv')
EPLPoints = rev(EPLPoints[1:22,1:41])
LIGUE1Points = read.csv('LIGUE1 CSV.csv')
LIGUE1Points = rev(LIGUE1Points[1:22,1:41])
#remove 2020/21 season - incomplete due to covid 19 lockdown
LIGUE1Points = subset(LIGUE1Points,select = -c(39))
SERIEAPoints = read.csv('SERIEA CSV.csv')
SERIEAPoints = rev(SERIEAPoints[1:22,])





#PLOT OF HHI AND ENTROPY FOR INTRODUCTION TO SHOW THAT THERE IS A TREND OF SOME SORT OVER THE SEASONS
library(readxl)
#vector of dates 

EPL = rev(read_excel('~/Research Project/Points Totals.xlsx',1))
dates = colnames(EPL)
SLL = rev(read_excel('~/Research Project/Points Totals.xlsx',2))
GB = rev(read_excel('~/Research Project/Points Totals.xlsx',3))
ISA = rev(read_excel('~/Research Project/Points Totals.xlsx',4))
FL1 = rev(read_excel('~/Research Project/Points Totals.xlsx',5))
FL1 = subset(FL1,select = c(-39))
EPLHHI = unlist(EPL[48,])
SLLHHI = unlist(SLL[47,])
GBHHI = unlist(GB[40,])
ISAHHI = unlist(ISA[47,])
FL1HHI = unlist(FL1[45,])
EPLent = unlist(EPL [ 73,])
SLLent = unlist(SLL[72,])
GBent = unlist(GB[61,])
ISAent = unlist(ISA[70,])
FL1ent = unlist(FL1[68,])
numeric = seq(1,41,1)
EPLPLOT = data.frame(EPLHHI,EPLent,dates,numeric)
SLLPLOT = data.frame(SLLHHI,SLLent,dates,numeric)
GBPLOT = data.frame(GBHHI,GBent,dates,numeric)
ISAPLOT = data.frame(ISAHHI,ISAent,dates,numeric)
numeric2 = seq(1,40,1)
dates2 = colnames(FL1)
FL1PLOT = data.frame(FL1HHI,FL1ent,dates2,numeric2)

range(EPLHHI)
range(FL1HHI)
range(SLLHHI)
range(GBHHI)
range(ISAHHI)



range(EPLent)
range(FL1ent)
range(SLLent)
range(GBent)
range(ISAent)

library(ggplot2)
library(dplyr)
library(tidyr)
library(changepoint)
library(cowplot)

ggplot(EPLPLOT,aes(x=numeric,y=EPLHHI))+geom_point(size=3)+
  ggtitle("Herfindahl-Hirschman Index Per Season - English Premier League") +
  labs(x="Season", y="HHI")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","20/21")) +
  ylim(1.02,1.16)
qqplot(numeric,EPLHHI)
hist(EPLHHI)
shapiro.test(EPLHHI)
#p-value greater than 0.05 -> data is not signficiantly different from a normal distribution


#regression assumptions
#1-linearity yes
#2 - homoscedasticity
# 3 - independence yes
#normality - yes
par(mfrow=c(2,2))

LALIGAHHI =ggplot(SLLPLOT,aes(x=numeric,y=SLLHHI))+geom_point(size=3)+
  ggtitle("Herfindahl-Hirschman Index Per Season - Spanish La Liga") +
  labs(x="Season", y="HHI")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","20/21"))+
  ylim(1.02,1.16)+
  theme(plot.title = element_text(size=10))
BUNDESLIGAHHI = ggplot(GBPLOT,aes(x=numeric,y=GBHHI))+geom_point(size=3)+
  ggtitle("Herfindahl-Hirschman Index Per Season - German Bundesliga") +
  labs(x="Season", y="HHI")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","20/21"))+
  ylim(1.02,1.16)+
  theme(plot.title = element_text(size=10))


SERIEAHHI = ggplot(ISAPLOT,aes(x=numeric,y=ISAHHI))+geom_point(size=3)+
  ggtitle("Herfindahl-Hirschman Index Per Season - Italian Serie A") +
  labs(x="Season", y="HHI")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","20/21"))+
  ylim(1.02,1.16)+
  theme(plot.title = element_text(size=10))


LIGUE1HHI = ggplot(FL1PLOT,aes(x=numeric2,y=FL1HHI))+geom_point(size=3)+
  ggtitle("Herfindahl-Hirschman Index Per Season - French Ligue 1") +
  labs(x="Season", y="HHI")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","21/22"))+
  ylim(1.02,1.16)+
  theme(plot.title = element_text(size=10))

plot_grid(LALIGAHHI,BUNDESLIGAHHI,SERIEAHHI,LIGUE1HHI)

# ENTROPY PLOTS


ggplot(EPLPLOT,aes(x=numeric,y=EPLent))+geom_point(size=3)+
  ggtitle("Relative Entropy Per Season - English Premier League") +
  labs(x="Season", y="Relative Entropy")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","20/21"))+
  ylim(0.9725,1)

LALIGARE = ggplot(SLLPLOT,aes(x=numeric,y=SLLent))+geom_point(size=3)+
  ggtitle("Relative Entropy Per Season - Spanish La Liga") +
  labs(x="Season", y="Relative Entropy")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","20/21"))+
  ylim(0.9725,1)+
  theme(plot.title = element_text(size=10))

BUNDESLIGARE = ggplot(GBPLOT,aes(x=numeric,y=GBent))+geom_point(size=3)+
  ggtitle("Relative Entropy Per Season - German Bundesliga") +
  labs(x="Season", y="Relative Entropy")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","20/21"))+
  ylim(0.9725,1)+
  theme(plot.title = element_text(size=10))

SERIEARE = ggplot(ISAPLOT,aes(x=numeric,y=ISAent))+geom_point(size=3)+
  ggtitle("Relative Entropy Per Season - Italian Serie A") +
  labs(x="Season", y="Relative Entropy")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","20/21"))+
  ylim(0.9725,1)+
  theme(plot.title = element_text(size=10))

LIGUE1RE = ggplot(FL1PLOT,aes(x=numeric2,y=FL1ent))+geom_point(size=3)+
  ggtitle("Relative Entropy Per Season - French Ligue 1") +
  labs(x="Season", y="Relative Entropy")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40),
                     labels=c("85/86", "90/91","95/96","00/01","05/06",
                              "10/11","15/16","21/22"))+
  ylim(0.9725,1)+
  theme(plot.title = element_text(size=10))

plot_grid(LALIGARE,BUNDESLIGARE,SERIEARE,LIGUE1RE)

