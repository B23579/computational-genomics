library(tidyverse)
library(caret)
library(ggplot2)
library(ggpubr)
df41<- data.frame(proportion=c(22.42,75.58),peak=c('(0.2,0.4,0.8)','(0.2,0.4,0.6,0.8)'))
df42<- data.frame(proportion=c(81.6,18.4),peak=c('(0.17,0.33,0.5,0.67)','(0.17,0.33,0.67)'))
df40<- data.frame(proportion=c(19.41,80.59),peak=c('(0.25,0.5,1)','(0.25,0.5,0.75,1)'))
df44<- data.frame(proportion=c(39.8,60.2),peak=c('(0.13,0.250,0.5)','(0.13,0.25,0.38,0.5)'))
df43<- data.frame(proportion=c(100),peak=c('(0.14,0.29,0.43,0.57)'))
df30<- data.frame(proportion=c(100),peak=c('(0.33,0.67,1)'))
df31<- data.frame(proportion=c(100),peak=c('(0.25,0.5,75)'))
df32<- data.frame(proportion=c(100),peak=c('(0.2,0.4,0.6)'))
df33<- data.frame(proportion=c(100),peak=c('(0.17,0.33,0.5)'))
df50<- data.frame(proportion=c(28.31,71.69),peak=c('(0.2,0.4,0.6,1)','(0.2,0.4,0.6,0.8,1)'))
df51<- data.frame(proportion=c(21.8,78.2),peak=c('(0.17,0.33,0.5,0.83)','(0.17,0.33,0.5,0.67,0.83)'))
df52<- data.frame(proportion=c(38.71,61.06,0.23),peak=c('(0.14,0.29,0.43,0.71)','(0.14,0.29,0.43,0.57,0.71)','(0.14,0.29,0.71)'))
df53<- data.frame(proportion=c(58.9,41.1),peak=c('(0.13,0.25,0.38,0.5,0.63)','(0.13,0.25,0.38,0.63)'))
df54<- data.frame(proportion=c(93.73,6.27),peak=c('(0.11,0.22,0.33,0.44,0.57)','(0.11,0.22,0.44,0.57)'))
df55<- data.frame(proportion=c(4.44,50,45.56),peak=c('(0.1,0.2,0.4,0.5)','(0.1,0.2,0.3,0.4,0.5)','(0.1,0.2,0.3,0.5)'))
#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p1<-ggplot(df41, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 4:1",
            )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p2<-ggplot(df42, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 4:2",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p2+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p3<-ggplot(df40, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 4:0",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p3+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


p4<-ggplot(df44, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 4:4",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p4+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


ggarrange(p3,p1, p2,p4,
          ncol = 2, nrow = 2)



#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p5<-ggplot(df30, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 3:0",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p5+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p6<-ggplot(df31, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 3:1",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p6+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p7<-ggplot(df32, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 3:2",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p7+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


p8<-ggplot(df33, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 3:3",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p8+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


ggarrange(p5,p6, p7,p8,
          ncol = 2, nrow = 2)




#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p9<-ggplot(df50, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:0",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p9+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p10<-ggplot(df51, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:1",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p10+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p11<-ggplot(df52, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:2",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p11+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


p12<-ggplot(df53, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:3",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p12+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")

p13<-ggplot(df43, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 4:3",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p13+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")

p11

ggarrange(p13,p9,p10,p12,
          ncol = 2, nrow = 2)



p14<-ggplot(df54, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:4",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p14+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")

p15<-ggplot(df55, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:5",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p15+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")



ggarrange(p11,p14,p15,
          ncol = 2, nrow = 2)










