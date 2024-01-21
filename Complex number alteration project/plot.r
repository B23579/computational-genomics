library(tidyverse)
library(caret)
library(ggplot2)
library(ggpubr)
library(formatR)
df64<- data.frame(proportion=c(45.6,38,2.7,13.2,0.5),peak=c('0.1 0.2 0.3 0.5 0.6','0.1 0.2 0.3 0.4 0.6','0.1 0.2 0.4 0.6','0.1 0.3 0.4 0.5 0.6','0.1 0.3 0.4 0.5 0.6'))
df63<- data.frame(proportion=c(41,34.3,17.9,6.8),peak=c('0.11 0.22 0.33 0.44 0.55 0.67','0.11 0.22 0.33 0.44 0.67 0.75','0.11 0.22 0.33 0.55 0.67','0.11 0.22 0.33 0.67'))

df62<- data.frame(proportion=c(70.5,12.6,13.5,3.4),peak=c('0.125 0.25 0.375 0.5 0.625 0.75','0.125 0.25 0.375 0.5 0.75','0.125 0.25 0.375 0.625','0.125 0.25 0.375 0.75'))
df61<- data.frame(proportion=c(64.2,26.2,4.9,4.7),peak=c('0.14 0.29 0.43 0.57 0.71 0.86','0.14 0.29 0.43 0.57 0.86','0.14 0.29 0.43 0.71 0.86','0.14 0.29 0.43 0.85'))
df60<- data.frame(proportion=c(65.1,24,6.2,4.8),peak=c('0.17 0.33 0.43 0.5 0.67 0.83 1','0.17 0.33 0.5 0.6 1','0.17 0.33 0.55 0.83 1','0.17 0.33 0.5 1'))

df50<- data.frame(proportion=c(70.4,26.1,3.5),peak=c('0.2 0.4 0.6 0.8 1','0.2 0.4 0.6 1', '0.2 0.4 0.8 1'))
df51<- data.frame(proportion=c(67.2,29.5,3.4),peak=c('0.17 0.33 0.67 0.83','0.17 0.33 0.5 0.83', '0.17 0.33 0.66 0.83'))
df52 <- data.frame(proportion=c(63.7,24.3,12),peak=c('0.14 0.29 0.43 0.57 0.71', '0.14 0.29 0.43 0.71', '0.14 0.28 0.57 0.71'))
df53<- data.frame(proportion=c(57.8,39.8,2.4),peak=c('0.125 0.25 0.375 0.625', ' 0.125 0.25 0.375 0.625', '0.25 0.5 0.75'))
df54<- data.frame(proportion=c(89.6, 8.7, 1.7),peak=c('0.11 0.22 0.33 0.56', '0.11 0.22 0.44 0.56', '0.11 0.33 0.44 0.56'))


df40<- data.frame(proportion=c(79.5,20.5),peak=c('0.25 0.5 0.75 1', '0.25 0.5 1'))
df41<- data.frame(proportion=c(73.5,26.5),peak=c('0.2 0.4 0.6 0.8', '0.2 0.4 0.8'))
df42<- data.frame(proportion=c(82.8, 17.2),peak=c('0.17 0.33 0.5 0.67', '0.17 0.33 0.67'))
df43<- data.frame(proportion=c(94.5,5.5),peak=c('0.14 0.29 0.43 0.57', '0.33 0.67'))
df44<- data.frame(proportion=c(70.5,29.5),peak=c('0.125 0.25 0.375 0.5', '0.125 0.25 0.5'))


# Basic piechart
p1<-ggplot(df60, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 6:0",
            )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p1

#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p2<-ggplot(df63, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 6:3",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p2+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p3<-ggplot(df62, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 6:2",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p3+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


p4<-ggplot(df61, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 6:1",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p4+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


ggarrange(p2,p3, p4,p1,
          ncol = 2, nrow = 2)


# Basic piechart
p1<-ggplot(df50, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex state 5:0",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p1

#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p2<-ggplot(df51, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex state 5:1",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p2+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p3<-ggplot(df52, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex state 5:2",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p3+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


p4<-ggplot(df53, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex state 5:3",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p4+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


p5<-ggplot(df54, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex state 5:4",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p5+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")

ggarrange(p5,p3, p2,p1,
          ncol = 2, nrow = 2)




# Basic piechart
p1<-ggplot(df40, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex state 4:0",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p1

#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p2<-ggplot(df41, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex state 4:1",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p2+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


#Proportion of trajectories that have the same peak composition for the
# Basic piechart
p3<-ggplot(df42, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex state 4:2",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p3+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


p4<-ggplot(df43, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex state 4:3",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p4+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")


p5<-ggplot(df44, aes(x="", y=proportion, fill=peak)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = proportion),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex state 4:4",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p5+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")

ggarrange(p1,p4, p3,p2,
          ncol = 2, nrow = 2)
