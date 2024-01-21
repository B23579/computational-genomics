library(tidyverse)
library(caret)
library(ggplot2)
library(ggpubr)

df <-read_csv('csv/complexsate32.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 3:2",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p
########

df <-read_csv('csv/complexsate31.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 3:1",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p
########
df <-read_csv('csv/complexsate33.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 3:3",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

#####

df <-read_csv('csv/complexsate30.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 3:0",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p
########
########

df <-read_csv('csv/complexsate40.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 4:0",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p



######################3
df <-read_csv('csv/complexsate41.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 4:1",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

#######################
df <-read_csv('csv/complexsate42.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 4:2",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate43.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 4:3",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate44.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 4:4",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate50.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:0",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate51.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:1",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate52.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:2",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate53.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:3",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate54.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:4",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p


##############
df <-read_csv('csv/complexsate54.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 5:4",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate60.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 6:0",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate61.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 6:1",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate62.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 6:2",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate63.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 6:3",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate64.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 6:4",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p

##############
df <-read_csv('csv/complexsate66.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 6:6",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p


##############
df <-read_csv('csv/complexsate65.csv')

df$peakcomposition <- as.factor(df$peakcomposition)

df1 <- df %>%
  group_by(peakcomposition)%>%
  count()

df1$n <- round(df1$n /sum(df1$n) *100,digits = 1)

p1<-ggplot(df1, aes(x="", y=n, fill=peakcomposition)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5))+
  ggtitle(label = " complex stat 6:5",
  )+labs(fill= "peak composition")+
  theme_void() # remove background, grid, numeric labels
p <- p1+ theme(plot.title=element_text(size=10),legend.text=element_text(size=9))+ labs(fill= "peak composition")
p




