####Heatmap####
library(tidyverse)
df<-read.csv( "Heatmap.csv",header=T)

df<- df %>%
  mutate(p = case_when(
    s > 0.05 ~ 1,
    s > 0.01 & s <= 0.05 ~ 2,
    s > 0.001 & s <= 0.01 ~ 3,
    s <= 0.001 ~ 4
  ))
# 查看新数据集
print(df)

data <- df %>% filter(Fertilizer == "CF")

glist <- data[1:11,]$ne
ggplot(data,
       aes(factor(fa,levels = c('MAP','MAT','Latitude','Longitude',
                                'N','P','K','Fertilizer','moisture','cpH','TOC','TN','CN',
                                'NH','NO','AP','AK'
       )),
       factor(ne,levels = rev(glist))))+
  geom_point(aes(size=`p`,
                 color=r))+
  labs(x=NULL,y=NULL,
       size='p',
       color='ne')+
  scale_color_gradient2(low = "#6699CC",high = "#CC0022",
                       limits=c(-1,1),breaks=c(-1.0,-0.5,0,0.5,1.0))+
  scale_size(range = c(3,8),breaks=c(4,3,2,1))+
  #scale_size(range = c(0, 11)) +
  theme_bw()+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(face = 'italic',size = 10),
        panel.border = element_rect(size = 1.5),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.4,'lines'),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=10),
        legend.key.size = unit(.3, "in"),
        legend.box.margin = margin(6,1,6,1,unit = 'cm'),
        panel.grid = element_line(size = 1)
  )+
  theme(plot.margin = unit(c(1,0,1,1),"cm"))

data <- df %>% filter(Fertilizer == "OF")
glist <- data[1:11,]$ne
ggplot(data,
       aes(factor(fa,levels = c('MAP','MAT','Latitude','Longitude','C',
                                'N','P','K','Fertilizer','moisture','cpH','TOC','TN','CN',
                                'NH','NO','AP','AK'
       )),
       factor(ne,levels = rev(glist))))+
  geom_point(aes(size=`p`,
                 color=r))+
  labs(x=NULL,y=NULL,
       size='p',
       color='ne')+
  scale_color_gradient2(low = "#6699CC",high = "#CC0022",
                        limits=c(-1,1),breaks=c(-1.0,-0.5,0,0.5,1.0))+
  scale_size(range = c(3,8),breaks=c(4,3,2,1))+
  #scale_size(range = c(0, 11)) +
  theme_bw()+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(face = 'italic',size = 10),
        panel.border = element_rect(size = 1.5),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.4,'lines'),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=10),
        legend.key.size = unit(.3, "in"),
        legend.box.margin = margin(6,1,6,1,unit = 'cm'),
        panel.grid = element_line(size = 1)
  )+
  theme(plot.margin = unit(c(1,0,1,0),"cm"))
