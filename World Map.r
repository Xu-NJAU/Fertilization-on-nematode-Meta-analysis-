#####World Map####
#setwd("E:/Meta-ss")
data <- read.csv("meta_update.csv")

subset_data <- data[, c("La", "Long", "RRTotal","n")]
library(dplyr)
result <- subset_data %>%
  group_by(La, Long, n) %>%
  summarise(RRTotal = mean(RRTotal, na.rm = TRUE))
colnames(result)

result$RRTotal[is.nan(result$RRTotal)] <- NA

library(ggplot2)
world<-map_data("world")
ggplot()+
  geom_polygon(data=world,aes(x=long,y=lat,group=group),
               fill="#dedede")+
  theme_bw()+
  scale_y_continuous(expand=expansion(mult=c(0,0)))+
  scale_x_continuous(expand=expansion(add=c(0,0)))->world.map

world.map
world.map+geom_point(data = result, 
                     aes(x = Long, y = La, 
                         color = RRTotal, 
                         fill = RRTotal,size = n),alpha=0.7
                     ) + 
  scale_fill_gradient2(low = "#429eb3",
                       mid = "#e9c922",
                       high = "#f22101",
                       midpoint = 0.5) +
  scale_color_gradient2(low = "#429eb3",
                        mid = "#e9c922",
                        high = "#f22101",
                        midpoint = 0.5)+
  scale_size(range = c(2,6))-> world.map.01
world.map.01

map<-read.csv("meta_update.csv")
p0 <- ggplot(map,aes(La))+#,fill=ecosystem
  geom_histogram(data=map,aes(La,..density..),position = 'identity',binwidth =4,alpha = 1) +#binwidth分组宽度
  #scale_fill_manual(values = c("#FF6666","#00CCCC"))+
  geom_density(data=map,aes(La,..density..),size=0.4)+
  #geom_density(position="fill")+
  #geom_density(data=map,aes(x=latitude,..density..),size=0.4)+
  labs(
    title = "Histogram",
    x = NULL,
    y = NULL
  )+theme_bw()+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#+theme_economist();p0
p0