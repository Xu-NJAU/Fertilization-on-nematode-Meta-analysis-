#### Whittaker图 ####
library(plotbiomes)
library(ggplot2)
# 加载数据
data <- read.csv("whittaker.csv")  # 替换为你的数据路径

# 使用 whittaker_base_plot 创建背景分布
P<-whittaker_base_plot() +
  # 添加温度和降水数据点
  geom_point(
    data = data, 
    aes(x = annual_temperature, y = annual_rainfall),  # 确保数据列名和实际数据文件一致
    size   = 2,
    shape  = 21,
    colour = "gray95", 
    fill   = "black",
    stroke = 1,
    alpha  = 0.7
  ) +
  theme_bw()
P
