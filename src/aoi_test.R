library(tidyverse)
library(readxl)


# 读取AOI信息
aoi <- read_excel("/Users/placenameday/R study/yajuan_phd/info/area1.xlsx") %>%
  mutate_at(vars(3:6), as.numeric)

# 读取注视点文件
fixation <- read_csv("/Users/placenameday/R study/yajuan_data/out/fix2/A002.fix.csv")

# 定义函数ac1, 在特定pic下与特定area下，根据坐标信息返回打标判断
aoi_cacu1 <- function(x, a, b) {
  aoi1 <- a %>% filter(area == x)
  
  fixation2 <- b %>%
    mutate(!!x :=case_when(between(axp, aoi1$X1, aoi1$X2)&between(ayp, aoi1$Y1, aoi1$Y3)~1))
}

ta <- aoi %>% filter(pic == "b1-1.jpg")

tf <- fixation %>% filter(pic == "b1-1.jpg")

tt2 <- reduce(map(ta$area, ~aoi_cacu1(.x, ta, tf)), full_join)


