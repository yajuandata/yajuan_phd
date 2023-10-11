library(tidyverse)
library(readxl)
library(data.table)
library(furrr)
plan(multisession(workers = 5))

dt_path <- "../yajuan_data/out/sacc2_AOI_type/A002.sacc.csv.aoi.csv"
loss_path <- "../yajuan_data/out/loss_rate_all/pic_list.csv"
words_path <- "info/count1.xlsx"

# 读取loss数据，字数数据，saccade数据
loss_info <- fread(loss_path)

dt_word <- read_excel(words_path) %>%
    select(pic, words_yuduan, words_question, words)

dt_a <- fread(dt_path) %>%
    mutate_at(vars(15:40), as.numeric) %>%
    anti_join(loss_info, by = c("participants", "pic"))

# 根据维度计算saccade_duration和saccade_ampl的平均值，每字平均注视次数，合并，删除中间数据
dt_a_s <- dt_a %>%
    group_by(type) %>%
    left_join(dt_word, by = "pic") %>%
    reframe(saccade_duration = mean(dur), saccade_ampl = mean(ampl), saccade_pv = mean(pv)) %>%
    mutate(participants = dt_a$participants[[1]], AOI = "overall")

dt_a_s2 <- dt_a %>%
    group_by(type, pic) %>%
    left_join(dt_word, by = "pic") %>%
    reframe(saccade_count = n(), words = mean(words)) %>%
    mutate(saccade_per_word = saccade_count / words) %>%
    select(-words, -saccade_count) %>%
    group_by(type) %>%
    reframe(saccade_per_word = mean(saccade_per_word)) %>%
    mutate(participants = dt_a$participants[[1]], AOI = "overall") %>%
    left_join(dt_a_s)

rm(dt_a_s)

# 根据刺激类型和AOI对数据进行变形，合并，删除中间数据
dt_yes <- dt_a %>%
    filter(stim %in% c("baseline", "target1", "target1_g")) %>%
    pivot_longer(15:40, names_to = "AOI", values_to = "hit") %>%
    filter(hit == 1) %>%
    select(1:11, type, AOI)

dt_17 <- dt_a %>%
    filter(stim %in% c("target2", "target2_g", "target3", "target3_g", "target4", "target4_g")) %>%
    pivot_longer(cols = (12:13 | 18:24), names_to = "AOI", values_to = "hit") %>%
    filter(hit == 1) %>%
    select(1:11, type, AOI)

dt_saccade <- dt_a %>%
    filter(stim %in% c("saccade", "saccade2", "saccade_g")) %>%
    pivot_longer(16:17, names_to = "AOI", values_to = "hit") %>%
    filter(hit == 1) %>%
    select(1:11, type, AOI)

dt_aoi <- bind_rows(dt_yes, dt_17, dt_saccade)

rm(dt_yes, dt_17, dt_saccade)

# 对字数信息进行变形
dt_word2 <- dt_word %>%
    select(-words) %>%
    pivot_longer(cols = 2:3, names_to = "AOI", values_to = "words") %>%
    mutate(AOI = str_replace_all(AOI, c("words_yuduan" = "yuduan", "words_question" = "question")))

# 根据AOI计算saccade_duration和saccade_ampl的平均值，每字平均注视次数，合并，删除中间数据
dt_aoi_s <- dt_aoi %>%
    group_by(type, AOI) %>%
    reframe(saccade_duration = mean(dur), saccade_ampl = mean(ampl)) %>%
    mutate(participants = dt_a$participants[[1]])

dt_aoi_s2 <- dt_aoi %>%
    group_by(type, pic, AOI) %>%
    reframe(saccade_count = n()) %>%
    left_join(dt_word2, by = c("pic", "AOI")) %>%
    mutate(words = case_when(is.na(words) ~ 1, .default = words)) %>%
    mutate(saccade_per_word = saccade_count / words) %>%
    select(-words, -saccade_count) %>%
    group_by(type, AOI) %>%
    reframe(saccade_per_word = mean(saccade_per_word)) %>%
    mutate(participants = dt_a$participants[[1]]) %>%
    left_join(dt_aoi_s)

rm(dt_aoi_s)

dt_saccade_summary <- bind_rows(dt_a_s2, dt_aoi_s2)

# 删除其余中间数据
rm(dt_word, dt_word2, dt_a, dt_aoi, loss_info, dt_a_s2, dt_aoi_s2)

# 返回数据
return(dt_saccade_summary)
