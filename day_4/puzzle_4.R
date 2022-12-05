library(tidyverse)

d_raw <- read_csv("day_4/input_4.txt", col_names = c("e1", "e2"))



d <- d_raw %>%
  separate(col = e1, into = c("e1_min", "e1_max"), sep = "-") %>%
  separate(col = e2, into = c("e2_min", "e2_max"), sep = "-")



d_proc <- d %>%
  rowwise() %>%
  mutate(e1_list = list(seq(from = e1_min, to = e1_max, by = 1))) %>%
  mutate(e2_list = list(seq(from = e2_min, to = e2_max, by = 1))) %>%
  mutate(overlap = list(intersect(e1_list, e2_list))) %>%
  mutate(len_ov = length(overlap)) %>%
  mutate(e1_within_e2 = length(overlap) == length(e1_list),
         e2_within_e1 = length(overlap) == length(e2_list))

d_proc %>% ungroup %>% summarize(full_overlap = sum(e1_within_e2) + sum(e2_within_e1))


d_proc %>% head(5)
d_proc[2, ]

d_proc[2, ]$e1_list
d_proc[2, ]$e2_list
d_proc[2, ]$overlap


d_proc %>%
  mutate(full_ov = e1_within_e2 | e2_within_e1) %>% ungroup() %>% summarize(sum(full_ov))
  select(e1_min:e2_max, full_ov) %>%
  View()
  
  
d_proc %>%
  filter(len_ov > 0) %>%
  nrow()
