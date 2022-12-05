library(tidyverse)
library(vecsets)

letterindex <- c(letters, LETTERS)

match(, letterindex)

d <- read_lines("day_3/input_day_3.txt")

d

str_split_fixed()

sacks <- tibble(s = d) %>%
  mutate(len = nchar(s)) %>%
  rowwise() %>%
  mutate(h1 = str_sub(s, 1, len/2),
         h2 = str_sub(s, (len/2) + 1, -1L)) %>%
  ungroup() %>%
  mutate(h1_split = str_split(h1, ""),
         h2_split = str_split(h2, "")) %>%
  rowwise() %>%
  mutate(common = intersect(h1_split, h2_split)) %>%
  mutate(common_prio = match(common, letterindex)) %>%
  ungroup

sum(sacks$common_prio)
  

wide_sacks <- sacks %>%
  select(s) %>%
  mutate(rn = row_number()) %>%
  mutate(group_number = (rn + 2) %/% 3,
         elf_within_group = str_c("elf_", ((rn + 2) %% 3) + 1)) %>%
  select(-rn) %>%
  pivot_wider(id_cols = group_number, names_from = elf_within_group,
              values_from = s) %>%
  mutate(e1_split = str_split(elf_1, ""),
         e2_split = str_split(elf_2, ""),
         e3_split = str_split(elf_3, "")) %>%
  rowwise() %>%
  mutate(common_letter = intersect(intersect(e1_split, e2_split), e3_split)) %>%
  mutate(group_prio = match(common_letter, letterindex))

sum(wide_sacks$group_prio)
  
  
wide_sacks

intersect(wide_sacks$elf_1[1], wide_sacks$elf_2[1])




