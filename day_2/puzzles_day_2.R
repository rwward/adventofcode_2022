library(tidyverse)

d <- read_delim("day_2/input_day_2.txt", delim = " ", col_names = c("opp", "self")) %>%
  mutate(self_abc = case_when(self == "X" ~ "A",
                              self == "Y" ~ "B",
                              self == "Z" ~ "C"))


scored <- d %>% mutate(outcome_score = case_when(
  opp == self_abc ~ 3,
  opp == "A" & self_abc == "C" ~ 0, #opp rock, self scissors: lose
  opp == "C" & self_abc == "A" ~ 6, #opp scissors, self rock: win
  opp == "B" & self_abc == "C" ~ 6, #opp paper, self scissors: win
  opp == "C" & self_abc == "B" ~ 0, #opp scissors, self paper: lose
  opp == "A" & self_abc == "B" ~ 6, #opp rock, self paper: win
  opp == "B" & self_abc == "A" ~ 0, #opp paper, self rock: lose
)) %>%
  mutate(choice_score = case_when(
    self_abc == "A" ~ 1,
    self_abc == "B" ~ 2,
    self_abc == "C" ~ 3
  )) %>%
  mutate(total_score = outcome_score + choice_score)

sum(scored$total_score)


d %>% mutate(self_abc = case_when(
  self == "Y" ~ opp,
  self == "X" & opp == "A" ~ "C",
  self == "X" & opp == "B" ~ "A",
  self == "X" & opp == "C" ~ "B",
  self == "Z" & opp == "A" ~ "B",
  self == "Z" & opp == "B" ~ "C",
  self == "Z" & opp == "C" ~ "A"
))  %>% mutate(outcome_score = case_when(
  opp == self_abc ~ 3,
  opp == "A" & self_abc == "C" ~ 0, #opp rock, self scissors: lose
  opp == "C" & self_abc == "A" ~ 6, #opp scissors, self rock: win
  opp == "B" & self_abc == "C" ~ 6, #opp paper, self scissors: win
  opp == "C" & self_abc == "B" ~ 0, #opp scissors, self paper: lose
  opp == "A" & self_abc == "B" ~ 6, #opp rock, self paper: win
  opp == "B" & self_abc == "A" ~ 0, #opp paper, self rock: lose
)) %>%
  mutate(choice_score = case_when(
    self_abc == "A" ~ 1,
    self_abc == "B" ~ 2,
    self_abc == "C" ~ 3
  )) %>%
  mutate(total_score = outcome_score + choice_score) %>%
  summarize(sum(total_score))
